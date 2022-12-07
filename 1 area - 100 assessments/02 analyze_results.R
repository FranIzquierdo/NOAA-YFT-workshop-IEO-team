# Clean workspace:
rm(list = ls())

# Libraries:
require(r4ss)

# Set dir -------------------------------------------------------------------------

# Main directory (model):
type_data = '1area_25'
main_dir = paste0(getwd(),"/",type_data)
all_iter = list.files(path = main_dir)

# Data:
this_url = "https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/blob/main/data/YFT_1area_observations_1_100_ESS_25.RData"
mydata = Rfssa::load_github_data(github_data_url = this_url)

# Output dir:
outdir<-paste(getwd(), "/100 output", sep="")
dir.create(outdir)

# Loop -------------------------------------------------------------------------

# Loop to read results from all replicates

# Create data.frames for likelihood and convergence:
ldat=data.frame(matrix(0,ncol=3,nrow=1))
colnames(ldat)=c("Likelihood","Convergence","run")

# Create data.frames for yield:
ydat=data.frame(matrix(0,ncol=3,nrow=1))
colnames(ydat)=c("Tot. Catch","F report","run")

# Create data.frames for management quants:
ggdat=data.frame(matrix(0,ncol=16,nrow=1))
colnames(ggdat)=c("years","rec_low", "rec_value", "rec_upp", "ssb_low",
                             "ssb_val", "ssb_upp", "Bio",
                             "F_low", "F_val", "F_upp","catch","run","ssb_rel","f_rel","cat_est")

#seq_along(mydata)

for(j in seq_along(mydata)) { 
  
  dir = paste0(main_dir, '/', file.path( mydata[j])) 
  output = SS_output(dir = dir, covar = FALSE) 
  
  ## SS plot for each model:
  png(file=paste(dir,"/a_SSplot.png",sep=""), width = 1200, height = 800,
      pointsize=18)
  par(mfrow=c(3,3))
  SSplotCatch(output, subplots=10); title("Landings")
  SSplotSummaryF(output); title("F")
  SSplotBiology(output,subplots = 1)
  SSplotTimeseries(output, subplot = 14, minyr = 1982); title("Recruits")
  SSplotRecdevs(output, subplots=1);title("recdevs")
  SSplotBiology(output,subplots = 6); title("Mat")
  SSplotSelex(output, subplot = 1)
  SSplotTimeseries(output, subplot = 7); title("Biomass")
  SSplotTimeseries(output, subplot = 7, minyr = 1982); title("Biomass recent")
  dev.off()
  
  ## Quants --------------------------------------------------------------------
  dtyr <- output$endyr
  years     <- output$startyr:dtyr
  yearsfore <- output$startyr:(dtyr+output$N_forecast_yrs)
  nyears    <- length(years)
  ssbrecf <- output$derived_quants
  
  ## SSB -----------------------------------------------------------------------
  spb <- ssbrecf[substr(ssbrecf$Label,1,3)=="SSB",]
  ssb <- spb[3:(3+length(yearsfore)-1), 2:3]
  upperssb <- ssb[(1:length(years)),1] +  2 * ssb[(1:length(years)),2]
  lowerssb <- ssb[(1:length(years)),1] -  2 * ssb[(1:length(years)),2]
  ssb=ssb[(1:length(years)),1]
  
  ## Rec -----------------------------------------------------------------------
  recr <- ssbrecf[substr(ssbrecf$Label,1,4)=="Recr",]
  recr <- recr[(1:length(years))+2,2:3]
  upperrecr<- recr[,1] +  2 * recr[,2]
  lowerrecr <- recr[,1] -  2 * recr[,2]
  rec=recr[,1]
  
  ## F -------------------------------------------------------------------------
  fratenum <- ssbrecf[substr(ssbrecf$Label,1,2)=="F_",];
  fratenum <- fratenum[,2:3]
  upperf <- fratenum[1:nyears,1]+ 2 * fratenum[1:nyears,2]
  lowerf <- fratenum[1:nyears,1]- 2 * fratenum[1:nyears,2]
  f=fratenum[1:nyears,1]
  
  ## files ---------------------------------------------------------------------
  start <- r4ss::SS_readstarter(file = file.path(dir, "starter.ss"),
                                verbose = FALSE)
  ss3Dat <- r4ss::SS_readdat(file = file.path(dir, start$datfile),
                             verbose = FALSE)
  catch=ss3Dat$catch
  library(plyr)
  catch=ddply(catch, .(year, fleet), summarize, catch=sum(catch))
  catch_sum=catch[order(catch$fleet),]

  catch_total=ddply(catch_sum, .(year), summarize, catch=sum(catch))
  catches=catch_total[,2]
  
  library(dplyr)
  Bio=as_tibble(output$timeseries) %>%
    filter(Seas == 1) %>%
    select("Yr", "Era", "Seas", 'Bio_all')
  ind=which(Bio$Yr==years[1])
  ind2=which(Bio$Yr==years[length(years)])
  Bio=Bio$Bio_all[ind:ind2]
  
  ## Relative ------------------------------------------------------------------
  ssbMSY<-output$derived_quants$Value[output$derived_quants$Label=="SSB_MSY"]
  ssb_rel<-ssb/ssbMSY
  
  fMSY<-output$derived_quants$Value[output$derived_quants$Label=="annF_MSY"]
  f_rel<-f/fMSY
  
  ## Catch ---------------------------------------------------------------------
  library(tidyverse)
  fltnms <- setNames(output$definitions$Fleet_name,1:8)
  
  catch <- as_tibble(output$timeseries) %>% filter(Era == "TIME" ) %>% 
    select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(N)"), starts_with("dead(N)")) 
  names(catch) <- c('year', 'season', paste('LanObs', fltnms[1:7], sep = "_"), paste('LanEst', fltnms[1:7], sep = "_"),
                    paste('CatEst', fltnms[1:7], sep = "_"))
  aux1 <- catch %>% select(starts_with('CatEst')) - catch %>% select(starts_with('LanEst'))
  names(aux1) <- paste('DisEst', fltnms[1:7], sep = "_")
  
  catch <- catch %>% bind_cols(aux1) 
  catch <- catch %>% pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
    mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
    select('year', 'season', 'fleet', 'indicator', 'value')  

  Cat=subset(catch, catch$indicator=="CatEst")
  Cat$year=as.factor(Cat$year)
  cat=ddply(Cat, .(year), summarize,  number=sum(value))
  
  ## Yield ----------------------------------------------------------------------
  
  # Yield-per-recruit analysis
  equil_yield <- output[["equil_yield"]]
  
  ## Likelihood -----------------------------------------------------------------
  
  # Likelihood and convergence
   ldat1=cbind(output$likelihoods_used [1,1],output$maximum_gradient_component ,rep(j))
   colnames(ldat1)=c("Likelihood","Convergence","run")
  
   ## Save  ---------------------------------------------------------------------
  
  # Table for likelihood
  ldat=rbind(ldat,ldat1)
  colnames(ldat)=c("Likelihood","Convergence","run")
  
  # Table for yield 
  ydat1=cbind(equil_yield[["Tot_Catch"]],equil_yield[["F_report"]],rep(j))
  ydat1<-as.data.frame(ydat1)
  colnames(ydat1)<-c("Tot. Catch","F report", "run")
  ydat=rbind(ydat,ydat1)
  
  # Table for quants
  Table_assessment=cbind(years,lowerrecr,rec,upperrecr,lowerssb,ssb,upperssb,Bio,
                         lowerf,f,upperf,catches,rep(j,length(years)),ssb_rel,f_rel,cat[,2])
  colnames(Table_assessment)=c("years","rec_low", "rec_value", "rec_upp", "ssb_low",
                               "ssb_val", "ssb_upp", "Bio",
                               "F_low", "F_val", "F_upp","catch","run","ssb_rel","f_rel","cat_est")
  ggdat=rbind(ggdat,Table_assessment)
  
}

# Remove first rows:
ggdat=ggdat[-1,]
ggdat$run<-as.factor(ggdat$run)

ydat=ydat[-1,]
ydat$run<-as.factor(ydat$run)

ldat=ldat[-1,]
ldat$run<-as.factor(ldat$run)


# Plot results -----------------------------------------------------------------

# Measures ---------------------------------------------------------------------

## NC ---------------------------------------------------------------------------

# non convergent = num. non convergent runs/total runs

nc<-ldat %>% filter(Convergence>0.001)
NC<-length(nc$run)/length(ldat$run)
NC

## RMSE -------------------------------------------------------------------------

# RMSE and MAPE for catch input (observed) and catch estimated (expected)
# Calculate RMSE and MAPE for each run, and then do the mean and sd

# Create data.frames for likelihood and convergence:
mdat=data.frame(matrix(0,ncol=3,nrow=1))
colnames(mdat)=c("catch_RMSE","catch_MAPE", "run")

# Split dataset
sp<-split(ggdat, ggdat$run)

a=1:length(sp) # all runs
b=a[-nc$run] # convergent runs

for (i in a){
  
  # RMSE & MAPE: Catch
  catch_RMSE<-sqrt(mean((sp[[i]]$catch - sp[[i]]$cat_est)^2))
  catch_MAPE<-mean(abs((sp[[i]]$catch-sp[[i]]$cat_est)/sp[[i]]$catch)) * 100
  
  mdat1<-cbind(catch_RMSE,catch_MAPE,rep(i))
  colnames(mdat1)<-c("catch_RMSE","catch_MAPE","run")
  mdat=rbind(mdat,mdat1)
  
}

# Remove first row:
mdat=mdat[-1,]
mdat$run<-as.factor(mdat$run)
mdat

# Total
library(dplyr)
summarise_all(mdat[,1:2], .funs = funs(mean, sd))

## CV ---------------------------------------------------------------------------

l=length(unique(ggdat$years))
Yr=unique(ggdat$years)
data_cv=data.frame(matrix(0,ncol=4,nrow=1))
colnames(data_cv)<-c("year","cv_ssb","cv_F","cv_rec")

for (i in 1:l){
  aux=subset(ggdat,ggdat$years==Yr[i])
  
  cv_ssb=sd(aux$ssb_val) / mean(aux$ssb_val) * 100
  
  cv_F=sd(aux$F_val) / mean(aux$F_val) * 100
  
  cv_rec=sd(aux$rec_value) / mean(aux$rec_value) * 100
  
  data_cv1=cbind(Yr[i],cv_ssb,cv_F,cv_rec)
  colnames(data_cv1)<-c("year","cv_ssb","cv_F","cv_rec")
  
  data_cv=rbind(data_cv,data_cv1)
  
}

# Remove first row:
data_cv=data_cv[-1,]
data_cv

# Total
library(dplyr)
summarise_all(data_cv[,2:4], .funs = funs(mean, sd))
# todo lo que este por debajo de 100, magnitud sd igual a la de la media
# todo lo que este por debajo de 30, variabiliad baja

# Save table * -----------------------------------------------------------------

NC
err_catch=summarise_all(mdat[,1:2], .funs = funs(mean, sd))
cv_quants=summarise_all(data_cv[,2:4], .funs = funs(mean, sd))

measures<-cbind(NC,err_catch,cv_quants)

write.table(round(measures,5), paste(outdir,"/100 measures.txt",sep=""))


# Quantities ---------------------------------------------------------------------

## Likelihood ------------------------------------------------------------------
dev.off()
pdf(paste(outdir, "/Likelihood & convergence 100.pdf", sep=""), height = 6.5, width = 11)

ggplot(data=ldat, aes(y=Likelihood, x=as.numeric(run), col=factor(run)))+
  geom_hline(yintercept = mean(ldat$Likelihood), linetype="dashed", col="grey", size=1)+
  geom_point(size=2) + theme_light()+ 
  geom_col()+
  ylab("logLikelihood") + xlab("run") +
  theme(legend.position="none") + ggtitle("100 assessment - logLikelihood")

ggplot(data=ldat, aes(y=Convergence, x=as.numeric(run), col=factor(run)))+
  geom_hline(yintercept = 0.0001, linetype="dashed", col="grey", size=1)+
  geom_point(size=2) + theme_light()+ 
  ylab("Convergence") + xlab("run") + ylim(0, 10)+
  theme(legend.position="none") + ggtitle("100 assessment - Convergence")

dev.off()

## Remove non convergent models
ggdat<-ggdat %>% filter(run!=7 & 
                run!=8 &
                  run!=16 &
                  run!=21 &
                  run!=26 &
                  run!=28 &
                  run!=30 &
                  run!=32 &
                  run!=33 &
                  run!=36 &
                  run!=40 &
                  run!=41 &
                  run!=43 &
                  run!=44 &
                  run!=46 &
                  run!=48 &
                  run!=59 &
                  run!=60 &
                  run!=61 &
                  run!=67 &
                  run!=76 &
                  run!=77 &
                  run!=80 &
                  run!=82 &
                  run!=85 &
                  run!=94 &
                  run!=95 &
                  run!=99 &
                  run!=100)

#remove<-as.vector(nc$run)
#ggdat<-ggdat %>% filter(run!=c(remove))


## SSB --------------------------------------------------------------------------

pdf(paste(outdir, "/SSB 100.pdf", sep=""), height = 6.5, width = 11)

# ssb
library(ggplot2)
ssb <- ggplot(ggdat, aes(x=years, y=ssb_val,col=run)) + 
  geom_line(size=0.7) + 
  theme_light() + theme(legend.position="none") + 
  ylab("SSB") + xlab("Year") + ggtitle("100 assessments - SSB")

ssb

# ssb relative
ssb_rel <- ggplot(ggdat, aes(years, ssb_rel,col=run))+geom_line(size=0.7) + 
  geom_hline(yintercept=1, linetype='dashed')+
  theme_light() + theme(legend.position="none") + 
  ylab("SSB") + xlab("Year") + ggtitle("100 assessments - SSB relative")

ssb_rel

# summary stats
library(plyr)
ssbd<-ddply(ggdat, .(years), summarize,  mean=mean(ssb_val), sd=sd(ssb_val), 
            upp=mean(ssb_val)+sd(ssb_val), low=mean(ssb_val)-sd(ssb_val))

gdat<-split(ggdat, ggdat$run)

ssb_dat=data.frame(matrix(0,ncol=length(gdat),nrow=dim(ssbd)[1]))

for (i in 1:length(gdat)){
  ssb_dat[,i]=gdat[[i]]$ssb_val
}

ssbd<-cbind(ssbd, ssb_dat)

library(reshape2)
melted <- melt(ssbd, id.vars=c("years", "mean", "sd","upp","low"))

ggplot(melted, aes(years, mean)) + 
  geom_line(aes(y=value, group=variable), colour="gray60")+
  geom_line(size=0.9, colour="blue")+
  geom_ribbon(aes(x = years, ymin = low, ymax =upp),alpha = 0.1, fill="blue", linetype=1) + 
  theme_light()+theme(legend.position="none")+
  ggtitle("100 assessments - SSB stats") + ylab("SSB") + xlab("Year")

dev.off()

## F ----------------------------------------------------------------------------

pdf(paste(outdir, "/F 100.pdf", sep=""), height = 6.5, width = 11)

# F
f <- ggplot(ggdat, aes(years, F_val,col=run))+geom_line(size=0.7) + 
  theme_light() + theme(legend.position="none") + 
  ylab("F") + xlab("Year") + ggtitle("100 assessments - F")

f

# F relative
f_rel <- ggplot(ggdat, aes(years, f_rel,col=run))+geom_line(size=0.7) +
  geom_hline(yintercept=1, linetype='dashed')+
  theme_light() + theme(legend.position="none") + 
  ylab("F") + xlab("Year") + ggtitle("100 assessments - F relative")

f_rel

# summary stats
library(plyr)
fd<-ddply(ggdat, .(years), summarize,  mean=mean(F_val), sd=sd(F_val), 
            upp=mean(F_val)+sd(F_val), low=mean(F_val)-sd(F_val))

gdat<-split(ggdat, ggdat$run)

f_dat=data.frame(matrix(0,ncol=length(gdat),nrow=dim(fd)[1]))

for (i in 1:length(gdat)){
  f_dat[,i]=gdat[[i]]$F_val
}

fd<-cbind(fd, f_dat)

library(reshape2)
melted <- melt(fd, id.vars=c("years", "mean", "sd","upp","low"))

ggplot(melted, aes(years, mean)) + 
  geom_line(aes(y=value, group=variable), colour="gray60")+
  geom_line(size=0.9, colour="blue")+
  geom_ribbon(aes(x = years, ymin = low, ymax =upp),alpha = 0.1, fill="blue", linetype=1) + 
  theme_light()+theme(legend.position="none")+
  ggtitle("100 assessments - F stats") + ylab("F") + xlab("Year")

dev.off()

## Rec --------------------------------------------------------------------------

pdf(paste(outdir, "/Rec 100.pdf", sep=""), height = 6.5, width = 11)

# Rec 
rec<- ggplot(ggdat, aes(years, rec_value,col=run))+geom_line(size=0.7) +
  theme_light() + theme(legend.position="none") + 
  ylab("Recruitment") + xlab("Year") + ggtitle("100 assessments - Recruitment")

rec

# summary stats
library(plyr)
rd<-ddply(ggdat, .(years), summarize,  mean=mean(rec_value), sd=sd(rec_value), 
          upp=mean(rec_value)+sd(rec_value), low=mean(rec_value)-sd(rec_value))

gdat<-split(ggdat, ggdat$run)

r_dat=data.frame(matrix(0,ncol=length(gdat),nrow=dim(rd)[1]))

for (i in 1:length(gdat)){
  r_dat[,i]=gdat[[i]]$rec_value
}

rd<-cbind(rd, r_dat)

library(reshape2)
melted <- melt(rd, id.vars=c("years", "mean", "sd","upp","low"))

ggplot(melted, aes(years, mean)) + 
  geom_line(aes(y=value, group=variable), colour="gray60")+
  geom_line(size=0.9, colour="blue")+
  geom_ribbon(aes(x = years, ymin = low, ymax =upp),alpha = 0.1, fill="blue", linetype=1) + 
  theme_light()+theme(legend.position="none")+
  ggtitle("100 assessments - Reccruitment stats") + ylab("Recruitment") + xlab("Year")

dev.off()

## Catch -----------------------------------------------------------------------

pdf(paste(outdir, "/Catch 100.pdf", sep=""), height = 6.5, width = 11)

catch<- ggplot(ggdat, aes(years, catch,col=run))+geom_line(size=0.7) + theme_light()+
  theme(legend.position="none") + ggtitle("input catch")

catch

catch_est<- ggplot(ggdat, aes(years, cat_est,col=run))+geom_line(size=0.7) + theme_light()+
  theme(legend.position="none") + ggtitle("est catch")

catch_est

dev.off()

## Yield ------------------------------------------------------------------------

pdf(paste(outdir, "/Yield 100.pdf", sep=""), height = 6.5, width = 11)

ggplot(data=ydat, aes(y=`Tot. Catch`, x=`F report`, col=run))+
  geom_line(size=0.7) + theme_light()+ 
  ylab("Equilibrium yield") + xlab("F") +
  theme(legend.position="none") + ggtitle("100 assessment - Yield")

dev.off()



## Selex ------------------------------------------------------------------------

# This plot needs to read the 100 models
retroModels <- SSgetoutput(dirvec=file.path(main_dir, mydata))
retro_sum <- SSsummarize(retroModels)

## Read SS output
output <- SS_output(dir = dir, repfile = "Report.sso", 
                    compfile = "CompReport.sso",covarfile = "covar.sso", 
                    ncols = 200, forecast = forecast,warn = TRUE,covar = TRUE, 
                    checkcor = TRUE, cormax = 0.95, cormin = 0.01,
                    printhighcor = 10, printlowcor = 10,  verbose = TRUE,
                    printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE,
                    aalmaxbinrange=0)

output$nforecastyears <- ifelse(is.na(output$nforecastyears), 0, 
                                output$nforecastyears)
fltNms <- setNames(output$definitions[,'Fleet_name'], output$definitions[,1]) 
years     <- output$startyr:output$endyr
yearsfore <- output$startyr:(output$endyr+3) 
nyears    <- length(years)

# Retro in selectivities, we only compare last year one, just to check
library(dplyr)
library(tidyverse)

retro_sel <- as_tibble(subset(retro_sum[['sizesel']], Yr == 2015 & Fleet %in% c(1:9, 13:14))) %>% 
  pivot_longer(cols = 6:78,  names_to = 'lng', values_to = 'value') %>% 
  mutate(lng = as.numeric(lng), FleetNm = fltNms[Fleet])

pdf(paste0(outdir,"/Selex by fleet 100.pdf"), height = 6.5, width = 11)
ggplot(retro_sel, aes(as.numeric(lng), value, group = name, colour = name)) + 
  geom_line(size = 1) + 
  facet_wrap(~FleetNm, ncol = 3)+
  theme_light()+
  ggtitle("Selectivity by fleet - 100 assessment")
dev.off()
