# Clean workspace:
rm(list = ls())

# TODO: Add movement parameters

# Set working directory
mainDir = getwd()
setwd(mainDir)

# Libraries:
require(r4ss)
require(Rfssa)
require(doSNOW)
require(parallel)
source('get_initial_files.R')
nCoresRemain = 3 #5

# -------------------------------------------------------------------------
# Read data from Github:
this_url = "https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/blob/main/data/YFT_4area_observations_1_100_ESS_25.RData"
mydata = Rfssa::load_github_data(github_data_url = this_url)
type_data = '4area_25'
dir.create(path = type_data)

# -------------------------------------------------------------------------
# Read template:
template_dat = r4ss::SS_readdat_3.30(file = 'base/data.ss')
template_ctl = r4ss::SS_readctl_3.30(file = 'base/control.ss', datlist = 'base/data.ss_new')
template_start = r4ss::SS_readstarter(file = 'base/starter.ss')
template_fore = r4ss::SS_readforecast(file = 'base/forecast.ss')

# -------------------------------------------------------------------------
# BEGIN LOOP ONLY TO CREATE THE INPUT SS FILES:

for(i in seq_along(mydata)) {
  
  dir.create(path = file.path(type_data, mydata[i]))
  this_data = get(mydata[i])
  
  # read new CPUE -----------------------------------------------------------
  cpue_path<-paste0("D:/R (all)/Github/NOAA-YFT-experiment-IEO-team/CPUE standardization/output/besag st model/SSinput/")
  cpue_sim<-paste0("input_SS_4A_", i, ".RData")
  
  load(paste0(cpue_path, cpue_sim))
  
  ## 4 area, besag st
  ## year seas index obs se
  library(dplyr)
  CPUE_bst4<-input_SS_4A%>%select(year, seas,regionID, sumX50, sumSd)
  CPUE_bst4$regionID<-as.factor(CPUE_bst4$regionID)
  levels(CPUE_bst4$regionID) # rename regions 1 2 3 4 with fleet 17 18 19 20
  levels(CPUE_bst4$regionID)<-c(17,18,19,20) ## region ID change by FLEET NUMBER
  levels(CPUE_bst4$seas)<-c(1,4,7,10) ## season levels
  #CPUE_bst4$se<-(CPUE_bst4$meanSd/sqrt(100)) # standard error
  CPUE_bst4$se<-sqrt(log(1+(0.1^2))) # standard error for CV=0.2
  CPUE_bst4<-CPUE_bst4[,c(1,2,3,4,6)] # order
  colnames(CPUE_bst4)<-c("year","seas","index","obs","se") # rename cols
  CPUE_bst4$year<-as.numeric(as.character(CPUE_bst4$year))
  CPUE_bst4$seas<-as.numeric(as.character(CPUE_bst4$seas))
  CPUE_bst4$index<-as.numeric(as.character(CPUE_bst4$index))
  CPUE_bst4<-round(CPUE_bst4,4)
  
  # Ojo, hay que cambiar el error distribution de la flota por -1 (normal)
  
  
  # -------------------------------------------------------------------------
  # Create input files DAT y CTL
  
  outFiles = get_initial_files(cpue_bst=CPUE_bst4, sim_dat = this_data, dat = template_dat)
  
  # -------------------------------------------------------------------------
  # Write SS files:
  
  r4ss::SS_writedat_3.30(datlist = outFiles$dat, outfile = paste0(file.path(type_data, mydata[i]), '/data.ss'), overwrite = TRUE)
  r4ss::SS_writestarter(mylist = template_start, file = paste0(file.path(type_data, mydata[i]), '/starter.ss'), overwrite = TRUE)
  r4ss::SS_writeforecast(mylist = template_fore, file = paste0(file.path(type_data, mydata[i]), '/forecast.ss'), overwrite = TRUE)
  r4ss::SS_writectl_3.30(ctllist = template_ctl, outfile = paste0(file.path(type_data, mydata[i]), '/control.ss'), overwrite = TRUE)
  
  print(i)
  
}


# -------------------------------------------------------------------------
# Detect number of cores:
cores = detectCores()
cl = makeCluster(cores[1] - nCoresRemain)
registerDoSNOW(cl)


# -------------------------------------------------------------------------
# BEGIN LOOP to RUN ss IN PARALLEL:

foreach(ix = seq_along(mydata)) %dopar% {
  
  # -------------------------------------------------------------------------
  # Run SS3:
  dir = paste0(mainDir, '/', file.path(type_data, mydata[ix])) 
  command = paste("cd", dir, "& ss -nohess", sep = " ")
  ss = shell(cmd = command, intern = T, wait = T)
  
}

stopCluster(cl)
