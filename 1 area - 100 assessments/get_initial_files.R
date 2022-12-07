
get_initial_files = function(cpue_bst,sim_dat, dat) {

  # INIT INFO:
  
  OutDataFile = dat
  myDataFile = sim_dat
  
  # DATA FILE :
  
  OutDataFile$N_areas = myDataFile$N_areas
  OutDataFile$Nfleets = myDataFile$Nfleet + myDataFile$Nsurveys
  OutDataFile$fleetinfo = data.frame(type = c(rep(1, times = myDataFile$Nfleet), rep(3, times = myDataFile$Nsurveys)), 
                                     surveytiming = c(rep(-1, times = myDataFile$Nfleet), rep(5.5, times = myDataFile$Nsurveys)),
                                     area = as.vector(as.matrix(myDataFile$fleetinfo1[2,])),
                                     units = 2, # llcpue units are ignored
                                     need_catch_mult = 0,
                                     fleetname = myDataFile$fleetnames)
  OutDataFile$fleetnames = myDataFile$fleetnames
  OutDataFile$surveytiming = c(rep(-1, times = myDataFile$Nfleet), rep(5.5, times = myDataFile$Nsurveys))
  OutDataFile$units_of_catch = rep(2, times = OutDataFile$Nfleets)
  OutDataFile$areas = as.vector(as.matrix(myDataFile$fleetinfo1[2,]))
  
  # Catch data:
  # pseudo-years to years transformation
  aux=OutDataFile$catch
  cat=myDataFile$catch
  l=nrow(cat)
  myDataFile$se_log_catch
  
  a=length(cat$year);a1=as.numeric(cat$year[a]);a2=as.numeric(cat$year[1])
  a==(a1-a2+1)
  
  a1=cbind(sort(rep(OutDataFile$styr:OutDataFile$endyr,4)),rep(1:4,l/4),rep(1,l),cat[,1],rep(0.01,l))
  a2=cbind(sort(rep(OutDataFile$styr:OutDataFile$endyr,4)),rep(1:4,l/4),rep(2,l),cat[,2],rep(0.01,l))
  a3=cbind(sort(rep(OutDataFile$styr:OutDataFile$endyr,4)),rep(1:4,l/4),rep(3,l),cat[,3],rep(0.01,l))
  a4=cbind(sort(rep(OutDataFile$styr:OutDataFile$endyr,4)),rep(1:4,l/4),rep(4,l),cat[,4],rep(0.01,l))
  a5=cbind(sort(rep(OutDataFile$styr:OutDataFile$endyr,4)),rep(1:4,l/4),rep(5,l),cat[,5],rep(0.01,l))
  a6=cbind(sort(rep(OutDataFile$styr:OutDataFile$endyr,4)),rep(1:4,l/4),rep(6,l),cat[,6],rep(0.01,l))
  a7=cbind(sort(rep(OutDataFile$styr:OutDataFile$endyr,4)),rep(1:4,l/4),rep(7,l),cat[,7],rep(0.01,l))
  
  a_f=rbind(a1,a2,a3,a4,a5,a6,a7)
  (a_f=as.data.frame(a_f))
  colnames(a_f)=c("year", 
                  "season"  ,  
                  "fleet"  , 
                  "catch" ,
                  "catch_se")
  
  # Because we need the format (we need to take it in this way)
  nr=dim(a_f)[1]
  aux=rbind(aux,aux,aux,aux)
  aux=aux[1:nr,]
  
  aux[,1]=a_f[,1]
  aux[,2]=a_f[,2]
  aux[,3]=a_f[,3]
  aux[,4]=a_f[,4]
  aux[,5]=a_f[,5]
  
  OutDataFile$catch = aux
  
  #CPUE data:
  OutDataFile$CPUEinfo = data.frame(Fleet = myDataFile$CPUEinfo$Fleet, Units = 0, Errtype = myDataFile$CPUEinfo$Errtype,
                                    SD_Report = 0)
  rownames(OutDataFile$CPUEinfo) = myDataFile$fleetnames
  
  # pseudo-years to years
  OutDataFile$CPUE = cpue_bst
  
  #Length composition data:
  OutDataFile$len_info = data.frame(mintailcomp = -0.001, addtocomp = myDataFile$add_to_comp, 
                                    combine_M_F = 0, CompressBins = 0, CompError = 1, ParmSelect = 1:8,
                                    minsamplesize = rep(0.01, times = OutDataFile$Nfleets)) # dirichlet error
  rownames(OutDataFile$len_info) = OutDataFile$fleetnames
  
  # pseudo-years to years
  l_time=length(1952:2015)
  time=data.frame(sort(rep(1952:2015,4)), rep(c(1,4,7,10),l_time))
  b=myDataFile$lencomp
  lb=dim(b)[1]
  for (i in 1:lb){
    b[i,1:2]=time[b$Yr[i],]
  }
  OutDataFile$lencomp = b
  OutDataFile$lencomp$Nsamp = 25 # ESS 25 
  OutDataFile$lencomp$Yr = OutDataFile$lencomp$Yr
  #OutDataFile$lencomp$Seas = 7 # should be 7 or 1?
  OutDataFile$Nfleet = myDataFile$Nfleet
  OutDataFile$Nsurveys = myDataFile$Nsurveys
  OutDataFile$fleetinfo1 = t(OutDataFile$fleetinfo[,c('surveytiming', 'area', 'type')])
  OutDataFile$fleetinfo2 = t(data.frame(units = OutDataFile$units_of_catch, need_catch_mult = 0))
  OutDataFile$max_combined_lbin = rep(myDataFile$max_combined_lbin, times = OutDataFile$Nfleets)
  

    # Tag data:
    OutDataFile$do_tags = myDataFile$do_tags
    OutDataFile$N_tag_groups = myDataFile$N_tag_groups
    OutDataFile$N_recap_events = myDataFile$N_recap_events
    OutDataFile$mixing_latency_period = 3 #myDataFile$mixing_latency_period
    OutDataFile$max_periods = 4
    # Pseudo-years to years
    l_time=length(1952:2015)
    time=data.frame(sort(rep(1952:2015,4)), rep(1:4,l_time))
    age_new=data.frame(1:28,sort(c(0,0,0,rep(1:6,4),7)));colnames(age_new)=c("pseudo","final")
    aux=myDataFile$tag_releases
    laux=dim(aux)[1]
    for (i in 1:laux){
      aux[i,3:4]=time[aux$yr[i],]
      aux[i,5]=time[aux$tfill[i],1]
      aux[i,7]=age_new[aux$age[i],2]
    }
    
    
    OutDataFile$tag_releases = aux
    
    aux=myDataFile$tag_recaps
    laux=dim(aux)[1]
    for (i in 1:laux){
      aux[i,2:3]=time[aux$yr[i],]
    }
    aux$fleet=6 # ps = 6
    OutDataFile$tag_recaps = aux
    
  # OUT FILES

  myFiles = list(dat = OutDataFile)
  
  return(myFiles)

}
