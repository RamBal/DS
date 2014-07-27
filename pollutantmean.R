pollutantmean<-function(directory,pollutant,id=1:332)
{
  idm<-0
  tm<-0
  idc<-0
  tc<-0
  for (i in 1:length(id)) {                                            ## loop the following code 
    if (id[i] < 10) file<-paste('00',as.character(id[i]),sep='')       ## prefix '00' for single digit IDs
    else if (id[i] < 100) file<-paste('0',as.character(id[i]),sep='')  ## prefix '0' for double digit IDs
    else file<-paste(as.character(id[i]),sep='')
    data<-read.csv(paste(directory,'/',file,'.csv',sep=''))            ## read file
        
    good_sulfate<-complete.cases(data$sulfate)                         ## eliminate NA data
    good_nitrate<-complete.cases(data$nitrate)
    

    if (pollutant == 'sulfate') {                                      ## select column for mean
       m<-m+sum(data$sulfate[good_sulfate]) 
       idc<-sum(good_sulfate)
       tc<-tc+sum(good_sulfate)
    }
    else {
      idm<-sum(data$nitrate[good_nitrate])
      tm<-tm+sum(data$nitrate[good_nitrate])
      idc<-sum(good_nitrate)
      tc<-tc+sum(good_nitrate)
    }
#    print(paste(i,idm,idc,idm/idc,sep='/   '))
  }
##  print(m)
##  print(c)
##  print(paste(tm,tc,tm/tc,sep='/   '))
tm/tc
}