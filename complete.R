complete<-function(directory,id)
{ 
  df<-matrix(0,length(id),2)                                           ## create a data frame of required size
  dimnames(df)<-list(1:length(id),c('ids','nobs'))
  for (i in 1:length(id)) {                                            ## loop the following code 
    if (id[i] < 10) file<-paste('00',as.character(id[i]),sep='')       ## prefix '00' for single digit IDs
    else if (id[i] < 100) file<-paste('0',as.character(id[i]),sep='')  ## prefix '0' for double digit IDs
    else file<-paste(as.character(id[i]),sep='')

    data<-read.csv(paste(directory,'/',file,'.csv',sep=''))            ## read file
    good<-complete.cases(data)                         ## eliminate NA data

    df[i,1]<-id[i]
    df[i,2]<-sum(good)
  }
  df<-as.data.frame(df)
#  print(df)
}