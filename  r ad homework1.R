ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))
  
ds$start_new <- as.POSIXlt(paste("2016-06-02", ds$start))
ds$end_new <- as.POSIXlt(paste("2016-06-02", ds$end))  

ds

time_interval <- function(time1_start,time1_end,time2_start,time2_end){
  if((difftime(time1_start,time2_start)>=0 && difftime(time1_start,time2_end)<0)||
     (difftime(time1_start,time2_start)<=0 && difftime(time1_end,time2_start)>0))
    return(1)
  else(return(0))
}  

find_interval<-function(data,k){
  interval<-c(k)
  for(i in 1:length(ds$id)){
    if(data[i,"anest"]==data[k,"anest"]){
      m<-0
      for(j in 1:length(interval)){
        if(time_interval(data[i,"start_new"],data[i,"end_new"],data[interval[j],"start_new"],data[interval[j],"end_new"])==0)
          m<-m+1}
      if(m==0 && i!=k)
      interval<-c(interval,i)
    }
  }
  return(ds[sort(interval),"id"])
}




for(i in 1:length(ds$id)){
  ds[i,"length"]<-length(find_interval(ds,i))
}
