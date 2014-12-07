plot2 <- function()
{
  hh1 <- read.table("hh_data.txt",header=TRUE,sep=";")
  hh2 <- data.frame()
  
for (n in 1:nrow(hh1))
{
       new_day_time_hold <-paste(hh1[n,1],hh1[n,4],sep=" ")          ## extract the date
       new_day_time <- strptime(new_day_time_hold,format="%Y-%m-%d %H:%M:%S")
       hh_row <- cbind (new_day_time,hh1[n,])                         ##  merge the date with the data
       hh2 <- rbind(hh2,hh_row)  }
       
###################################################################

  f <- factor(hh2[,6])              #  step to convert factor data type to numeric data type
  x <- as.numeric(levels(f))[f]

  plot(hh2[,1],x,type="l",ylab="Global Active Power (kilowatts)",xlab="")   # plot required data

##################################################################

 dev.copy(png,file="plot2.png",width=480,height=480)     # save plot to file
 dev.off()                                               # close PNG device
    
}  


  
  