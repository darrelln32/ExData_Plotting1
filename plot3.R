plot3 <- function()
{
  hh1 <- read.table("hh_data.txt",header=TRUE,sep=";")    # read in data
  hh2 <- data.frame()
  
for (n in 1:nrow(hh1))
{
       new_day_time_hold <-paste(hh1[n,1],hh1[n,4],sep=" ")          ## extract the date
       new_day_time <- strptime(new_day_time_hold,format="%Y-%m-%d %H:%M:%S")
       hh_row <- cbind (new_day_time,hh1[n,])                         ##  merge the date with the data
       hh2 <- rbind(hh2,hh_row)  }


       
###################################################################

   f <- factor(hh2[,10])              #  steps to convert factor data type to numeric data type (for 3 variables)
   x <- as.numeric(levels(f))[f]
   g <- factor(hh2[,11])
   x2 <- as.numeric(levels(g))[g]
   h <- factor(hh2[,12])
   x3 <- as.numeric(levels(h))[h]

   plot(hh2[,1],x,type="l",ylab="Energy sub metering",xlab="")     # plot required data
   points(hh2[,1],x2,col=2,type="l")
   points(hh2[,1],x3,col=4,type="l")
   legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c(1,2,4),lty=c(1,1,1))

##################################################################

  dev.copy(png,file="plot3.png",width=480,height=480)     # save plot to file
  dev.off()                                                # close PNG device
    
}  


  
  