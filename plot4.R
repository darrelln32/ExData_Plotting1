plot4 <- function()
{
  hh1 <- read.table("hh_data.txt",header=TRUE,sep=";")    # read in data
  hh2 <- data.frame()
  
for (n in 1:nrow(hh1))
{
       new_day_time_hold <-paste(hh1[n,1],hh1[n,4],sep=" ")          ## extract the date
       new_day_time <- strptime(new_day_time_hold,format="%Y-%m-%d %H:%M:%S")
       hh_row <- cbind (new_day_time,hh1[n,])                         ##  merge the date with the data
       hh2 <- rbind(hh2,hh_row)  }

#####  steps to convert factor data type to numeric data type (for 3 variables)

   f <- factor(hh2[,6])                                         
   x <- as.numeric(levels(f))[f]         # column 6  -> global active power

   g <- factor(hh2[,8])
   x2 <- as.numeric(levels(g))[g]        # column 8 -> voltage
   
   h <- factor(hh2[,10])                    #  or the 3prd plot(for 3 variables)
   x3 <- as.numeric(levels(h))[h]            # column 10 -> Sub_metering_1 
   j <- factor(hh2[,11])
   x4 <- as.numeric(levels(j))[j]           # column 11 -> Sub_metering_2
   k <- factor(hh2[,12])
   x5 <- as.numeric(levels(k))[k]           # column 12 -> Sub_metering_3
   
   m <- factor(hh2[,7])                                         
   x6 <- as.numeric(levels(m))[m]         # column 7  -> voltage
   
### plot the required graphs   ######

par(mfrow=c(2,2))                       #  set up "panels" for graph

plot(hh2[,1],x,type="l",ylab="Global Active Power",xlab="")                   # graph 1
plot(hh2[,1],x2,type="l",ylab="Voltage",xlab="datetime")                      # graph 2

plot(hh2[,1],x3,type="l",ylab="Energy sub metering",xlab="")                   # graph 3
   points(hh2[,1],x4,col=2,type="l")
   points(hh2[,1],x5,col=4,type="l")
   legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),bty="n",col=c(1,2,4),lty=c(1,1,1))

plot(hh2[,1],x6,type="l",ylab="Global_reactive_power",xlab="datetime")         # graph 4

##################################################################

dev.copy(png,file="plot4.png",width=480,height=480)        # save plot to file  
dev.off()                                                   # close PNG device
    
}  


  
  