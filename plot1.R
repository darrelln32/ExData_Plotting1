plot1 <- function()
{
  hh1 <- read.table("household_power_consumption.txt",header=TRUE,sep=";")
  
  hh_row <- data.frame();
  hh2 <- data.frame();
  rec <- 0
  
####################################################################  
  
  for (n in 1:nrow(hh1))
  {
    hold_date <- as.Date(hh1[n,1],format='%d/%m/%Y')   ## extract the date
    
    if (identical(hold_date,as.Date("2007-02-01")))    ## build a new table based on the date
    {
      hold_day <- weekdays(hold_date)
      hh_row <- cbind (hold_date,hold_day,hh1[n,])
      hh2 <- rbind(hh2,hh_row) }
    
    if (identical(hold_date,as.Date("2007-02-02")))    ## build a new table based on the date
    {
     hold_day <- weekdays(hold_date)
     hh_row <- cbind (hold_date,hold_day,hh1[n,]) 
     hh2 <- rbind(hh2,hh_row) }   }
  
###################################################################

  hh2[,3] <- NULL      # eliminate old date column
  hh2[,3] <- NULL      # eliminate old time column

   f <- factor(hh2[,3])              #  step to convert factor data type to numeric data type
   x <- as.numeric(levels(f))[f]     
 
   hist(x,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")  # plot required data

   dev.copy(png,file="plot1.png",width=480,height=480)     # save plot to file 
   dev.off()                                               # close PNG device
    
  }  


  
  