library("ggplot2")
library("plyr")
library("reshape2")
library("xtable")

main <- read.csv("activity.csv")


main$fullTime <- strptime(paste(main$date,sprintf("%04d",main$interval),sep=" "), "%F %H%M")
main$hour <- as.POSIXct(strptime(sprintf("%04d",main$interval), "%H%M"))
main$dayName <- weekdays(main$fullTime)
day.names <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
main$dayFactor <- factor(main$dayName, levels=day.names, ordered=TRUE)
main$dayFlag <- factor(ifelse(as.integer(main$dayFactor)<=5,0,1),labels=c("weekday","weekend"))

chunkTimes <- strptime(c("00:00:00","06:00:00","12:00:00","18:00:00","23:59:59"), "%H:%M:%S")
main$chunk <- cut(main$hour, breaks=chunkTimes, labels=c("00to06","06to12","12to18","18to24"))


str(main)

## check NA and valid entries

daily.stats <- ddply(main, .(date), summarize, 
                     nNA = sum(is.na(steps)), 
                     nGood.All = sum(!is.na(steps)),
                     nGood.Not0 = sum(!is.na(steps) & steps>0), 
                     nSteps = sum(steps))

table(daily.stats$nNA)

## Plot show distribution of number of intervals

hist(daily.stats$nGood.Not0, xlim=c(-10,160), breaks=seq(-10,305,by=10), 
     col=c("red",rep("forestgreen",20)), cex.main=0.85, 
     main="Distribution of the daily number of 'good values' of 'steps' (!NA & >0)", 
     xlab="Number of 'good' intervals")

# Day split into four intervals

tmp.df <- ddply(main, .(chunk), summarize, 
                Ntot = length(steps[!is.na(steps)]), 
                Ndata.gt.0 = sum(!is.na(steps) & steps>0), 
                fraction = sprintf("%6.2f %%",sum(!is.na(steps) & steps>0)/length(steps[!is.na(steps)])*100.))


tmp.df <- ddply(main, .(chunk), summarize, 
                Ntot = length(steps[!is.na(steps)]), 
                Ndata.gt.0 = sum(!is.na(steps) & steps>0), 
                fraction = sprintf("%6.2f %%",sum(!is.na(steps) & steps>0)/length(steps[!is.na(steps)])*100.))


# Distribution by day chunk

byChunk <- subset(ddply(main, .(date, chunk), summarize, Ndata=sum(!is.na(steps) & steps>0)), Ndata>0)

ggplot(byChunk, aes(x=Ndata)) + theme_bw() + theme(legend.position="none") + 
  geom_histogram(aes(fill=chunk), colour="grey50", breaks=seq(-4,54,by=4), right=TRUE) +
  ggtitle("Distribution by 'day chunk' of the number of 'good values' of steps (!NA & >0)") + 
  xlab("Number of 'good' intervals") + 
  ylab("Number of days") + 
  facet_wrap( ~ chunk, nrow = 1)

# Mean Total number of Steps taken per Day

summary(daily.stats$nSteps)

# Distribution of Total Daily Steps
hist(daily.stats$nSteps, breaks = seq(0, 30000, by = 2500), col = "orange", 
     main = "Distribution of Total Daily Steps", 
     xlab = "Total Daily Steps", 
     ylab = "N_{days}")


## Average Daily Activity Pattern

main.m2 <- melt(main[,c("steps","date","hour","dayFactor","dayFlag")], na.rm = TRUE, 
                id = c("date","hour","dayFactor","dayFlag"), 
                measured = c("steps"), 
                value.name = "steps")

ts.ByHour <- ddply(main.m2, .(hour), summarize, Avrg=mean(steps), Sd=sd(steps), N=length(steps))
ts.ByHour$Avrg[ts.ByHour$N==0] <- NA
ts.ByHour$Sd[ts.ByHour$N<2] <- NA

gm.v1 <- ggplot(ts.ByHour, aes(hour, Avrg)) + theme_bw() + 
  geom_line(lty = 1, col = "red2") + 
  labs(title = "Activity (steps/5min) Averaged over all days") + 
  labs(x = "Time during the day") + 
  labs(y = "Mean number of steps") 

gm.v1 

gm.v1 + geom_errorbar(aes(ymin = ts.ByHour$Avrg-ts.ByHour$Sd/sqrt(ts.ByHour$N),
                          ymax = ts.ByHour$Avrg+ts.ByHour$Sd/sqrt(ts.ByHour$N)), 
                      color = "red2", alpha = "0.5") 

maxHr <- format(ts.ByHour[which.max(x=ts.ByHour$Avrg),"hour"], "%H:%M")
maxN  <- ts.ByHour[which.max(x=ts.ByHour$Avrg),"Avrg"]


gm.v1 + geom_errorbar(aes(ymin = ts.ByHour$Avrg-ts.ByHour$Sd/sqrt(ts.ByHour$N),
                          ymax = ts.ByHour$Avrg+ts.ByHour$Sd/sqrt(ts.ByHour$N)), 
                      color = "red2", alpha = "0.5") 

maxHr <- format(ts.ByHour[which.max(x=ts.ByHour$Avrg),"hour"], "%H:%M")
maxN  <- ts.ByHour[which.max(x=ts.ByHour$Avrg),"Avrg"]

ts.ByHour[which.max(x=ts.ByHour$Avrg),c("hour","Avrg")]

Total.NA <- sum(daily.stats$nNA)

BadDays <- as.character(daily.stats$date[daily.stats$nNA > 0])

Ntot <- tapply(main$steps, main$dayFactor, function(x){length(x)/288}, simplify=TRUE)
Nbad <- tapply(main$steps, main$dayFactor, function(x){length(x[is.na(x)])/288}, simplify=TRUE)

df <- data.frame(daily.stats[,c(1,2)], flag = ifelse(daily.stats$nNA == 0,1,0), 
                 dayF = factor(weekdays(as.Date(daily.stats$date)), 
                               levels = day.names, 
                               ordered = TRUE))
mat <- matrix(c(df$flag,-1,-1),nrow=7)

image(x=1:7, y=1:9, mat[,ncol(mat):1],
      col=c("grey80", "#DD4444", "#55CC55"), 
      xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
      main = "Matrix plot of 'good' and 'bad' days", cex.main = 0.9)

grid(nx = 7, ny = 9, col = "white")
axis( 1, at = 1:7, labels=c("M","Tu","W","Th","F","S","Su"), las=1, padj=-1, tcl=0 )
axis( 2, at = 1:9, labels=paste("wk",9:1,sep=" "), las=1, tcl=0, hadj=0.7 )

# summarizing by weekday and hour
ts.ByDay.ByHour <- ddply(main.m2, .(dayFactor, hour), summarize, 
                         Avrg=mean(steps), 
                         Sd=sd(steps), 
                         N=length(steps))

str(ts.ByDay.ByHour)
# 'data.frame': 2016 obs. of  5 variables:
#  $ dayFactor: Ord.factor w/ 7 levels "Monday"<"Tuesday"<..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ hour     : POSIXct, format: "2015-08-03 00:00:00" "2015-08-03 00:05:00" "2015-08-03 00:10:00" ...
#  $ Avrg     : num  1.43 0 0 0 0 ...
#  $ Sd       : num  3.78 0 0 0 0 ...
#  $ N        : int  7 7 7 7 7 7 7 7 7 7 ...


gm.ByDay.ByHour <- ggplot(ts.ByDay.ByHour, aes(hour, Avrg))
gm.ByDay.ByHour + theme_bw() + theme(legend.position = "none") +
  geom_line(aes(color = dayFactor), lty = 1) +
  labs(title = "activity averaged over time, by day") + 
  labs(x = "hour of the day") + 
  labs(y = "number of steps / 5 minutes") + 
  facet_wrap( ~ dayFactor, ncol = 1) 

main$dayGroup <- main$dayFactor
levels(main$dayGroup) <- c("G1","G1","G1","G2","G2","G3","G3")
main.m2 <- melt(main[, c("steps","date","hour","dayFactor","dayFlag","dayGroup")],
                id = c("date","hour","dayFactor","dayFlag","dayGroup"), 
                measured = c("steps"), value.name="steps", na.rm=TRUE)

ts.ByGroup.ByHour <- ddply(main.m2, .(dayGroup, hour), summarize, 
                           Avrg = mean(steps), 
                           Sd = sd(steps), 
                           N = length(steps))

ggplot(ts.ByGroup.ByHour, aes(hour, Avrg)) + theme_bw() + theme(legend.position="none") +
  geom_line(aes(color=dayGroup),lty=1) +
  labs(title="activity averaged over time, by type of day") + 
  labs(x="hour of the day") + labs(y="number of steps / 5 minutes") + 
  facet_wrap( ~ dayGroup, ncol=1) 

bd <- subset(main, date %in% BadDays, select=c("date","hour","dayGroup"))
bd.sequence <- as.character(ddply(bd, .(date, dayGroup), summarize, l = length(dayGroup))$dayGroup)
bd.sequence
# [1] "G1" "G1" "G2" "G3" "G2" "G3" "G1" "G2"

template.G1 <- as.vector(subset(ts.ByGroup.ByHour, dayGroup=="G1", select=c("Avrg"))$Avrg)
template.G2 <- as.vector(subset(ts.ByGroup.ByHour, dayGroup=="G2", select=c("Avrg"))$Avrg)
template.G3 <- as.vector(subset(ts.ByGroup.ByHour, dayGroup=="G3", select=c("Avrg"))$Avrg)

get.template <- function(x) { name <- paste("template", x, sep = "."); v <- get(name); return(v)}

newvec <- vector(mode="numeric", length=Total.NA)
for(i in 1:length(bd.sequence)) { 
  i1<-288*(i-1)+1; i2<-i*288; newvec[i1:i2]<-get.template(bd.sequence[i]); 
}

length(newvec)
# [1] 2304
summary(newvec[newvec>0])
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.1667   7.4000  29.3800  44.8700  67.4300 273.7000

main$stepsNew <- main$steps
main$stepsNew[is.na(main$steps)] <- newvec

daily.stats.new <- ddply(main, .(date), summarize, 
                         nNA = sum(is.na(stepsNew)), 
                         nGood.All = sum(!is.na(stepsNew)),
                         nGood.Not0 = sum(!is.na(stepsNew) & steps>0), 
                         nSteps = sum(stepsNew))

summary(daily.stats$nSteps)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      41    8841   10760   10770   13290   21190       8

summary(daily.stats.new$nSteps)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      41    9819   10570   10760   12810   21190

par(mfrow=c(1,2))
hist(daily.stats.new$nSteps, breaks=seq(0,30000,by=2500), col="orange", 
     xlim = c(0, 25000), 
     ylim = c(0, 25),
     main = "New Distribution of Total Daily Steps", 
     xlab = "Total Daily Steps", 
     ylab = "N_{days}")

hist(daily.stats$nSteps, breaks=seq(0,30000,by=2500), col="orange", 
     xlim=c(0,25000), ylim=c(0,25),
     main="Distribution of Total Daily Steps", xlab="Total Daily Steps", ylab="N_{days}")

par(mfrow=c(1,1))

ts.ByFlag.ByHour <- ddply(main.m2, .(dayFlag, hour), summarize, 
                          Avrg = mean(steps), 
                          Sd = sd(steps), 
                          N = length(steps))

gm.ByFlag.ByHour <- ggplot(ts.ByFlag.ByHour, aes(hour, Avrg))
gm.ByFlag.ByHour + theme_bw() + theme(legend.position = "none") +
  geom_line(aes(color = dayFlag), lty = 1) +
  labs(title="Activity averaged over time, by type of day") + 
  labs(x = "Hour of the day") + 
  labs(y = "Number of steps / 5 minutes") + 
  facet_wrap( ~ dayFlag, ncol = 1) 

sessionInfo()


