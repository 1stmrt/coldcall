library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)


cidata <- read.csv("carInsurance_train.csv", header=T)

summary(cidata)

summary(cidata$Job)
summary(cidata$CallStart)
summary(cidata$CallEnd)

cidata$DaysPassed

starttime <- str_split(string = cidata$CallStart, pattern=":")
starttime <- data.frame(Reduce(rbind, starttime)) 
starttime$X1 <- as.numeric(as.character(starttime$X1))
starttime$X2 <- as.numeric(as.character(starttime$X2))
starttime$X3 <- as.numeric(as.character(starttime$X3))
startfactor <- c("starthour", "startminute", "startsecond")
names(starttime) <- startfactor
row.names(starttime) <- NULL
starttime


endtime <- str_split(string = cidata$CallEnd, pattern=":")
endtime <- data.frame(Reduce(rbind, endtime)) 
endtime$X1 <- as.numeric(as.character(endtime$X1))
endtime$X2 <- as.numeric(as.character(endtime$X2))
endtime$X3 <- as.numeric(as.character(endtime$X3))
endfactor <- c("endhour", "endminute", "endsecond")
names(endtime) <- endfactor
row.names(endtime) <- NULL
endtime

dif <- endtime - starttime
dif <- (dif$endhour * 3600) + (dif$endminute * 60) + (dif$endsecond)
summary(dif)

cidata <- cbind(cidata, starttime, endtime)
cidata$dif <- dif
cidata <- cidata[,-c(17,18)]

summary(cidata$Age)
ggplot(cidata, aes(y=cidata$Age, x=1)) + geom_violin()

cidata<- transform(cidata,
                   age_32 = ifelse(cidata$Age<=32, 1, 0),
                   age32_42= ifelse(cidata$Age>32 & cidata$Age<=42 , 1, 0),
                   age42_49 = ifelse(cidata$Age>42 & cidata$Age<=49, 1, 0),
                   age49_ = ifelse(cidata$Age>49, 1, 0))

cidata <- cidata[,-2]

## Missing Value 처리하기
sum(is.na(cidata$Job)) ## 19개는 그냥 버림 귀찮음
cidata <- cidata[!c(is.na(cidata$Job)),] ## 4000 -> 3981

sum(is.na(cidata$Marital))
sum(is.na(cidata$Education))


cidata$Educationf <- addNA(cidata$Education)
levels(cidata$Educationf) <- c(levels(cidata$Education), 'idk')

cidata$Educationf
cidata$Education <- NULL

##