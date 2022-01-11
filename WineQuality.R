setwd("C:/Users/User/Desktop/Uni/1.Vorlesungen/3.Semester_WS_2021-2022/02.Projekte/03.Assistenzsysteme/Projektarbeit_Wein_Qualität")

WeinR <- read.csv("winequality-red.csv", header=TRUE, sep=";",fill=TRUE)
WeinW <- read.csv("winequality-white.csv", header=TRUE, sep=";",fill=TRUE)

x <- WeinR[,"quality"]
y <- WeinR[,"alcohol"]

#mean(x)
#median(x)
#summary(x)
#max(x)
#min(x)
#sd(x)
#quantile(x,0.3)
#IQR(x)
#quantile(x,0.75)-quantile(x,0.25)
#hist(x)

print(model)

a <- data.frame(x=c(5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10))
model <- lm(x ~ y)
result <- predict(model,newdata = a, interval = 'confidence')
plot(y,x,col = "blue",main ="Qualität & Alcohol Regression", abline(lm(x ~ y)),cex=1.3,pch=16,xlab="Alcohol gehalt",ylab="Quality(0-10)")

