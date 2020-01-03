#Run "Quick and Dirty Extraction First for applicable data sets"

#Documentation Linear http://r-statistics.co/Linear-Regression.html
#Documentation Logistic http://r-statistics.co/Logistic-Regression-With-R.html
#Documentation Stat Tests http://r-statistics.co/Statistical-Tests-in-R.html

#Starting and Ending Spline to generate variable
oldw <- getOption("warn")
options(warn = -1)
FT_Time <- as.double((z[2]-1988+4))

# Linear Model 
linearmod <-lm(as.double(ALB[1,4:FT_Time]) ~ as.double(ALB[4,4:FT_Time]), data=ALB)
summary(linearmod)
#Record R2 from summary tab
R2 =data.frame(0.9502)

#Logarithmic Model (Single Log)
l_x <- (as.double(ALB[4,4:FT_Time ]))
logmodel <- lm(log(as.double(ALB[1,4:FT_Time])) ~ l_x, data=ALB)
summary(logmodel)
#Record R2
R2 =cbind(R2,0.9843)

#Logarithmic Model (Log-Log Exponential)
lg_x <- log(as.double(ALB[4,4:FT_Time ]))
loglogmodel <- lm(log(as.double(ALB[1,4:FT_Time])) ~ lg_x, data=ALB)
summary(logmodel)
#Record R2
R2 =cbind(R2,0.9842)
#Logistic Model
#??? (Doesnt really work since there isnt a strong binary operator before the split. Maybe with Personal/Corporate this will work?)




#Compare R2 values, choose best fit
print(R2)
#Chosen Linear since all R2 are good
#Extrapolate values into present day
Coefs <-summary(linearmod)$coefficients
yr <-z[2]
estGDP =data.frame("Estimated GDP")
for (i in 2:(FT_Time-1)){
  estGDP=cbind(estGDP,ALB[1,i])
}
for (i in FT_Time:length(GDP_20)){
  estav <- Coefs[2,1]*(yr)+Coefs[1,1]
  yr<-yr+1
  estGDP=cbind(estGDP,estav)
}
#generate the fractional changes between estimates and actual GDP numbers
#Below Code is for countries that DON'T reject FT 
#percent_changes <- data.frame((-as.numeric(estGDP)[FT_Time:(length(ALB)-1)]+as.numeric(ALB[1,FT_Time:(length(ALB)-1)]))/as.numeric(ALB[1,FT_Time:(length(ALB)-1)]))


#countries that DO reject FT
RT_Time <-as.double((z[3]-1988+4))
percent_changes <- data.frame((-as.numeric(estGDP)[FT_Time:RT_Time]+as.numeric(ALB[1,FT_Time:RT_Time]))/as.numeric(ALB[1,FT_Time:RT_Time]))

#calculate the p value or > or <0 (depending on how the data looks) (T-test?)
names(percent_changes)="Comparisons"

mu<-mean(percent_changes$Comparisons)
estv<-sd(percent_changes$Comparisons)
#lower.tail=FALSE for P>0, nothing for P<0
score<-pnorm(0, mean=mu, sd=estv, lower.tail = FALSE)

#significant Growth against mean 
options(warn = oldw)
