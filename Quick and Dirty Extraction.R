
library(readxl)
library(xlsx)
oldw <- getOption("warn")
options(warn = -1)



#Extract Data from Working Directory (Copy "General Data" to wherever you have R going)
GDP_20 <- read_excel("~/R/Data Extracts Excel Files/GDP_20.xlsx")
Adoption_Rejection_Dates <- read_excel("~/R/Data Extracts Excel Files/Adoption&Rejection Dates.xlsx")
#Initial Values (ALB > k=0, BEL > k=1 etc.)
k <- 0
#Set Values of %GDP Change, Break Point
set_GDP <-(GDP_20[2+k,])
x= data.frame(c("GDP Changes"),c("NA"),c("NA"),c("NA"))
for (i in 5:length(GDP_20)-1) {
 
  y <- ((as.numeric(GDP_20[2+k,i+1])-as.numeric(GDP_20[2+k,i]))/as.numeric(GDP_20[2+k,i]))
  x=cbind(x,y)
}
#Just find the dates on the A&R workbook, found in General Data
z=data.frame("Date of FT/RT", 2008,2014)
names(x)<-names(set_GDP)
ALB=rbind(set_GDP,x)

for (i in 4:length(GDP_20)){
  z=cbind(z,"NA")
}
names(z)<-names(set_GDP)
ALB=rbind(ALB,z)

q=data.frame("Time","NA","NA")
l <- 1988
for (i in 4:length(GDP_20)){
  q=cbind(q,l)
  l <- l+1
}

names(q)<-names(set_GDP)
ALB=rbind(ALB,q)

options(warn = oldw)