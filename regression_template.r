company_data <- read.csv("prefix2.csv")
attach(company_data)

#Segment the Data
APPLE <- subset(company_data, Company=="APPLE", select=Company:Month)
#NEED TO CREATE HOLDOUTS
BESTBUY <- subset(company_data, Company=="BEST BUY", select=Company:Month)
H&M <- subset(company_data, Company=="H&M", select=Company:Month)
WALMART <- subset(company_data, Company=="WALMART", select=Company:Month)
PETCO <- subset(company_data, Company=="PETCO", select=Company:Month)
MCDONALDS <- subset(company_data, Company=="MCDONALD'S", select=Company:Month)
SHELL <- subset(company_data, Company=="SHELL", select=Company:Month)


#Runthrough of Regression with specific company -> Set targetco = COMPANY_NEEDED
targetco <- WALMART

#Create dummies for Month
ByMonth <- factor(targetco$Month)
Monthdummy <- model.matrix(~ByMonth)

#Create Dummy Variables bucketing into Regions
#Zip <- factor(targetco$Zip.Prefix)
#zipdummy <- model.matrix(~Zip)

#Create Dummy Variables for each integer of Temp
#Temps <- factor(as.integer(targetco$Temp/10))
#tempdummy <- model.matrix(~Temps)

#Create Dummy Variables for month


#Create a Squared Temperature
tempsquared <- targetco$Temp*targetco$Temp

#Interactives
#tempbyregion <- targetco$Temperature*targetco$Region
#precipbyregion <- targetco$Precipitation*targetco$Region
onlinebysearch <- targetco$Non.Physical.Sales*targetco$GoogTrend
onlinebylagged <- targetco$Non.Physical.Sales*targetco$GoogTrendM1

#Regression time - NEED TO ADD REGION, temp*region, precip*region
regress = lm(targetco$Sales~ targetco$Temp + 
               tempsquared + 
               targetco$Precip + 
               targetco$Non.Physical.Sales +
               Monthdummy +
               targetco$Gprice +
               targetco$GoogTrend + 
               targetco$GoogTrendM1 + 
               onlinebysearch +
               onlinebylagged
)
summary(regress)
plot(regress$residuals)

#Graphing
plot(targetco$Temp, targetco$Non.Physical.Sales)
