company_data <- read.csv("data_mat2.txt")
attach(company_data)

#Segment the Data
APPLE <- subset(company_data, Company=="APPLE", select=Company:Gasprice)
APPLE_SubSample <- sample(APPLE)
BESTBUY <- subset(company_data, Company=="BEST BUY", select=Company:Gasprice)
H&M <- subset(company_data, Company=="H&M", select=Company:Gasprice)
WALMART <- subset(company_data, Company=="WALMART", select=Company:Gasprice)
PETCO <- subset(company_data, Company=="PETCO", select=Company:Gasprice)
MCDONALDS <- subset(company_data, Company=="MCDONALD'S", select=Company:Gasprice)
SHELL <- subset(company_data, Company=="SHELL", select=Company:Gasprice)


#Runthrough of Regression with specific company -> Set targetco = COMPANY_NEEDED
targetco <- APPLE

#Create dummies for weeks for top 5 retail (Black Friday, etc)
Byweek <- factor(targetco$Week.Number)
weekdummy <- model.matrix(~Byweek)

#Create Dummy Variables bucketing into Regions
Zip <- factor(targetco$Zip.Code.Prefix)
zipdummy <- model.matrix(~Zip)

#Create Dummy Variables for each integer of Temp
Temp <- factor(as.integer(targetco$Temperature/10))
tempdummy <- model.matrix(~Temp)

#Create a Squared Temperature
tempsquared <- targetco$Temperature*targetco$Temperature

#Regression time
regress = lm(targetco$Sales~ targetco$Precipitation + 
               targetco$SearchTrends + 
               targetco$SearchTrendsLaggedby1 + 
               targetco$Week.Number+ 
               targetco$Temperature + 
               tempsquared)
summary(regress)
plot(regress$residuals)

#Graphing
plot(targetco$Temperature, targetco$Sales)

