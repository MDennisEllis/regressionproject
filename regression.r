currentDirectory <- "C:\\Users\\John\\google_drive\\gsb_academic\\data_and_decisions\\regressionproject"
setwd(currentDirectory)
company_data <- read.csv("prefix3.csv")  

#Segment the Data
#TODO: CREATE HOLDOUTS
companies = c("APPLE", "BEST BUY", "H&M", "WALMART", "PETCO", "MCDONALD'S", "SHELL")

APPLE <- subset(company_data, Company=="APPLE")
BESTBUY <- subset(company_data, Company=="BEST BUY")
H_AND_M <- subset(company_data, Company=="H&M")
WALMART <- subset(company_data, Company=="WALMART")
PETCO <- subset(company_data, Company=="PETCO")
MCDONALDS <- subset(company_data, Company=="MCDONALD'S")
SHELL <- subset(company_data, Company=="SHELL")

#Runthrough of Regression with specific company -> Set targetcoSample = COMPANY_NEEDED
targetco <- WALMART
#filter out any '!NA!' results.  These can be caused by sales in zipcodes corresponding to military bases, territories, etc
targetco <- targetco[targetco$Region != "!NA!",]
names(targetcoSample)
#create sample set and holdout set
set.seed(8675309)
holdoutSize <- 0.10
totalSize <- nrow(targetco)
draw <- sample(1:totalSize)
targetcoHoldout <- targetco[draw < (totalSize * holdoutSize),]
targetcoSample <- targetco[draw > (totalSize * holdoutSize),]

#setup region dummy variable in the sample and holdout data frames
regions <- levels(targetco$Region)
regions <- regions[regions != "!NA!"] #filter out the NAs
regions <- regions[2:length(regions)] #remove one dummy

for (i in 1:length(regions)){
  varname <- paste("region", regions[i], "dummy", sep="_")
  targetcoSample[varname] = 1 * (targetcoSample$Region == regions[i])  
  targetcoHoldout[varname] = 1 * (targetcoHoldout$Region == regions[i])
  varname <- paste("region", regions[i], "times_temp_dummy", sep="_")
  targetcoSample[varname] = 1 * (targetcoSample$Region == regions[i]) * targetcoSample$Temp
  targetcoHoldout[varname] = 1 * (targetcoHoldout$Region == regions[i]) * targetcoHoldout$Temp
  varname <- paste("region", regions[i], "times_precip_dummy", sep="_")
  targetcoSample[varname] = 1 * (targetcoSample$Region == regions[i]) * targetcoSample$Precip
  targetcoHoldout[varname] = 1 * (targetcoHoldout$Region == regions[i]) * targetcoHoldout$Precip
}

#create the month dummy variables in the sample and holdout data frames
targetcoHoldout['Region']
for (month in 2:12){
  varname <- paste("month", month, "dummy", sep="_")
  targetcoSample[varname] = 1 * (targetcoSample$Month == month)  
  targetcoHoldout[varname] = 1 * (targetcoHoldout$Month == month)
}

#Create add'l interaction terms
targetcoSample$onlineBySearch <- targetcoSample$Non.Physical.Sales*targetcoSample$GoogTrend
targetcoSample$onlineBySearchLagged <- targetcoSample$Non.Physical.Sales*targetcoSample$GoogTrendM1

targetcoHoldout$onlineBySearch <- targetcoHoldout$Non.Physical.Sales*targetcoHoldout$GoogTrend
targetcoHoldout$onlineBySearchLagged <- targetcoHoldout$Non.Physical.Sales*targetcoHoldout$GoogTrendM1


#Create squared terms
targetcoSample$tempSquared <- targetcoSample$Temp*targetcoSample$Temp
targetcoHoldout$tempSquared <- targetcoHoldout$Temp*targetcoHoldout$Temp

regress = lm(targetcoSample$Sales~ targetcoSample$Temp + 
               targetcoSample$tempSquared + 
               targetcoSample$onlineBySearch +
               targetcoSample$onlineBySearchLagged + 
               targetcoSample$Precip + 
               targetcoSample$Non.Physical.Sales +
               targetcoSample$Gprice +
               targetcoSample$GoogTrend + 
               targetcoSample$GoogTrendM1 + 
               targetcoSample$month_2_dummy +
               targetcoSample$month_3_dummy +
               targetcoSample$month_4_dummy +
               targetcoSample$month_5_dummy +
               targetcoSample$month_6_dummy +
               targetcoSample$month_7_dummy +
               targetcoSample$month_8_dummy +
               targetcoSample$month_9_dummy +
               targetcoSample$month_10_dummy +
               targetcoSample$month_11_dummy +
               targetcoSample$month_12_dummy +
               targetcoSample$region_Northeast_dummy +
               targetcoSample$region_Midwest_dummy +
               targetcoSample$region_Hawaii_dummy +
               targetcoSample$region_South_dummy +
               targetcoSample$region_West_dummy +
               targetcoSample$region_Northeast_times_precip_dummy +
               targetcoSample$region_Midwest_times_precip_dummy +
               targetcoSample$region_Hawaii_times_precip_dummy +
               targetcoSample$region_South_times_precip_dummy +
               targetcoSample$region_West_times_precip_dummy +
               targetcoSample$region_Northeast_times_temp_dummy +
               targetcoSample$region_Midwest_times_temp_dummy +
               targetcoSample$region_Hawaii_times_temp_dummy +
               targetcoSample$region_South_times_temp_dummy +
               targetcoSample$region_West_times_temp_dummy
             )
summary(regress)


targetcoHoldout$predictedSales = 
  regress$coefficients["targetcoSample$tempSquared"] * targetcoHoldout$tempSquared+
  regress$coefficients["targetcoSample$onlineBySearch"] * targetcoHoldout$onlineBySearch+
  regress$coefficients["targetcoSample$onlineBySearchLagged"] * targetcoHoldout$onlineBySearchLagged+
  regress$coefficients["targetcoSample$Precip"] * targetcoHoldout$Precip+
  regress$coefficients["targetcoSample$Non.Physical.Sales"] * targetcoHoldout$Non.Physical.Sales+
  regress$coefficients["targetcoSample$Gprice"] * targetcoHoldout$Gprice+
  regress$coefficients["targetcoSample$GoogTrend"] * targetcoHoldout$GoogTrend+
  regress$coefficients["targetcoSample$GoogTrendM1"] * targetcoHoldout$GoogTrendM1+
  regress$coefficients["targetcoSample$month_2_dummy"] * targetcoHoldout$month_2_dummy+
  regress$coefficients["targetcoSample$month_3_dummy"] * targetcoHoldout$month_3_dummy+
  regress$coefficients["targetcoSample$month_4_dummy"] * targetcoHoldout$month_4_dummy+
  regress$coefficients["targetcoSample$month_5_dummy"] * targetcoHoldout$month_5_dummy+
  regress$coefficients["targetcoSample$month_6_dummy"] * targetcoHoldout$month_6_dummy+
  regress$coefficients["targetcoSample$month_7_dummy"] * targetcoHoldout$month_7_dummy+
  regress$coefficients["targetcoSample$month_8_dummy"] * targetcoHoldout$month_8_dummy+
  regress$coefficients["targetcoSample$month_9_dummy"] * targetcoHoldout$month_9_dummy+
  regress$coefficients["targetcoSample$month_10_dummy"] * targetcoHoldout$month_10_dummy+
  regress$coefficients["targetcoSample$month_11_dummy"] * targetcoHoldout$month_11_dummy+
  regress$coefficients["targetcoSample$month_12_dummy"] * targetcoHoldout$month_12_dummy+
  regress$coefficients["targetcoSample$region_Northeast_dummy"] * targetcoHoldout$region_Northeast_dummy+
  regress$coefficients["targetcoSample$region_Midwest_dummy"] * targetcoHoldout$region_Midwest_dummy+
  regress$coefficients["targetcoSample$region_Hawaii_dummy"] * targetcoHoldout$region_Hawaii_dummy+
  regress$coefficients["targetcoSample$region_South_dummy"] * targetcoHoldout$region_South_dummy+
  regress$coefficients["targetcoSample$region_West_dummy"] * targetcoHoldout$region_West_dummy+
  regress$coefficients["targetcoSample$region_Northeast_times_precip_dummy"] * targetcoHoldout$region_Northeast_times_precip_dummy+
  regress$coefficients["targetcoSample$region_Midwest_times_precip_dummy"] * targetcoHoldout$region_Midwest_times_precip_dummy+
  regress$coefficients["targetcoSample$region_Hawaii_times_precip_dummy"] * targetcoHoldout$region_Hawaii_times_precip_dummy+
  regress$coefficients["targetcoSample$region_South_times_precip_dummy"] * targetcoHoldout$region_South_times_precip_dummy+
  regress$coefficients["targetcoSample$region_West_times_precip_dummy"] * targetcoHoldout$region_West_times_precip_dummy+
  regress$coefficients["targetcoSample$region_Northeast_times_temp_dummy"] * targetcoHoldout$region_Northeast_times_temp_dummy+
  regress$coefficients["targetcoSample$region_Midwest_times_temp_dummy"] * targetcoHoldout$region_Midwest_times_temp_dummy+
  regress$coefficients["targetcoSample$region_Hawaii_times_temp_dummy"] * targetcoHoldout$region_Hawaii_times_temp_dummy+
  regress$coefficients["targetcoSample$region_South_times_temp_dummy"] * targetcoHoldout$region_South_times_temp_dummy+
  regress$coefficients["targetcoSample$region_West_times_temp_dummy"] * targetcoHoldout$region_West_times_temp_dummy

targetcoHoldout$predictedSalesSquaredErrors = (targetcoHoldout$Sales - targetcoHoldout$predictedSales) ** 2
mean_standard_error_holdout <- sqrt(sum(targetcoHoldout$predictedSalesSquaredErrors)/nrow(targetcoHoldout))
print(mean_standard_error_holdout)
#Graphing
