currentDirectory <- "C:\\Users\\John\\google_drive\\gsb_academic\\data_and_decisions\\regressionproject"
setwd(currentDirectory)
company_data <- read.csv("prefix3.csv")  

#Segment the Data
companies = c("APPLE", "BEST BUY", "H&M", "WALMART", "PETCO", "MCDONALD'S", "SHELL")

for (company in companies){
  #run the regression on each company
  targetco <- subset(company_data, Company=="APPLE")
  
  #filter out any '!NA!' results.  These can be caused by sales in zipcodes corresponding to military bases, territories, etc
  targetco <- targetco[targetco$Region != "!NA!",]
  
  #create sample set and holdout set.  Set the rand seed so the test is repeatable.
  set.seed(8675309)
  tSize <- 0.10 #arbitrary, but 10% seems about right
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
    
  #Create squared terms
  targetcoSample$tempSquared <- targetcoSample$Temp*targetcoSample$Temp
  targetcoHoldout$tempSquared <- targetcoHoldout$Temp*targetcoHoldout$Temp

  #N is the number of Yodlee users in that sample
  regress = lm(targetcoSample$Sales~ targetcoSample$N +
                 targetcoSample$Temp + 
                 targetcoSample$tempSquared + 
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
  
  
  print("**************************************")
  print(paste("Results for", company))
  print("**************************************")
  print(summary(regress))
  
  #Test the holdout sample
  targetcoHoldout$predictedSales = regress$coefficients["(Intercept)"]
  
  regressCoeffs <- names(regress$coefficients)[2:length(names(regress$coefficients))]  
  for (coeff in regressCoeffs){  
    betaVal <- 1*regress$coefficients[coeff]
    varname <- unlist(strsplit(coeff, "$", fixed=TRUE))[2]  
    if (!is.na(betaVal)){     
      targetcoHoldout$predictedSales = targetcoHoldout$predictedSales+ targetcoHoldout[,varname]* regress$coefficients[coeff]   
    } else {
      print(paste("Cannot get value for ", coeff))
    }
  }

  targetcoHoldout$predictedSalesSquaredErrors = (targetcoHoldout$Sales - targetcoHoldout$predictedSales) ** 2
  mean_standard_error_holdout <- sqrt(sum(targetcoHoldout$predictedSalesSquaredErrors)/nrow(targetcoHoldout))
  print("Holdout Standard Error:")
  print(mean_standard_error_holdout)
  #Graphing
} # end all companies loop