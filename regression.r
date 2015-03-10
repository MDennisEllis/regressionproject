TWO_DIGIT_DATA <- F;
if (TWO_DIGIT_DATA){
  company_data <- read.csv("prefix2.csv")  
} else {
  company_data <- read.csv("prefix3.csv")  
}

region_data <- read.csv("zip_code_regions.csv")
TWO_DIGIT_DATA <- F;

#Segment the Data
#TODO: CREATE HOLDOUTS
APPLE <- subset(company_data, Company=="APPLE", select=Company:Month)
BESTBUY <- subset(company_data, Company=="BEST BUY", select=Company:Month)
H_AND_M <- subset(company_data, Company=="H&M", select=Company:Month)
WALMART <- subset(company_data, Company=="WALMART", select=Company:Month)
PETCO <- subset(company_data, Company=="PETCO", select=Company:Month)
MCDONALDS <- subset(company_data, Company=="MCDONALD'S", select=Company:Month)
SHELL <- subset(company_data, Company=="SHELL", select=Company:Month)

#Runthrough of Regression with specific company -> Set targetco = COMPANY_NEEDED
targetco <- BESTBUY


#add regions
region_names <- names(region_data)
if (TWO_DIGIT_DATA){
  region_names[region_names == "zip_first_two_digits"] <- "Zip.Prefix"
} else {
  region_names[region_names == "zip_first_three_digits"] <- "Zip.Prefix"
}
names(region_data) <- region_names
targetco <-merge(targetco, region_data, by='Zip.Prefix')
#now...there will be quite a few records with "!NA!" in the region.  These are regions that are not matched to a US State, including army bases, us territories, etc.
#the data to match zips to state was collected from the USPS zipcode database
targetco <- targetco[targetco$Region != '!NA!', ]

#create dummies for region
weekDummy <- model.matrix(~factor(targetco$WeekNum))
monthDummy <- model.matrix(~factor(targetco$Month))
regionDummy <- model.matrix(~factor(targetco$Region))


#Create interaction terms
regionPrecip <- regionDummy * targetco$Precip
regionTemp <- regionDummy * targetco$Temp
onlineBySearch <- targetco$Non.Physical.Sales*targetco$GoogTrend
onlineBySearchLagged <- targetco$Non.Physical.Sales*targetco$GoogTrendM1

#Create squared terms
tempSquared <- targetco$Temp*targetco$Temp

regress = lm(targetco$Sales~ targetco$Temp + 
               tempSquared + 
               targetco$Precip + 
               targetco$Non.Physical.Sales +
               monthDummy +
               targetco$Gprice +
               targetco$GoogTrend + 
               targetco$GoogTrendM1 + 
               onlineBySearch +
               onlineBySearchLagged +
               regionDummy + 
               regionTemp + 
               regionPrecip
)
summary(regress)

#Graphing
plot(targetco$Month, targetco$Sales)