## Setting the directory
setwd("H:/Srilekha/PGDDS/Linear Regression/Assignment")

##importing  the data set 
cardata<-read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)

##Extrating company name from CarName variable by removing spaces
cardata$Car_Company <- gsub("\\s.*","",cardata$CarName)

cardata$Car_Company

str(cardata$Car_Company)

#converting all values of car_company to lower case
cardata$Car_Company <- tolower(cardata$Car_Company)

#rectifying the spelling mistakes in car_company
cardata$Car_Company <- gsub("vw","volkswagen",cardata$Car_Company)
cardata$Car_Company <- gsub("vokswagen","volkswagen",cardata$Car_Company)
cardata$Car_Company <- gsub("toyouta","toyota",cardata$Car_Company)
cardata$Car_Company <- gsub("maxda","mazda",cardata$Car_Company)
cardata$Car_Company <- gsub("porcshe","porsche",cardata$Car_Company)

## deal with all variables one by one
## If there are only 2 levels, convert into 1 and 0
## if there are more tha 2 levels , create dumy variables using model.matrix 

cardata$fueltype<-as.factor(cardata$fueltype)
str(cardata$fueltype)
levels(cardata$fueltype)<-c(1,0)
cardata$fueltype<-as.numeric(levels(cardata$fueltype))[cardata$fueltype]


str(cardata$aspiration)
cardata$aspiration<-as.factor(cardata$aspiration)
levels(cardata$aspiration)<-c(1,0)
cardata$aspiration<-as.numeric(levels(cardata$aspiration))[cardata$aspiration]
str(cardata$aspiration)

cardata$doornumber<-as.factor(cardata$doornumber)
str(cardata$doornumber)
levels(cardata$doornumber)<-c(1,0)
cardata$doornumber<-as.numeric(levels(cardata$doornumber))[cardata$doornumber]
str(cardata$doornumber)

cardata$carbody<-as.factor(cardata$carbody)
str(cardata$carbody)
 dummy_carbody<-model.matrix(~carbody,data = cardata)
View(dummy_carbody)
 dummy_carbody<-dummy_carbody[,-1]

 

cardata$carbody<-as.factor(cardata$carbody)
dummy_carbody<-model.matrix(~carbody - 1, data = cardata)
View(dummy_carbody)
dummy_carbody<-dummy_carbody[,-1]
View(dummy_carbody)


cardata$drivewheel<-gsub("4wd","fwd",cardata$drivewheel)
View(cardata)
cardata$drivewheel<-as.factor(cardata$drivewheel)
str(cardata$drivewheel)
levels(cardata$drivewheel)<-c(1,0)
cardata$drivewheel<-as.numeric(levels(cardata$drivewheel))[cardata$drivewheel]
str(cardata$drivewheel)

cardata$enginelocation<-as.factor(cardata$enginelocation)
str(cardata$enginelocation)
levels(cardata$enginelocation)<-c(1,0)
cardata$enginelocation<-as.numeric(levels(cardata$enginelocation))[cardata$enginelocation]
str(cardata$enginelocation)

cardata$enginetype<-as.factor(cardata$enginetype)
str(cardata$enginetype)
dummy_enginetype<-model.matrix(~enginetype - 1, data = cardata)
dummy_enginetype<-dummy_enginetype[,-1]
str(dummy_enginetype)

cardata$cylindernumber<-as.factor(cardata$cylindernumber)
str(cardata$cylindernumber)
dummy_cylindernum<-model.matrix(~cylindernumber - 1 , data = cardata)
View(dummy_cylindernum)
dummy_cylindernum<-dummy_cylindernum[,-1]

str(cardata$fuelsystem)
cardata$fuelsystem<-as.factor(cardata$fuelsystem)
dummy_fuelsystem<-model.matrix(~fuelsystem - 1, data = cardata)
View(dummy_fuelsystem)
dummy_fuelsystem<-dummy_fuelsystem[,-1]
str(dummy_fuelsystem)

cardata$symboling<- as.factor(cardata$symboling)
str(cardata$symboling)
dummy_symbolising<-model.matrix(~symboling -1, data = cardata)
View(dummy_symbolising)
dummy_symbolising<-dummy_symbolising[,-1]

str(cardata$Car_Company)
cardata$Car_Company<-as.factor(cardata$Car_Company)
dummy_company1<-model.matrix(~Car_Company - 1, data = cardata)
dummy_company1<-dummy_company1[,-1]

##Dropping columns from the dataset to make it suitable for modelling.
cardata_new<-cardata[c(-1,-2,-3,-7,-15,-16,-18,-27)]

## Creating new data set by adding dummy variables
cardata_new<-cbind(cardata_new,dummy_carbody,dummy_company1,dummy_cylindernum,dummy_enginetype,dummy_fuelsystem,dummy_symbolising)

View(cardata_new)


## checking for outliers and removing them if necessary

quantile(cardata_new$wheelbase,seq(0,1,0.01))
## no outliers
quantile(cardata_new$carlength,seq(0,1,0.01))

quantile(cardata_new$carwidth,seq(0,1,0.01))

quantile(cardata_new$carheight,seq(0,1,0.01))

quantile(cardata_new$curbweight,seq(0,1,0.01))
cardata_new$curbweight[which(cardata_new$curbweight<1819.72)]<-1819.72

quantile(cardata_new$enginesize,seq(0,1,0.01))
cardata_new$enginesize[which(cardata_new$enginesize>209.00)]<-209.00

quantile(cardata_new$boreratio,seq(0,1,0.01))

quantile(cardata_new$stroke,seq(0,1,0.01))

quantile(cardata_new$compressionratio,seq(0,1,0.01))
cardata_new$compressionratio[which(cardata_new$compressionratio>10.9400)]<-10.9400

quantile(cardata_new$horsepower,seq(0,1,0.01))
cardata_new$horsepower[which(cardata_new$horsepower>184.00)]<-184.00

quantile(cardata_new$peakrpm,seq(0,1,0.01))

quantile(cardata_new$citympg,seq(0,1,0.01))
cardata_new$citympg[which(cardata_new$citympg>38.00)]<-38.00

quantile(cardata_new$highwaympg,seq(0,1,0.01))
cardata_new[which(cardata_new$highwaympg>49.88)]<-49.88



 ## seperating training and test indices
 
set.seed(100)

trainindices= sample(1:nrow(cardata_new),0.7*nrow(cardata_new))

train = cardata_new[trainindices,]

test = cardata_new[-trainindices,]

## creating a linear model with all variables

model_1<-lm(price~., data= train)

summary(model_1)
#the R-squared value is very high, but very few variables seem to be significant
#eliminating the variables that are insignificant by using step-AIC

install.packages("MASS")
library(MASS)

install.packages("car")
library(car)

##building a model using step AIC
model_step<-stepAIC(model_1,direction = "both")

model_2<-lm(formula = price ~Car_Companyhonda      
            + symboling1            
            + enginetypeohc         
            + wheelbase            
            + Car_Companymercury    
            + carbodysedan          
            + carbodyhatchback       
            + enginesize            
            + stroke                 
            + Car_Companyrenault     
            + Car_Companysaab       
            + carbodywagon           
            + Car_Companychevrolet  
            + aspiration            
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen  
            + carwidth             
            + Car_Companyplymouth    
            + Car_Companydodge      
            + Car_Companypeugeot    
            + drivewheel             
            + Car_Companyjaguar      
            + cylindernumberfour    
            + cylindernumbersix      
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru      
            + cylindernumberfive    
            + Car_Companybmw       
            + enginelocation , data = train)       

summary(model_2)

vif(model_2)

## dropping insignificant variables by checking p value and vif value

model_3<-lm(formula = price ~Car_Companyhonda      
            + symboling1            
            + enginetypeohc         
            + wheelbase            
            + Car_Companymercury    
            + carbodysedan          
            + carbodyhatchback         
            + stroke                 
            + Car_Companyrenault     
            + Car_Companysaab       
            + carbodywagon           
            + Car_Companychevrolet  
            + aspiration            
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen  
            + carwidth             
            + Car_Companyplymouth    
            + Car_Companydodge      
            + Car_Companypeugeot    
            + drivewheel             
            + Car_Companyjaguar      
            + cylindernumberfour    
            + cylindernumbersix      
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru      
            + cylindernumberfive    
            + Car_Companybmw       
            + enginelocation , data = train)

summary(model_3)
vif(model_3)

## dropping insignificant variables by checking p value and vif value


model_4<-lm(formula = price ~Car_Companyhonda      
            + symboling1            
            + enginetypeohc       
            + Car_Companymercury    
            + carbodysedan          
            + carbodyhatchback         
            + stroke                 
            + Car_Companyrenault     
            + Car_Companysaab       
            + carbodywagon           
            + Car_Companychevrolet  
            + aspiration            
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen  
            + carwidth             
            + Car_Companyplymouth    
            + Car_Companydodge      
            + Car_Companypeugeot    
            + drivewheel             
            + Car_Companyjaguar      
            + cylindernumberfour    
            + cylindernumbersix      
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru      
            + cylindernumberfive    
            + Car_Companybmw       
            + enginelocation , data = train)

summary(model_4)
       
vif(model_4)

## dropping insignificant variables by checking p value and vif value

model_5<-lm(formula = price ~Car_Companyhonda      
            + symboling1            
            + enginetypeohc       
            + Car_Companymercury    
            + carbodysedan          
            + carbodyhatchback         
            + stroke                 
            + Car_Companyrenault     
            + Car_Companysaab       
            + carbodywagon           
            + Car_Companychevrolet  
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen
            + Car_Companyplymouth    
            + Car_Companydodge      
            + Car_Companypeugeot    
            + drivewheel             
            + Car_Companyjaguar      
            + cylindernumberfour    
            + cylindernumbersix      
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru      
            + cylindernumberfive    
            + Car_Companybmw       
            + enginelocation
            +carwidth, data = train)

summary(model_5)
vif(model_5)

## removing cylindernumberfour
model_6<-lm(formula = price ~Car_Companyhonda      
            + symboling1            
            + enginetypeohc       
            + Car_Companymercury    
            + carbodysedan          
            + carbodyhatchback         
            + stroke                 
            + Car_Companyrenault     
            + Car_Companysaab       
            + carbodywagon           
            + Car_Companychevrolet  
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen
            + Car_Companyplymouth    
            + Car_Companydodge      
            + Car_Companypeugeot    
            + drivewheel             
            + Car_Companyjaguar   
            + cylindernumbersix      
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru      
            + cylindernumberfive    
            + Car_Companybmw       
            + enginelocation
            +carwidth, data = train)

summary(model_6)
vif(model_6)


model_7<-lm(formula = price ~Car_Companyhonda      
            + symboling1            
            + enginetypeohc     
            + carbodysedan          
            + carbodyhatchback      
            + Car_Companyrenault     
            + Car_Companysaab       
            + carbodywagon   
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen
            + Car_Companyplymouth    
            + Car_Companydodge      
            + Car_Companypeugeot    
            + drivewheel             
            + Car_Companyjaguar   
            + cylindernumbersix      
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru      
            + cylindernumberfive    
            + Car_Companybmw       
            + enginelocation
            +carwidth, data = train)
summary(model_7)



model_8<-lm(formula = price ~Car_Companyhonda      
            + enginetypeohc       
            + Car_Companyrenault     
            + Car_Companysaab       
            + carbodywagon   
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen
            + Car_Companyplymouth    
            + Car_Companydodge      
            + Car_Companypeugeot    
            + drivewheel             
            + Car_Companyjaguar    
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru     
            + Car_Companybmw       
            + enginelocation
            +carwidth, data = train)

summary(model_8)


model_9<-lm(formula = price ~      
              + enginetypeohc       
            + Car_Companyrenault     
            + Car_Companysaab
            + curbweight            
            + Car_Companynissan
            + Car_Companyvolkswagen
            + Car_Companyplymouth  
            + Car_Companypeugeot   
            + Car_Companyjaguar    
            + Car_Companymazda       
            + Car_Companytoyota      
            + Car_Companybuick       
            + Car_Companymitsubishi 
            + Car_Companysubaru     
            + Car_Companybmw       
            + enginelocation
            +carwidth, data = train)

summary(model_9)
vif(model_9)


model_10<-lm(formula = price ~      
               + enginetypeohc       
             + Car_Companyrenault 
             + curbweight            
             + Car_Companynissan
             + Car_Companypeugeot   
             + Car_Companyjaguar    
             + Car_Companymazda       
             + Car_Companytoyota      
             + Car_Companybuick       
             + Car_Companymitsubishi 
             + Car_Companysubaru     
             + Car_Companybmw       
             + enginelocation
             +carwidth, data = train)

summary(model_10)
vif(model_10)


model_11<-lm(formula = price ~      
               + enginetypeohc 
             + curbweight
             + Car_Companypeugeot   
             + Car_Companyjaguar    
             + Car_Companymazda       
             + Car_Companytoyota      
             + Car_Companybuick 
             + Car_Companysubaru     
             + Car_Companybmw       
             + enginelocation
             +carwidth, data = train)
summary(model_11)


model_12<-lm(formula = price ~      
               + enginetypeohc 
             +Car_Companyporsche
             + curbweight
             + Car_Companypeugeot   
             + Car_Companyjaguar     
             + Car_Companybuick 
             + Car_Companysubaru     
             + Car_Companybmw       
             + enginelocation
             +carwidth, data = train)

summary(model_12)

vif(model_12)

model_13<-lm(formula = price ~      
               + enginetypeohc 
             + curbweight
             + Car_Companypeugeot   
             + Car_Companyjaguar     
             + Car_Companybuick 
             + Car_Companysubaru     
             + Car_Companybmw       
             + enginelocation
             +carwidth, data = train)
summary(model_13)


## The prices of the variables are mostly dependent on Companies of the cars, enginetypeohc ,engine location and car width
## After removing all insignificant variables, we can say that model_13 predicts prices more accurately

## The summary of the final model
#Coefficients:
 #                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -6.265e+04  9.622e+03  -6.511 1.41e-09 ***
#enginetypeohc      -3.301e+03  5.287e+02  -6.244 5.33e-09 ***
#curbweight          4.843e+00  8.279e-01   5.850 3.62e-08 ***
#Car_Companypeugeot -6.519e+03  9.776e+02  -6.668 6.33e-10 ***
#Car_Companyjaguar   6.900e+03  1.677e+03   4.115 6.75e-05 ***
#Car_Companybuick    8.511e+03  1.009e+03   8.433 4.92e-14 ***
#Car_Companysubaru  -3.996e+03  9.753e+02  -4.098 7.21e-05 ***
#Car_Companybmw      1.054e+04  1.226e+03   8.597 1.97e-14 ***
#enginelocation     -1.973e+04  1.282e+03 -15.387  < 2e-16 ***
#carwidth            1.289e+03  1.732e+02   7.438 1.13e-11 ***

## It clearly says that the prices of the cars directly proportional to curbweight, Car_Companyjaguar ,Car_Companybuick, Car_Companybmw, carwidth and 
## is inversely proportional to carwidth,Car_Companypeugeot ,Car_Companysubaru and enginelocation

## Predicting the price from the final model.
cardata_new$Predicted_price <- predict(model_13, cardata_new)

##Prediction Error
cardata_new$error<-cardata_new$price - cardata_new$Predicted_price

## By seeing the values of the errors, we can say that they are randomly distribuuted. Hence we can say that model_13 is appropriate.