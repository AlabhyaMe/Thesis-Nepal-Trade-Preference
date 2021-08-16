#Loading Package
library(readxl)
library(tidyverse)
library(zoo)

setwd("~/OneDrive/My file/Thesis/Data Analysis")
rm(list = ls())

# 1 Importing and cleaning the major data set
US_imports_value <- read_excel("US imports value.xlsx", 
                               sheet = "General Customs Value", skip = 2)

US_imports_quantity <- read_excel("US imports quantity.xlsx", 
                                  sheet = "General 1st Unit of Qty")

Products_Approved_for_Nepal_Trade_Preferences_Program <- read_excel("Products-Approved-for-Nepal-Trade-Preferences-Program.xlsx")

GSP <- read_excel("GSP eligible products only from LDBDC June 2018.xlsx", skip = 1)
GSPall <- read_excel("GSP eligible products for all BDC June 2018.xlsx", skip = 3)

Products_Approved_for_Nepal_Trade_Preferences_Program <- na.omit(Products_Approved_for_Nepal_Trade_Preferences_Program)
Products_Approved_for_Nepal_Trade_Preferences_Program <- Products_Approved_for_Nepal_Trade_Preferences_Program[!duplicated(Products_Approved_for_Nepal_Trade_Preferences_Program),]

GSP <- na.omit(GSP)
GSPall <- na.omit(GSPall)
GSPall <- GSPall[,-4]
colnames(GSPall)[1] <- "HTS Number"
GSP <- bind_rows(GSP, GSPall)
rm(GSPall)

US_imports_quantity$`HTS Number` <- str_remove_all(US_imports_quantity$`HTS Number`,"[[:punct:]]")
US_imports_value$`HTS Number` <- str_remove_all(US_imports_value$`HTS Number`,"[[:punct:]]")
GSP$`HTS Number`<- str_remove_all(GSP$`HTS Number`,"[[:punct:]]")


# 3 Ignoring the unit of quantity 
Quant <- aggregate(.~`HTS Number`+ `Special Import Program` +`Description` , data= US_imports_quantity[ ,c(3,4,5,7:26)], FUN=sum)
Quant_US <- gather(Quant, "Year", "Quantity", 4:23) 
Quant_US$Year <- as.double(substr(Quant_US$Year, 5,9))

# 4 Value aggregate
Value <- gather(US_imports_value[!is.na(US_imports_value$`HTS Number`),-c(1,2)], "Year", "Value",4:23 )
Value$Year <- as.double(substr(Value$Year, 5,9))

All <- left_join(Quant_US,Value, by=c("HTS Number","Description","Special Import Program", "Year"))
rm(Value,Quant)


# 5 After 2016 
# 6 Table 1 ; since the policy was effective from 2017, we are calculating data after 2016

All$NTPP <- ifelse(All$`HTS Number` %in% Products_Approved_for_Nepal_Trade_Preferences_Program$HTS, 1,0)
All$GSP <- ifelse(All$`HTS Number` %in% GSP$`HTS Number`, 1,0)

rm(list=setdiff(ls(), c("All","GSP", "Products_Approved_for_Nepal_Trade_Preferences_Program")))

All$Benefit <- ifelse(All$NTPP==1, "NTPP", ifelse(All$GSP==1 & All$NTPP!=1, "GSP", "No Benefit"))

AllEasy <- summarise(group_by(All, Benefit,Year), Quantity = sum(Quantity), Value= sum(Value))

AllEasy$MovingAverageQuantity <- c(NA,rollmean(AllEasy$Quantity,3),NA)
AllEasy$MovingAverageValue <- c(NA,rollmean(AllEasy$Value,3),NA)

#Trying out parallel trend assumption
#Quantity over years
AllEasy%>%
  filter(Year>2010)%>%
  ggplot( aes(x=Year, y=Quantity, group=Benefit, color= Benefit)) +
  geom_line() + geom_point()

#Log Quantity
AllEasy%>%
  filter(Year>2010)%>%
  ggplot( aes(x=Year, y=log(Quantity), group=Benefit, color= Benefit)) +
  geom_line() + geom_point()

# Three years moving Quantity
AllEasy%>%
  filter(Year>2010)%>%
  ggplot( aes(x=Year, y=MovingAverageQuantity, group=Benefit, color= Benefit)) +
  geom_line() + geom_point()

#Three years moving Value
AllEasy%>%
  filter(Year>2010)%>%
  ggplot( aes(x=Year, y=MovingAverageValue, group=Benefit, color= Benefit)) +
  geom_line() + geom_point()

#We find the Three years moving Quantity has somewhat parallel trend since 2010,
#Since trade is very dynamic event, a trend at three years would suffice our assumption

FinalDataSet <- All %>% filter(Year>2009)
FinalDataSet <- summarise(group_by(FinalDataSet, `HTS Number`, Year), Total_Quantity= sum(Quantity)) # summarize because the product must not repeat more than one in a year

FinalDataSet$NTPP <- ifelse(FinalDataSet$`HTS Number` %in% Products_Approved_for_Nepal_Trade_Preferences_Program$HTS,1,0)
FinalDataSet$GSP <- ifelse(FinalDataSet$`HTS Number` %in% GSP$`HTS Number`,1,0)
FinalDataSet$InterventionYear <- ifelse(FinalDataSet$Year>2016,1,0)

FinalDataSet <- FinalDataSet[!c(FinalDataSet$GSP==1 & FinalDataSet$NTPP==0),] #If GSP but not NTPP, it is deleted

extra <- summarise(group_by(All,`HTS Number`), total = sum(Quantity))  ## To delete product with no trade in the record (any year)
extra <- extra$`HTS Number`[extra$total==0]

FinalDataSet <- FinalDataSet[!(FinalDataSet$`HTS Number` %in% extra),]

DID <- lm(log(Total_Quantity+1) ~  NTPP*InterventionYear, data=FinalDataSet )
summary(DID)

