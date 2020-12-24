setwd("/Users/praveenmohan/Desktop/Fall2020/IE500/untitled folder/DataCleaning")

# Libraries
library(dplyr)
library(caret)

# Main data frame
df1 <- read.csv("city_day.csv")

# Removing unwanted columns
df1 <- df1[,-c(1,17,18,19,21,22,23,26,27,28)]

summary(df1)

# Extracting Month column
df1$Month <- substring(df1$Date,6,7)
df1$Month <- as.numeric(df1$Month)
df1$Month <- as.factor(df1$Month)
levels(df1$Month)

# Imputing NA's 

# Level 1 Imputation - Group by City, Year, Month
df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(PM2.5=ifelse(is.na(PM2.5),mean(PM2.5,na.rm=TRUE),PM2.5))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(PM10=ifelse(is.na(PM10),mean(PM10,na.rm=TRUE),PM10))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(NO=ifelse(is.na(NO),mean(NO,na.rm=TRUE),NO))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(NO2=ifelse(is.na(NO2),mean(NO2,na.rm=TRUE),NO2))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(NOx=ifelse(is.na(NOx),mean(NOx,na.rm=TRUE),NOx))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(NH3=ifelse(is.na(NH3),mean(NH3,na.rm=TRUE),NH3))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(CO=ifelse(is.na(CO),mean(CO,na.rm=TRUE),CO))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(SO2=ifelse(is.na(SO2),mean(SO2,na.rm=TRUE),SO2))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(O3=ifelse(is.na(O3),mean(O3,na.rm=TRUE),O3))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(Benzene=ifelse(is.na(Benzene),mean(Benzene,na.rm=TRUE),Benzene))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(Toluene=ifelse(is.na(Toluene),mean(Toluene,na.rm=TRUE),Toluene))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(Xylene=ifelse(is.na(Xylene),mean(Xylene,na.rm=TRUE),Xylene))

df1 <- df1 %>% group_by(City, Year, Month) %>%
  mutate(AQI=ifelse(is.na(AQI),mean(AQI,na.rm=TRUE),AQI))
summary(df1)

# Level 2 Imputation - Group by City, Year, Season
df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(PM2.5=ifelse(is.na(PM2.5),mean(PM2.5,na.rm=TRUE),PM2.5))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(PM10=ifelse(is.na(PM10),mean(PM10,na.rm=TRUE),PM10))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(NO=ifelse(is.na(NO),mean(NO,na.rm=TRUE),NO))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(NO2=ifelse(is.na(NO2),mean(NO2,na.rm=TRUE),NO2))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(NOx=ifelse(is.na(NOx),mean(NOx,na.rm=TRUE),NOx))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(NH3=ifelse(is.na(NH3),mean(NH3,na.rm=TRUE),NH3))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(CO=ifelse(is.na(CO),mean(CO,na.rm=TRUE),CO))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(SO2=ifelse(is.na(SO2),mean(SO2,na.rm=TRUE),SO2))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(O3=ifelse(is.na(O3),mean(O3,na.rm=TRUE),O3))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(Benzene=ifelse(is.na(Benzene),mean(Benzene,na.rm=TRUE),Benzene))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(Toluene=ifelse(is.na(Toluene),mean(Toluene,na.rm=TRUE),Toluene))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(Xylene=ifelse(is.na(Xylene),mean(Xylene,na.rm=TRUE),Xylene))

df1 <- df1 %>% group_by(City, Year, Season) %>%
  mutate(AQI=ifelse(is.na(AQI),mean(AQI,na.rm=TRUE),AQI))
summary(df1)
 
# Level 3 Imputation - Group by City, Year
df1 <- df1 %>% group_by(City, Year) %>%
  mutate(PM2.5=ifelse(is.na(PM2.5),mean(PM2.5,na.rm=TRUE),PM2.5))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(PM10=ifelse(is.na(PM10),mean(PM10,na.rm=TRUE),PM10))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(NO=ifelse(is.na(NO),mean(NO,na.rm=TRUE),NO))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(NO2=ifelse(is.na(NO2),mean(NO2,na.rm=TRUE),NO2))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(NOx=ifelse(is.na(NOx),mean(NOx,na.rm=TRUE),NOx))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(NH3=ifelse(is.na(NH3),mean(NH3,na.rm=TRUE),NH3))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(CO=ifelse(is.na(CO),mean(CO,na.rm=TRUE),CO))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(SO2=ifelse(is.na(SO2),mean(SO2,na.rm=TRUE),SO2))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(O3=ifelse(is.na(O3),mean(O3,na.rm=TRUE),O3))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(Benzene=ifelse(is.na(Benzene),mean(Benzene,na.rm=TRUE),Benzene))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(Toluene=ifelse(is.na(Toluene),mean(Toluene,na.rm=TRUE),Toluene))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(Xylene=ifelse(is.na(Xylene),mean(Xylene,na.rm=TRUE),Xylene))

df1 <- df1 %>% group_by(City, Year) %>%
  mutate(AQI=ifelse(is.na(AQI),mean(AQI,na.rm=TRUE),AQI))
summary(df1)

# Level 4 Imputation - Group by City
df1 <- df1 %>% group_by(City) %>%
  mutate(PM2.5=ifelse(is.na(PM2.5),mean(PM2.5,na.rm=TRUE),PM2.5))

df1 <- df1 %>% group_by(City) %>%
  mutate(PM10=ifelse(is.na(PM10),mean(PM10,na.rm=TRUE),PM10))

df1 <- df1 %>% group_by(City) %>%
  mutate(NO=ifelse(is.na(NO),mean(NO,na.rm=TRUE),NO))

df1 <- df1 %>% group_by(City) %>%
  mutate(NO2=ifelse(is.na(NO2),mean(NO2,na.rm=TRUE),NO2))

df1 <- df1 %>% group_by(City) %>%
  mutate(NOx=ifelse(is.na(NOx),mean(NOx,na.rm=TRUE),NOx))

df1 <- df1 %>% group_by(City) %>%
  mutate(NH3=ifelse(is.na(NH3),mean(NH3,na.rm=TRUE),NH3))

df1 <- df1 %>% group_by(City) %>%
  mutate(CO=ifelse(is.na(CO),mean(CO,na.rm=TRUE),CO))

df1 <- df1 %>% group_by(City) %>%
  mutate(SO2=ifelse(is.na(SO2),mean(SO2,na.rm=TRUE),SO2))

df1 <- df1 %>% group_by(City) %>%
  mutate(O3=ifelse(is.na(O3),mean(O3,na.rm=TRUE),O3))

df1 <- df1 %>% group_by(City) %>%
  mutate(Benzene=ifelse(is.na(Benzene),mean(Benzene,na.rm=TRUE),Benzene))

df1 <- df1 %>% group_by(City) %>%
  mutate(Toluene=ifelse(is.na(Toluene),mean(Toluene,na.rm=TRUE),Toluene))

df1 <- df1 %>% group_by(City) %>%
  mutate(Xylene=ifelse(is.na(Xylene),mean(Xylene,na.rm=TRUE),Xylene))

df1 <- df1 %>% group_by(City) %>%
  mutate(AQI=ifelse(is.na(AQI),mean(AQI,na.rm=TRUE),AQI))
summary(df1)



df1$PM <- rowSums(df1[,c("PM2.5","PM10")], na.rm = TRUE)
df1$BTX <- rowSums(df1[,c("Benzene","Toluene","Xylene")], na.rm = TRUE)


# Removing unwanted columns
df1 <- df1[,-c(2,3,4,12,13,14)]

# Final Imputation
df1$O3[is.na(df1$O3)] <- mean(df1$O3, na.rm = T)
df1$NOx[is.na(df1$NOx)] <- mean(df1$NOx, na.rm = T)
df1$NH3[is.na(df1$NH3)] <- mean(df1$NH3, na.rm = T)

summary(df1)

# Rearranging columns
df1 <- df1[,c(1,10,11,13,12,14,15,2,3,4,5,6,7,8,9)]

df1$City <- as.factor(df1$City) 
df1$State <- as.factor(df1$State) 
df1$Year <- as.factor(df1$Year) 
df1$Month <- as.factor(df1$Month) 
df1$Season <- as.factor(df1$Season) 

summary(df1)

# Merging Population data
df2 <- read.csv("population.csv")
df2 <- df2[,c(1,2,4,5)]

summary(df2)
df2$City <- as.factor(df2$City)
df2$Region <- as.factor(df2$Region)
df2$Year <- as.factor(df2$Year)
df2$Population <- as.numeric(df2$Population)
summary(df2)

df1 <- merge(df1,df2,by=c("City", "Year"))
df1$Region <- as.factor(df1$Region)

# Cleaning Thermal Energy Data
df3 <- read.csv("Thermal.csv")

df3$Year <- substring(df3$Date,1,4)
df3$Year <- as.numeric(df3$Year)
df3$Year <- as.factor(df3$Year)
levels(df3$Year)

df3 <- df3[,c(3,10,4)]
df3 <- df3 %>% rename(Thermal = Thermal.Generation.Actual..in.MU.)
df3$Thermal <- as.numeric(df3$Thermal)
df3$Region <- as.factor(df3$Region)
df3$Year <- as.factor(df3$Year)

summary(df3)

# NA Imputation

df3 <- df3 %>% group_by(Region, Year) %>%
  mutate(Thermal=ifelse(is.na(Thermal),mean(Thermal,na.rm=TRUE),Thermal))
df3 <- df3 %>% group_by(Region) %>%
  mutate(Thermal=ifelse(is.na(Thermal),mean(Thermal,na.rm=TRUE),Thermal))
summary(df3)

df3 <-  aggregate(df3[, c(3)], list(df3$Region, df3$Year), mean)
df3 <- df3 %>% rename(Region = Group.1, Year = Group.2)
summary(df3)

# Merging thermal data
df1 <- merge(df1,df3,by=c("Region", "Year"))
summary(df1)


# Rearranging Columns
df1 <- df1[,c(3,4,1,2,5,6,17,18,7,8,9,10,11,12,13,14,15,16)]
summary(df1)

# Writing the final CSV
write.csv(df1, "Final.csv", row.names = FALSE)





