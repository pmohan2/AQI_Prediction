setwd("/Users/praveenmohan/Desktop/Fall2020/IE500/untitled folder/ViZ")

library(dplyr)
library(Hmisc)
library(ggplot2)
library(ggridges)



df <- read.csv("Final.csv")
df$City <- as.factor(df$City)

df1 <- aggregate(df[, 7:18], list(df$City, df$State, df$Region, df$Year, df$Month, df$Season), mean)
df1 <- df1 %>% rename(City = Group.1, State = Group.2, Region = Group.3, Year = Group.4,
                      Month = Group.5, Season = Group.6)

# AQI Trend
df2 <- aggregate(df[, 7:18], list(df$Year, df$Month), mean)
df2 <- df2 %>% rename(Year = Group.1,
                      Month = Group.2)
df2$Year <- as.factor(df2$Year)
df2$Month <- as.factor(df2$Month)

set.seed(45)
ggplot(data = df2, aes(x=Month, y=AQI, group=Year, color=Year)) + geom_line() + geom_point() + ggtitle("Air Quality Index Trend")

df3 <- aggregate(df[, 7:18], list(df$City), mean)
df3 <- df3 %>% rename(City = Group.1)

ggplot(data=df3, aes(x=reorder(City, AQI), y = AQI)) + geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()+ coord_flip() + xlab("City") + ylab("AQI") + labs(title = "Air Quality Index" )

ggplot(df, aes(x=Season, y=AQI, fill=Season)) +
  geom_violin(trim=FALSE) + labs(title = "Air Quality Index - Season Wise" )

ggplot(data=df3, aes(x=reorder(City, Population), y = Population)) + geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()+ coord_flip() + xlab("City") + ylab("Population") + labs(title = "Citywise Population" )

ggplot(data=df3, aes(x=reorder(City, Thermal), y = Thermal)) + geom_bar(stat="identity", fill="steelblue") + 
  theme_minimal()+ coord_flip() + xlab("City") + ylab("Thermal") + labs(title = "Citywise Thermal Power Generation (Mega Unit)")

# Pollutants 
ggplot(data = df2, aes(x=Month, y=Year, fill=AQI)) + geom_tile() + labs(title = "Air Quality Index") +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow") + theme_minimal()

ggplot(data = df2, aes(x=Month, y=Year, fill=CO)) + geom_tile() + labs(title = "Carbon Monoxide (CO)") +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow") + theme_minimal()

ggplot(data = df2, aes(x=Month, y=Year, fill=PM)) + geom_tile() + labs(title = "Particulate Matter (PM)") +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow") + theme_minimal()

ggplot(data = df2, aes(x=Month, y=Year, fill=BTX)) + geom_tile() + labs(title = "Benzene, Toluene, Xylene (BTX)") +
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow") + theme_minimal()

# Region vs Season
df4 <- aggregate(df[, 7:18], list(df$Region, df$Season), mean)
df4 <- df4 %>% rename(Region = Group.1, Season = Group.2)

ggplot(data = df4, aes(x=Season, y=Region, fill=AQI)) + geom_tile() + labs(title = "Air Quality Index (AQI)") +
  scale_fill_gradient2(low = "white", high = "green", mid = "white") + theme_minimal()

df$Year <- as.factor(df$Year)
dens_plot <- ggplot(df, aes(x = Thermal, y = Year, fill = Year)) + geom_density_ridges() + 
  ggtitle("Thermal Power Generation (MU) vs Year") 
dens_plot

dens_plot2 <- ggplot(df, aes(x = Thermal, y = Region, fill = Region)) + geom_density_ridges() + 
  ggtitle("Thermal Power Generation (MU) vs Region") 
dens_plot2

dens_plot3 <- ggplot(df, aes(x = Thermal, y = State, fill = State)) + geom_density_ridges() + 
  ggtitle("Thermal Power Generation (MU) vs State") 
dens_plot3

# Correlation Plot
corr <- cor(df[,c(7:18)])
heatmap(x = corr, symm = TRUE, main = "Correlation Heatmap", Colv=NA, Rowv=NA)
