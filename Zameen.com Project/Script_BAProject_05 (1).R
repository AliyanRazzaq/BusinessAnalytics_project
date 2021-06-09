#
# The first 1000 lines show case one example of the code we ran for each city
# The next 2000 lines are termed the Misc code and they are for the other two cities. 
# This was done so that all the analysis can be covered in the first 1000 lines
#

#########################################################################
# The analysis are separated using the following separation for clarity
#########################################################################



library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(factoextra)
library(cluster)
library(NbClust)
library(ISLR) 
library(tree) 

#############################ISLAMABAD DATA PREP ##############################
Isb <- read.csv("PropertyPortal-islamabad.csv", header = T)

Isb <- Isb[,c(-1,-2,-3,-4)]

Isb$Price <- substr(Isb$Price,7,20)

#Isb$Price <- sub("PKR \xa0", "", Isb$Price)

Isb <- separate(
  Isb,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Isb$Number <- as.numeric(Isb$Number)

Isb$Price <- ifelse(Isb$Label == "Crore", Isb$Number*10000000, Isb$Number*100000)

#dropped number and label
Isb <- Isb[,c(-2,-3)]

Isb$Baths <- ifelse(Isb$Baths == "-", NA, Isb$Baths)

Isb <- separate(
  Isb,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Isb$Number <- as.numeric(Isb$Number)
Isb$Area <- ifelse(Isb$Label == "Kanal", Isb$Number*20, Isb$Number)

#dropped number and label
Isb <- Isb[,c(-5,-6)]

#dropped purpose
Isb <- Isb[,-5]

Isb$Bedroom.s. <- ifelse(Isb$Bedroom.s.== "-", NA, Isb$Bedroom.s.)

#removing description
Isb <- Isb[,-7]

Isb <- Isb[,c(-7,-8,-9,-10,-11,-12,-13,-14,-15)]

Isb$Price <- ordered(cut(Isb$Price, 
+ c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb$Area <- ordered(cut(Isb$Area, 
+ c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Isb))
Isb_C<- na.omit(Isb)

Isb_C <- Isb_C[,-6]

Isb_C$Price <- as.factor(Isb_C$Price)
Isb_C$Location <- as.factor(Isb_C$Location)
Isb_C$Baths<- as.factor(Isb_C$Baths)
Isb_C$Area <- as.factor(Isb_C$Area)
Isb_C$Bedroom.s. <- as.factor(Isb_C$Bedroom.s.)
#Isb_C$Added <- as.factor(Isb_C$Added)

#############################ISLAMABAD DATA PREP ENDS ##############################

#############################LAHORE DATA PREP ##############################

Lhr <- read.csv("PropertyPortal-lahore.csv", header = T)

Lhr <- Lhr[,c(-1,-2,-3,-4)]

Lhr$Price <- substr(Lhr$Price,7,20)

Lhr <- separate(
  Lhr,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Lhr$Number <- as.numeric(Lhr$Number)

Lhr$Price <- ifelse(Lhr$Label == "Crore", Lhr$Number*10000000, Lhr$Number*100000)

#dropped number and label
Lhr <- Lhr[,c(-2,-3)]

Lhr$Baths <- ifelse(Lhr$Baths == "-", NA, Lhr$Baths)

Lhr <- separate(
  Lhr,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Lhr$Number <- as.numeric(Lhr$Number)
Lhr$Area <- ifelse(Lhr$Label == "Kanal", Lhr$Number*20, Lhr$Number)

#dropped number and label
Lhr <- Lhr[,c(-5,-6)]

#dropped purpose
Lhr <- Lhr[,-5]

Lhr$Bedroom.s. <- ifelse(Lhr$Bedroom.s.== "-", NA, Lhr$Bedroom.s.)

#removing description
Lhr <- Lhr[,-7]

Lhr <- Lhr[,c(-7,-8,-9,-10,-11,-12,-13,-14,-15)]
summary(Lhr$Price)
Lhr$Price <- ordered(cut(Lhr$Price, 
                         + c(0, 16500000, 29000000, 45000000, 390000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))
summary(Lhr$Area)
Lhr$Area <- ordered(cut(Lhr$Area, 
                        + c(0, 8, 10, 20, 120)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Lhr))
Lhr_C<- na.omit(Lhr)

Lhr_C <- Lhr_C[,-6]

Lhr_C$Price <- as.factor(Lhr_C$Price)
Lhr_C$Location <- as.factor(Lhr_C$Location)
Lhr_C$Baths<- as.factor(Lhr_C$Baths)
Lhr_C$Area <- as.factor(Lhr_C$Area)
Lhr_C$Bedroom.s. <- as.factor(Lhr_C$Bedroom.s.)
#Lhr_C$Added <- as.factor(Lhr_C$Added)

#############################LAHORE DATA PREP ENDS ##############################

#############################KARACHI DATA PREP ##############################

Khi <- read.csv("PropertyPortal-karachi.csv", header = T)

Khi <- Khi[,c(-1,-2,-3,-4)]

Khi$Price <- substr(Khi$Price,7,20)

Khi <- separate(
  Khi,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Khi$Number <- as.numeric(Khi$Number)

Khi$Price <- ifelse(Khi$Label == "Crore", Khi$Number*10000000, Khi$Number*100000)

#dropped number and label
Khi <- Khi[,c(-2,-3)]

Khi$Baths <- ifelse(Khi$Baths == "-", NA, Khi$Baths)

Khi <- separate(
  Khi,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Khi$Number <- as.numeric(Khi$Number)
Khi$Area <- ifelse(Khi$Label == "Kanal", Khi$Number*20, Khi$Number)

#dropped number and label
Khi <- Khi[,c(-5,-6)]

#dropped purpose
Khi <- Khi[,-5]

Khi$Bedroom.s. <- ifelse(Khi$Bedroom.s.== "-", NA, Khi$Bedroom.s.)

#removing description
Khi <- Khi[,-7]

Khi <- Khi[,c(-7,-8,-9,-10,-11,-12,-13,-14,-15)]
summary(Khi$Price)
Khi$Price <- ordered(cut(Khi$Price, 
                         + c(0, 13000000, 20500000, 66000000, 550000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))
summary(Khi$Area)
Khi$Area <- ordered(cut(Khi$Area, 
                        + c(0, 6.10, 8, 17.10, 200)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Khi))
Khi_C<- na.omit(Khi)

Khi_C <- Khi_C[,-6]

Khi_C$Price <- as.factor(Khi_C$Price)
Khi_C$Location <- as.factor(Khi_C$Location)
Khi_C$Baths<- as.factor(Khi_C$Baths)
Khi_C$Area <- as.factor(Khi_C$Area)
Khi_C$Bedroom.s. <- as.factor(Khi_C$Bedroom.s.)
#Khi_C$Added <- as.factor(Khi_C$Added)


#############################KARACHI DATA PREP ENDS##############################

############################# GG PLOT ANALYSIS ##################################

# For Islamabad

colSums(is.na(Isb))
Isb_C<- na.omit(Isb)

Isb_C$Location <- as.factor(Isb_C$Location)

Ordered_Isb <- Isb_C %>% group_by(Location) %>% summarise(Freq=n()) %>% arrange(desc(Freq))

Ordered_Isb <- filter(Ordered_Isb, Freq > 30)

Isb_subset <- filter(Isb_C, Location == "Bahria Town, Islamabad, Islamabad Capital"| 
                       Location == "G-13, Islamabad, Islamabad Capital"| 
                       Location == "DHA Defence, Islamabad, Islamabad Capital"| 
                       Location == "I-8, Islamabad, Islamabad Capital" | 
                       Location == "E-11, Islamabad, Islamabad Capital"| 
                       Location == "Soan Garden, Islamabad, Islamabad Capital"| 
                       Location == "Ghauri Town, Islamabad, Islamabad Capital" | 
                       Location == "F-10, Islamabad, Islamabad Capital" | 
                       Location == "F-11, Islamabad, Islamabad Capital" | 
                       Location == "CBR Town, Islamabad, Islamabad Capital" )

remove(Ordered_Isb)

Isb_subset$Location <- as.character(Isb_subset$Location)
Isb_subset$Location <- as.factor(Isb_subset$Location)

ggplot(Isb_subset, aes(x = Location, y = Price, color = Location)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Isb_subset, aes(x = Location, y = Area, color = Location)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Isb_subset, aes(x = Price, y = Area, color = Location)) +
  geom_point() +
  scale_y_continuous(n.breaks = 10)

Isb_subset$Baths <- as.numeric(Isb_subset$Baths)
Isb_subset$Bedroom.s.<- as.numeric(Isb_subset$Bedroom.s.)

Isb_subset$Baths <- as.factor(Isb_subset$Baths)
Isb_subset$Bedroom.s.<- as.factor(Isb_subset$Bedroom.s.)

ggplot(Isb_subset, aes(x = Bedroom.s., y = Price, color = Bedroom.s.)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

ggplot(Isb_subset, aes(x = Baths, y = Price, color = Baths)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

Isb_subset$Popular<- as.factor(Isb_subset$Popular)

ggplot(Isb_subset, aes(x = Popular, y = Price, color = Popular)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)


#Lahore Begins

colSums(is.na(Lhr))
Lhr_C<- na.omit(Lhr)



Lhr_C$Location <- as.factor(Lhr_C$Location)

Ordered_Lhr <- Lhr_C %>% group_by(Location) %>% summarise(Freq=n()) %>% arrange(desc(Freq))

Ordered_Lhr <- filter(Ordered_Lhr, Freq > 5)

Lhr_subset <- filter(Lhr_C, Location == "DHA Defence, Lahore, Punjab"| 
                       Location == "Bahria Town, Lahore, Punjab"| 
                       Location == "State Life Housing Society, Lahore, Punjab"| 
                       Location == "Wapda Town, Lahore, Punjab" | 
                       Location == "Johar Town, Lahore, Punjab"| 
                       Location == "Valencia Housing Society, Lahore, Punjab"| 
                       Location == "Pak Arab Housing Society, Lahore, Punjab" | 
                       Location == "Tariq Gardens, Lahore, Punjab" | 
                       Location == "Askari, Lahore, Punjab" | 
                       Location == "Rehan Garden, Lahore, Punjab" )

remove(Ordered_Lhr)

Lhr_subset$Location <- as.character(Lhr_subset$Location)
Lhr_subset$Location <- as.factor(Lhr_subset$Location)

ggplot(Lhr_subset, aes(x = Location, y = Price, color = Location)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Lhr_subset, aes(x = Location, y = Area, color = Location)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Lhr_subset, aes(x = Price, y = Area, color = Location)) +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  scale_y_log10() + scale_x_log10()

Lhr_subset$Baths <- as.numeric(Lhr_subset$Baths)
Lhr_subset$Bedroom.s.<- as.numeric(Lhr_subset$Bedroom.s.)

Lhr_subset$Baths <- as.factor(Lhr_subset$Baths)
Lhr_subset$Bedroom.s.<- as.factor(Lhr_subset$Bedroom.s.)

ggplot(Lhr_subset, aes(x = Bedroom.s., y = Price, color = Bedroom.s.)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

ggplot(Lhr_subset, aes(x = Baths, y = Price, color = Baths)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

Lhr_subset$Popular<- as.factor(Lhr_subset$Popular)

ggplot(Lhr_subset, aes(x = Popular, y = Price, color = Popular)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)


# For Karachi

colSums(is.na(Khi))
Khi_C<- na.omit(Khi)


Khi_C$Location <- as.factor(Khi_C$Location)

Ordered_Khi <- Khi_C %>% group_by(Location) %>% summarise(Freq=n()) %>% arrange(desc(Freq))

Ordered_Khi <- filter(Ordered_Khi, Freq > 20)

Khi_subset <- filter(Khi_C, Location == "Bahria Town Karachi, Karachi, Sindh"| 
                       Location == "DHA Defence, Karachi, Sindh"| 
                       Location == "Gadap Town, Karachi, Sindh"| 
                       Location == "Gulistan-e-Jauhar, Karachi, Sindh" | 
                       Location == "Malir, Karachi, Sindh"| 
                       Location == "Cantt, Karachi, Sindh"| 
                       Location == "Scheme 33, Karachi, Sindh" | 
                       Location == "North Nazimabad, Karachi, Sindh" | 
                       Location == "Gulshan-e-Iqbal Town, Karachi, Sindh" | 
                       Location == "North Karachi, Karachi, Sindh" )

remove(Ordered_Khi)

Khi_subset$Location <- as.character(Khi_subset$Location)
Khi_subset$Location <- as.factor(Khi_subset$Location)

ggplot(Khi_subset, aes(x = Location, y = Price, color = Location)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Khi_subset, aes(x = Location, y = Area, color = Location)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Khi_subset, aes(x = Price, y = Area, color = Location)) +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  scale_x_log10() + scale_y_log10()

Khi_subset$Baths <- as.numeric(Khi_subset$Baths)
Khi_subset$Bedroom.s.<- as.numeric(Khi_subset$Bedroom.s.)

Khi_subset$Baths <- as.factor(Khi_subset$Baths)
Khi_subset$Bedroom.s.<- as.factor(Khi_subset$Bedroom.s.)

ggplot(Khi_subset, aes(x = Bedroom.s., y = Price, color = Bedroom.s.)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

ggplot(Khi_subset, aes(x = Baths, y = Price, color = Baths)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

Khi_subset$Popular<- as.factor(Khi_subset$Popular)

ggplot(Khi_subset, aes(x = Popular, y = Price, color = Popular)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

############################# GG PLOT ANALYSIS ENDS ##################################

############################## K MEANS CLUSTERING ######################################

# Just showing Islamabad, other cities are found in Misc code below 
Isb <- Isb[,-6]

dev.off()

Isb_Cluster_all <- Isb


Isb_Cluster_all$Baths <- as.numeric(Isb_Cluster_all$Baths)
Isb_Cluster_all$Bedroom.s. <- as.numeric(Isb_Cluster_all$Bedroom.s.)

colSums(is.na(Isb_Cluster_all))
Isb_Cluster_all<- na.omit(Isb_Cluster_all)

#Isb_Cluster <- Isb_Cluster_all[,-2]
Isb_Cluster <- data.frame(scale(Isb_Cluster_all[,-2]))

nb<- NbClust(Isb_Cluster, distance = "euclidean", min.nc = 2,max.nc=10, method = "kmeans", index ="all")

fviz_nbclust(nb)

kout3 <- kmeans(Isb_Cluster, centers = 3, nstart = 10) 
kout3
kout3$size
table(kout3$size)

table(Isb_Cluster_all$Location, kout3$cluster)

library(ggplot2)
A <- ggplot(data=Isb_Cluster, aes(Price, Area))
A+geom_point(aes(col=factor(kout3$cluster))) + scale_y_log10()

Price.c <- tapply(Isb_Cluster$Price,kout3$cluster, mean)
Area.c <- tapply(Isb_Cluster$Area,kout3$cluster, mean)
centers <- data.frame(Price.c,Area.c)

fviz_cluster(kout3,Isb_Cluster, geom=c("point","text"))

remove(kout3)

############################## K MEANS CLUSTERING ENDS ######################################

################################## A RULES BEGIN ##########################################

#ISLAMABAD
Isb_T <- as(Isb_C,"transactions")

inspect(Isb_T[10:14])

Grule <- apriori(Isb_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Isb_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Area=Extra Large", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

#LAHORE
Lhr_T <- as(Lhr_C,"transactions")

inspect(Lhr_T[10:14])

Grule <- apriori(Lhr_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()

Grule <- apriori(Lhr_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Area=Extra Large", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
Grule
plot(Grule)
inspect(Grule[1:10])

#KARACHI
Khi_T <- as(Khi_C,"transactions")

inspect(Khi_T[10:14])

Grule <- apriori(Khi_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()

Grule <- apriori(Khi_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Price=Very High Price", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
Grule
plot(Grule)
inspect(Grule[1:10])


################################## A RULES ENDS ##########################################

################################## A RULES 2.0 BEGINS ##########################################

# Changing the cols

# Only showing Lahore

Lhr_2 <- Lhr

Lhr_2 <- Lhr_2[,c(-12,-13,-14,-15)]

Lhr_2$Healthcare.Recreational<- as.factor(Lhr_2$Healthcare.Recreational)

summary(Lhr_2$Main.Features)

Lhr_2 <- separate(
  Lhr_2,
  Healthcare.Recreational,
  into = c("A","B","C","D","E"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)

Lhr_2$A <- as.factor(Lhr_2$A)
Lhr_2$B <- as.factor(Lhr_2$B)
Lhr_2$C <- as.factor(Lhr_2$C)
Lhr_2$D <- as.factor(Lhr_2$D)
Lhr_2$E <- as.factor(Lhr_2$E)

summary(Lhr_2[,11:15])

Lhr_2$`Lawn or Garden` <- ifelse(Lhr_2$A == " Lawn or Garden", 1,0) 
Lhr_2$`Jacuzzi` <- ifelse(Lhr_2$A == " Jacuzzi" | Lhr_2$B == "Jacuzzi" | Lhr_2$C == "Jacuzzi"| Lhr_2$D == "Jacuzzi", 1,0) 
Lhr_2$`Swimming Pool` <- ifelse(Lhr_2$A == " Swimming Pool" | Lhr_2$B == "Swimming Pool" | Lhr_2$C == "Swimming Pool"| Lhr_2$D == "Swimming Pool", 1,0) 
Lhr_2$`Sauna` <- ifelse(Lhr_2$A == " Sauna" | Lhr_2$B == "Sauna" | Lhr_2$C == "Sauna"| Lhr_2$D == "Sauna", 1,0) 
Lhr_2$`Other Healthcare and Recreation Facilities` <- ifelse(Lhr_2$A == " Other Healthcare and Recreation Facilities" | Lhr_2$B == "Other Healthcare and Recreation Facilities" | Lhr_2$C == "Other Healthcare and Recreation Facilities"| Lhr_2$D == "Other Healthcare and Recreation Facilities" | Lhr_2$E == "Other Healthcare and Recreation Facilities", 1,0) 


Lhr_2$`Lawn or Garden` <- as.factor(Lhr_2$`Lawn or Garden`)
Lhr_2$Jacuzzi <- as.factor(Lhr_2$Jacuzzi)
Lhr_2$`Swimming Pool` <- as.factor(Lhr_2$`Swimming Pool`)
Lhr_2$Sauna <- as.factor(Lhr_2$Sauna)
Lhr_2$`Other Healthcare and Recreation Facilities` <- as.factor(Lhr_2$`Other Healthcare and Recreation Facilities`)

Lhr_2$Jacuzzi[is.na(Lhr_2$Jacuzzi)] = 0
Lhr_2$`Swimming Pool`[is.na(Lhr_2$`Swimming Pool`)] = 0
Lhr_2$Sauna[is.na(Lhr_2$Sauna)] = 0

Lhr_2$`Other Healthcare and Recreation Facilities` <- as.character(Lhr_2$`Other Healthcare and Recreation Facilities`)
Lhr_2$`Other Healthcare and Recreation Facilities`[is.na(Lhr_2$`Other Healthcare and Recreation Facilities`)] = 0
Lhr_2$`Other Healthcare and Recreation Facilities` <- as.factor(Lhr_2$`Other Healthcare and Recreation Facilities`)



Lhr_2 <- Lhr_2[,c(1,2,3,4,5,17,18,19,20,21)]

Lhr_Class2 <- Lhr_2

Lhr_Class <- Lhr

Lhr_Class <- Lhr[,c(-7,-8,-9,-11,-12,-13,-14,-15)]


Lhr_Class$Price <- ordered(cut(Lhr_Class$Price, 
                               + c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Lhr_Class$Area <- ordered(cut(Lhr_Class$Area, 
                              + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

Lhr_Class$Baths <- as.factor(Lhr_Class$Baths)
Lhr_Class$Bedroom.s.<- as.factor(Lhr_Class$Bedroom.s.)

Lhr_Class$Healthcare.Recreational <- ifelse(Lhr_Class$Healthcare.Recreational == " ", NA, Lhr_Class$Healthcare.Recreational)

colSums(is.na(Lhr_Class))
Lhr_Class<- na.omit(Lhr_Class)

Lhr_Class <- Lhr_Class[,-2]

Lhr_Class <- Lhr_Class[,-5]

Lhr_Class <- Lhr_Class[,c(-9,-6,-7,-8)]

Lhr_Class$`Lawn or Garden` <- ifelse(Lhr_Class$`Lawn or Garden` == 1, "Yes","No")
Lhr_Class$`Lawn or Garden` <- as.factor(Lhr_Class$`Lawn or Garden`)


#LAHORE A rules

Lhr <- Lhr_Class2

Lhr$Price <- ordered(cut(Lhr$Price, 
                         + c(0, 16500000, 29000000, 45000000, 390000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Lhr$Area <- ordered(cut(Lhr$Area, 
                        + c(0,8, 10, 20, 120)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Lhr))
Lhr_C<- na.omit(Lhr)

Lhr_C <- Lhr_C[,-6]

Lhr_C$Price <- as.factor(Lhr_C$Price)
Lhr_C$Location <- as.factor(Lhr_C$Location)
Lhr_C$Baths<- as.factor(Lhr_C$Baths)
Lhr_C$Area <- as.factor(Lhr_C$Area)
Lhr_C$Bedroom.s. <- as.factor(Lhr_C$Bedroom.s.)
#Lhr_C$Added <- as.factor(Lhr_C$Added)

Lhr_T <- as(Lhr_C,"transactions")

inspect(Lhr_T[10:14])

Grule <- apriori(Lhr_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Lhr_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Other Healthcare and Recreation Facilities=1", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
plot (Grule)
inspect(Grule[1:10])

################################## A RULES 2.0 ENDS ##########################################

####################Predictive Classification #########################################

# Just for Islamabad

Isb_Class <- Isb

Isb_Class <- Isb_Class[,-6]

Isb_Class <- Isb_Class[,-2]

summary(Isb_Class$Price)
summary(Isb_Class$Area)

Isb_Class$Price <- ordered(cut(Isb_Class$Price, 
+ c(0, 16500000    , 27000000      , 47500000   , 850000000   )), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb_Class$Area <- ordered(cut(Isb_Class$Area, 
+ c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Isb_Class))
Isb_Class<- na.omit(Isb_Class)

set.seed(234) 

Isb_Class$Location <- as.factor(Isb_Class$Location)

#taking 65% observations in dataset for training
1147*.65

Isb_Class$Price <- as.character(Isb_Class$Price)

Isb_Class$Price <- as.factor(Isb_Class$Price)

Pprice <- as.factor(Isb_Class[,1])

train <- sample(1:nrow(Isb_Class), 746) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_price <- Pprice[test]

testing_price <- as.factor(testing_price)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Price~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 5)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxx AREA xxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
1147*.65

Isb_Class$Area <- as.character(Isb_Class$Area)

Isb_Class$Area <- as.factor(Isb_Class$Area)


PArea <- as.factor(Isb_Class[,3])

train <- sample(1:nrow(Isb_Class), 745) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_area <- PArea[test]

testing_area <- as.factor(testing_area)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_area)
mean(tree_pred == testing_area)
table(testing_area, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 4)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_area)
mean(tree_pred == testing_area)
table(testing_area, tree_pred)

############################Predictive Ends####################################

############################# SENTIMENT ANALYSIS ##############################

# COMBINED
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")


# Read the text file from local machine , choose file interactively
text <- read.csv("PropertyPortal-islamabad.csv", header = T)
# Load the data as a corpus

text <- Combined[,12]
TextDoc <- Corpus(VectorSource(text))


#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
#TextDoc <- tm_map(TextDoc, stemDocument)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

dev.off()
#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("good","work","health"), corlimit = 0.25)

# Find associations for words that occur at least 50 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

d$overall <- (d$positive - d$negative)

Combined$Description.sentiscore <- d$overall

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

dev.off()

#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)


Combined <- Combined[,c(-2,-3,-4)]


Combined$Price <- substr(Combined$Price,7,20)

Combined <- separate(
  Combined,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Combined$Number <- as.numeric(Combined$Number)

Combined$Price <- ifelse(Combined$Label == "Crore", Combined$Number*10000000, Combined$Number*100000)

#dropped number and label
Combined <- Combined[,c(-3,-4)]

Combined$Baths <- ifelse(Combined$Baths == "-", NA, Combined$Baths)

Combined <- separate(
  Combined,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Combined$Number <- as.numeric(Combined$Number)
Combined$Area <- ifelse(Combined$Label == "Kanal", Combined$Number*20, Combined$Number)

#dropped number and label
Combined <- Combined[,c(-6,-7)]

Combined$Bedroom.s. <- ifelse(Combined$Bedroom.s.== "-", NA, Combined$Bedroom.s.)

summary(Combined)

#Combined$Price <- ordered(cut(Combined$Price, 
#+ c(0, 14500000, 26000000, 48500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

#Combined$Area <- ordered(cut(Combined$Area, 
#+ c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Combined))
Combined_C<- na.omit(Combined)


Combined_C$Location <- as.factor(Combined_C$Location)

Ordered_Combined <- Combined_C %>% group_by(Location) %>% summarise(Freq=n()) %>% arrange(desc(Freq))

Ordered_Combined <- filter(Ordered_Combined, Freq > 50)

Combined_C <- filter(Combined_C, Location == "DHA Defence, Lahore, Punjab"| 
                       Location == "Bahria Town Karachi, Karachi, Sindh"| 
                       Location == "Bahria Town, Lahore, Punjab"| 
                       Location == "DHA Defence, Karachi, Sindh" | 
                       Location == "Bahria Town, Islamabad, Islamabad Capital"| 
                       Location == "G-13, Islamabad, Islamabad Capital"| 
                       Location == "DHA Defence, Islamabad, Islamabad Capital" | 
                       Location == "I-8, Islamabad, Islamabad Capital" | 
                       Location == "State Life Housing Society, Lahore, Punjab" | 
                       Location == "Gadap Town, Karachi, Sindh" )

remove(Ordered_Combined)

Combined_C$Location <- as.character(Combined_C$Location)
Combined_C$Location <- as.factor(Combined_C$Location)

dev.off()

ggplot(Combined_C, aes(x = Description.sentiscore, y = Price, color = Location)) +
  geom_jitter() +
  scale_y_continuous(n.breaks = 10) +
  scale_y_log10() +scale_x_log10()

ggplot(Combined_C, aes(x = Description.sentiscore, y = Area, color = Location)) +
  geom_jitter() +
  scale_y_continuous(n.breaks = 10) +
  scale_y_log10() +scale_x_log10()

ggplot(Combined_C, aes(x = Price, y = Area, color = Location)) +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank()) +
  scale_y_log10() +scale_x_log10()

Combined_C$Baths <- as.numeric(Combined_C$Baths)
Combined_C$Bedroom.s.<- as.numeric(Combined_C$Bedroom.s.)

Combined_C$Baths <- as.factor(Combined_C$Baths)
Combined_C$Bedroom.s.<- as.factor(Combined_C$Bedroom.s.)

Combined_C$Popular<- as.factor(Combined_C$Popular)

ggplot(Combined_C, aes(x = Popular, y = Description.sentiscore, color = Popular)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

############################# SENTIMENT ANALYSIS ENDS ##############################


################################# MISC CODE #########################################

Isb_2 <- Isb

Isb_2 <- Isb_2[,c(-12,-13,-14,-15)]

Isb_2$Healthcare.Recreational<- as.factor(Isb_2$Healthcare.Recreational)

Isb_2 <- separate(
  Isb_2,
  Healthcare.Recreational,
  into = c("A","B","C","D","E"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)

Isb_2$A <- as.factor(Isb_2$A)
Isb_2$B <- as.factor(Isb_2$B)
Isb_2$C <- as.factor(Isb_2$C)
Isb_2$D <- as.factor(Isb_2$D)
Isb_2$E <- as.factor(Isb_2$E)

summary(Isb_2[,11:15])

Isb_2$`Lawn or Garden` <- ifelse(Isb_2$A == " Lawn or Garden", 1,0) 
Isb_2$`Jacuzzi` <- ifelse(Isb_2$A == " Jacuzzi" | Isb_2$B == "Jacuzzi" | Isb_2$C == "Jacuzzi"| Isb_2$D == "Jacuzzi", 1,0) 
Isb_2$`Swimming Pool` <- ifelse(Isb_2$A == " Swimming Pool" | Isb_2$B == "Swimming Pool" | Isb_2$C == "Swimming Pool"| Isb_2$D == "Swimming Pool", 1,0) 
Isb_2$`Sauna` <- ifelse(Isb_2$A == " Sauna" | Isb_2$B == "Sauna" | Isb_2$C == "Sauna"| Isb_2$D == "Sauna", 1,0) 
Isb_2$`Other Healthcare and Recreation Facilities` <- ifelse(Isb_2$A == " Other Healthcare and Recreation Facilities" | Isb_2$B == "Other Healthcare and Recreation Facilities" | Isb_2$C == "Other Healthcare and Recreation Facilities"| Isb_2$D == "Other Healthcare and Recreation Facilities" | Isb_2$E == "Other Healthcare and Recreation Facilities", 1,0) 


Isb_2$`Lawn or Garden` <- as.factor(Isb_2$`Lawn or Garden`)
Isb_2$Jacuzzi <- as.factor(Isb_2$Jacuzzi)
Isb_2$`Swimming Pool` <- as.factor(Isb_2$`Swimming Pool`)
Isb_2$Sauna <- as.factor(Isb_2$Sauna)
Isb_2$`Other Healthcare and Recreation Facilities` <- as.factor(Isb_2$`Other Healthcare and Recreation Facilities`)

Isb_2$Jacuzzi[is.na(Isb_2$Jacuzzi)] = 0
Isb_2$`Swimming Pool`[is.na(Isb_2$`Swimming Pool`)] = 0
Isb_2$Sauna[is.na(Isb_2$Sauna)] = 0

Isb_2$`Other Healthcare and Recreation Facilities` <- as.character(Isb_2$`Other Healthcare and Recreation Facilities`)
Isb_2$`Other Healthcare and Recreation Facilities`[is.na(Isb_2$`Other Healthcare and Recreation Facilities`)] = 0
Isb_2$`Other Healthcare and Recreation Facilities` <- as.factor(Isb_2$`Other Healthcare and Recreation Facilities`)



Isb_2 <- Isb_2[,c(1,2,3,4,5,17,18,19,20,21)]

Isb_Class2 <- Isb_2

Isb_Class <- Isb

Isb_Class <- Isb[,c(-7,-8,-9,-11,-12,-13,-14,-15)]


Isb_Class$Price <- ordered(cut(Isb_Class$Price, 
+ c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb_Class$Area <- ordered(cut(Isb_Class$Area, 
+ c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

Isb_Class$Baths <- as.factor(Isb_Class$Baths)
Isb_Class$Bedroom.s.<- as.factor(Isb_Class$Bedroom.s.)

Isb_Class$Healthcare.Recreational <- ifelse(Isb_Class$Healthcare.Recreational == " ", NA, Isb_Class$Healthcare.Recreational)

colSums(is.na(Isb_Class))
Isb_Class<- na.omit(Isb_Class)

Isb_Class <- Isb_Class[,-2]

Isb_Class <- Isb_Class[,-5]

Isb_Class <- Isb_Class[,c(-9,-6,-7,-8)]

Isb_Class$`Lawn or Garden` <- ifelse(Isb_Class$`Lawn or Garden` == 1, "Yes","No")
Isb_Class$`Lawn or Garden` <- as.factor(Isb_Class$`Lawn or Garden`)

summary(Isb_Class)

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Isb_Class$Price <- as.character(Isb_Class$Price)

Isb_Class$Price <- as.factor(Isb_Class$Price)

Pprice <- as.factor(Isb_Class[,1])

train <- sample(1:nrow(Isb_Class), 337) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_price <- Pprice[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx AREA xxxxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Isb_Class$Area <- as.character(Isb_Class$Area)

Isb_Class$Area <- as.factor(Isb_Class$Area)


PArea <- as.factor(Isb_Class[,3])

train <- sample(1:nrow(Isb_Class), 337) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_price <- PArea[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)


#######################################################################

#####################################A Rules 2.0##########################

Isb <- Isb_Class2

Isb$Price <- ordered(cut(Isb$Price, 
                         + c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb$Area <- ordered(cut(Isb$Area, 
                        + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Isb))
Isb_C<- na.omit(Isb)

Isb_C <- Isb_C[,-6]

Isb_C$Price <- as.factor(Isb_C$Price)
Isb_C$Location <- as.factor(Isb_C$Location)
Isb_C$Baths<- as.factor(Isb_C$Baths)
Isb_C$Area <- as.factor(Isb_C$Area)
Isb_C$Bedroom.s. <- as.factor(Isb_C$Bedroom.s.)
#Isb_C$Added <- as.factor(Isb_C$Added)

Isb_T <- as(Isb_C,"transactions")

#inspect(Isb_T)

Grule <- apriori(Isb_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Isb_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Jacuzzi=1", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

detach(package:tm, unload=TRUE)
library(arules)
############################################################################################

###################################### Combining (Running A-rules 3.0) ###########################################

Khi <- read.csv("PropertyPortal-karachi.csv")
Khi$X <- ifelse(Khi$X == 0, "Karachi", 0) 

Isb <-read.csv("PropertyPortal-islamabad.csv")
Isb$X <- ifelse(Isb$X == 0, "Islamabad", 0) 

Lhr <-read.csv("PropertyPortal-lahore.csv")
Lhr$X <- ifelse(Lhr$X == 0, "Lahore", 0) 

Combined <-rbind(Lhr,Khi,Isb)

Combined <- Combined %>% 
  rename(City = X)

names(Combined)

Combined <- Combined[,c(-2,-3,-4,-9,-11,-12,-13,-14,-15,-16,-17,-18,-20,-21)]

Combined$Price <- substr(Combined$Price,7,20)

Combined <- separate(
  Combined,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Combined$Number <- as.numeric(Combined$Number)

Combined$Price <- ifelse(Combined$Label == "Crore", Combined$Number*10000000, Combined$Number*100000)

#dropped number and label
Combined <- Combined[,c(-3,-4)]

Combined$Baths <- ifelse(Combined$Baths == "-", NA, Combined$Baths)

Combined <- separate(
  Combined,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Combined$Number <- as.numeric(Combined$Number)
Combined$Area <- ifelse(Combined$Label == "Kanal", Combined$Number*20, Combined$Number)

#dropped number and label
Combined <- Combined[,c(-6,-7)]

Combined$Bedroom.s. <- ifelse(Combined$Bedroom.s.== "-", NA, Combined$Bedroom.s.)

summary(Combined)

Combined$Price <- ordered(cut(Combined$Price, 
+ c(0, 14500000, 26000000, 48500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))


Combined$Area <- ordered(cut(Combined$Area, 
+ c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Combined))
Combined_C<- na.omit(Combined)

Combined_C$Popular <- ifelse(Combined_C$Popular == "" , "Normal",Combined_C$Popular)

Combined_C$Popular <- as.factor((Combined_C$Popular))
summary(Combined_C)

Combined_C$City <- as.factor(Combined_C$City)
Combined_C$Price <- as.factor(Combined_C$Price)
Combined_C$Location <- as.factor(Combined_C$Location)
Combined_C$Baths<- as.factor(Combined_C$Baths)
Combined_C$Area <- as.factor(Combined_C$Area)
Combined_C$Bedroom.s. <- as.factor(Combined_C$Bedroom.s.)
#Isb_C$Added <- as.factor(Isb_C$Added)

Combined_T <- as(Combined_C,"transactions")

inspect(Combined_T[10:14])

Grule <- apriori(Combined_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Combined_T, parameter = list (support = 0.008, conf = 0.05, minlen=2), 
                 appearance = list (rhs = "Area=Extra Large", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

inspect(Grule)

#################################Combining Ends ####################


################################ Sentiment Analysis ################

# ISLAMABAD
Isb_subset$Description <- as.factor(Isb_subset$Description)

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")


# Read the text file from local machine , choose file interactively
text <- read.csv("PropertyPortal-islamabad.csv", header = T)
# Load the data as a corpus

text <- text[,12]
TextDoc <- Corpus(VectorSource(text))


#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
#TextDoc <- tm_map(TextDoc, stemDocument)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("good","work","health"), corlimit = 0.25)

# Find associations for words that occur at least 50 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

d$overall <- (d$positive - d$negative)

Isb_Senti$sentiscore <- d$overall

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

dev.off()

#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)

Isb$sentiscore <- d$overall

Isb <- Isb_Senti[,c(-1,-2,-3,-4)]

Isb$Price <- substr(Isb$Price,7,20)

Isb <- separate(
  Isb,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Isb$Number <- as.numeric(Isb$Number)

Isb$Price <- ifelse(Isb$Label == "Crore", Isb$Number*10000000, Isb$Number*100000)

#dropped number and label
Isb <- Isb[,c(-2,-3)]

Isb$Baths <- ifelse(Isb$Baths == "-", NA, Isb$Baths)

Isb <- separate(
  Isb,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Isb$Number <- as.numeric(Isb$Number)
Isb$Area <- ifelse(Isb$Label == "Kanal", Isb$Number*20, Isb$Number)

#dropped number and label
Isb <- Isb[,c(-5,-6)]

#dropped purpose
#Isb <- Isb[,-5]

Isb$Bedroom.s. <- ifelse(Isb$Bedroom.s.== "-", NA, Isb$Bedroom.s.)

#removing description
#Isb <- Isb[,-7]

names(Isb)

Isb <- Isb[,c(-5,-7,-8,-9,-10,-11,-12,-13,-14,-16,-17)]

#Isb$Price <- ordered(cut(Isb$Price, 
#+ c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

#Isb$Area <- ordered(cut(Isb$Area, 
#+ c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Isb))
Isb_C<- na.omit(Isb)



Isb_C$Location <- as.factor(Isb_C$Location)

Ordered_Isb <- Isb_C %>% group_by(Location) %>% summarise(Freq=n()) %>% arrange(desc(Freq))

Ordered_Isb <- filter(Ordered_Isb, Freq > 30)

Isb_subset <- filter(Isb_C, Location == "Bahria Town, Islamabad, Islamabad Capital"| 
                       Location == "G-13, Islamabad, Islamabad Capital"| 
                       Location == "DHA Defence, Islamabad, Islamabad Capital"| 
                       Location == "I-8, Islamabad, Islamabad Capital" | 
                       Location == "E-11, Islamabad, Islamabad Capital"| 
                       Location == "Soan Garden, Islamabad, Islamabad Capital"| 
                       Location == "Ghauri Town, Islamabad, Islamabad Capital" | 
                       Location == "F-10, Islamabad, Islamabad Capital" | 
                       Location == "F-11, Islamabad, Islamabad Capital" | 
                       Location == "CBR Town, Islamabad, Islamabad Capital" )

remove(Ordered_Isb)

Isb_subset$Location <- as.character(Isb_subset$Location)
Isb_subset$Location <- as.factor(Isb_subset$Location)

dev.off()

ggplot(Isb_subset, aes(x = sentiscore, y = Price, color = Location)) +
  geom_jitter()
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Isb_subset, aes(x = sentiscore, y = Area, color = Location)) +
  geom_jitter() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

ggplot(Isb_subset, aes(x = Price, y = Area, color = sentiscore)) +
  geom_point() +
  scale_y_continuous(n.breaks = 10) +
  theme(axis.text.x = element_blank())

Isb_subset$Baths <- as.numeric(Isb_subset$Baths)
Isb_subset$Bedroom.s.<- as.numeric(Isb_subset$Bedroom.s.)

Isb_subset$Baths <- as.factor(Isb_subset$Baths)
Isb_subset$Bedroom.s.<- as.factor(Isb_subset$Bedroom.s.)


ggplot(Isb_subset, aes(x = Baths, y = sentiscore, color = Baths)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

Isb_subset$Popular<- as.factor(Isb_subset$Popular)

ggplot(Isb_subset, aes(x = Popular, y = sentiscore, color = Popular)) +
  geom_boxplot() +
  scale_y_continuous(n.breaks = 10)

########################### Islamabad Sentiment ENDS ########################

################################## MISC 2.0 #################################

############################Starting with Lahore#################################################
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(cluster)
library(NbClust)
library(ISLR) 
library(tree) 




########################################## A rules done ###########################

###################################### HClust not possible ####
################################## Kmeans how to run ###########
summary(Lhr)

Lhr <- Lhr[,-6]

dev.off()

Lhr_Cluster_all <- Lhr


Lhr_Cluster_all$Baths <- as.numeric(Lhr_Cluster_all$Baths)
Lhr_Cluster_all$Bedroom.s. <- as.numeric(Lhr_Cluster_all$Bedroom.s.)

colSums(is.na(Lhr_Cluster_all))
Lhr_Cluster_all<- na.omit(Lhr_Cluster_all)

#Lhr_Cluster <- Lhr_Cluster_all[,-2]
Lhr_Cluster <- data.frame(scale(Lhr_Cluster_all[,-2]))

nb<- NbClust(Lhr_Cluster, distance = "euclidean", min.nc = 2,max.nc=10, method = "kmeans", index ="all")

fviz_nbclust(nb)

kout3 <- kmeans(Lhr_Cluster, centers = 2, nstart = 10) 
kout3
kout3$size
table(kout3$size)

table(Lhr_Cluster_all$Location, kout3$cluster)

library(ggplot2)
A <- ggplot(data=Lhr_Cluster, aes(Price, Area))
A+geom_point(aes(col=factor(kout3$cluster)))

Price.c <- tapply(Lhr_Cluster$Price,kout3$cluster, mean)
Area.c <- tapply(Lhr_Cluster$Area,kout3$cluster, mean)
centers <- data.frame(Price.c,Area.c)

C<- B+geom_point(data=centers,aes(Price.c,Area.c),color="red",pch=4,size=10)
C

fviz_cluster(kout3,Lhr_Cluster, geom=c("point","text"))

remove(kout3)


################## K means clustering ends ###############################

####################Predicitive Regression #########################################

Lhr_Class <- Lhr

Lhr_Class <- Lhr_Class[,-6]

Lhr_Class <- Lhr_Class[,-2]


Lhr_Class$Price <- ordered(cut(Lhr_Class$Price, 
                               + c(0, 16500000, 29000000, 45000000, 390000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Lhr_Class$Area <- ordered(cut(Lhr_Class$Area, 
                              + c(0,8, 10, 20, 120)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Lhr_Class))
Lhr_Class<- na.omit(Lhr_Class)

set.seed(234) 

Lhr_Class$Location <- as.factor(Lhr_Class$Location)

#taking 65% observations in dataset for training
1147*.65

Lhr_Class$Price <- as.character(Lhr_Class$Price)

Lhr_Class$Price <- as.factor(Lhr_Class$Price)

Pprice <- as.factor(Lhr_Class[,1])

train <- sample(1:nrow(Lhr_Class), 746) 
test <- -train
train
test
training_data <-Lhr_Class[train,]
testing_data <- Lhr_Class[test,]
testing_price <- Pprice[test]

testing_price <- as.factor(testing_price)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Price~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx AREA xxxxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
1147*.65

Lhr_Class$Area <- as.character(Lhr_Class$Area)


Lhr_Class$Area <- as.factor(Lhr_Class$Area)


PArea <- as.factor(Lhr_Class[,4])

train <- sample(1:nrow(Lhr_Class), 746) 
test <- -train
train
test
training_data <-Lhr_Class[train,]
testing_data <- Lhr_Class[test,]
testing_price <- PArea[test]

testing_price <- as.factor(testing_price)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 5)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)



############################Predictive Ends####################################


summary(Lhr_Class)

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Lhr_Class$Price <- as.character(Lhr_Class$Price)

Lhr_Class$Price <- as.factor(Lhr_Class$Price)

Pprice <- as.factor(Lhr_Class[,1])

train <- sample(1:nrow(Lhr_Class), 337) 
test <- -train
train
test
training_data <-Lhr_Class[train,]
testing_data <- Lhr_Class[test,]
testing_price <- Pprice[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx AREA xxxxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Lhr_Class$Area <- as.character(Lhr_Class$Area)

Lhr_Class$Area <- as.factor(Lhr_Class$Area)


PArea <- as.factor(Lhr_Class[,3])

train <- sample(1:nrow(Lhr_Class), 337) 
test <- -train
train
test
training_data <-Lhr_Class[train,]
testing_data <- Lhr_Class[test,]
testing_price <- PArea[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)


#######################################################################

#####################################A Rules 2.0##########################




############################################################################################
############################Starting with Karachi#################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(cluster)
library(NbClust)
library(ISLR) 
library(tree) 




########################################## A rules done ###########################

###################################### HClust not possible ####
################################## Kmeans how to run ###########
summary(Khi)

Khi <- Khi[,-6]

dev.off()

Khi_Cluster_all <- Khi


Khi_Cluster_all$Baths <- as.numeric(Khi_Cluster_all$Baths)
Khi_Cluster_all$Bedroom.s. <- as.numeric(Khi_Cluster_all$Bedroom.s.)

colSums(is.na(Khi_Cluster_all))
Khi_Cluster_all<- na.omit(Khi_Cluster_all)

#Khi_Cluster <- Khi_Cluster_all[,-2]
Khi_Cluster <- data.frame(scale(Khi_Cluster_all[,-2]))

nb<- NbClust(Khi_Cluster, distance = "euclidean", min.nc = 2,max.nc=10, method = "kmeans", index ="all")

fviz_nbclust(nb)

kout3 <- kmeans(Khi_Cluster, centers = 3, nstart = 10) 
kout3
kout3$size
table(kout3$size)

table(Khi_Cluster_all$Location, kout3$cluster)

library(ggplot2)
A <- ggplot(data=Khi_Cluster, aes(Price, Area))
A+geom_point(aes(col=factor(kout3$cluster))) + scale_y_log10()



Price.c <- tapply(Khi_Cluster$Price,kout3$cluster, mean)
Area.c <- tapply(Khi_Cluster$Area,kout3$cluster, mean)
centers <- data.frame(Price.c,Area.c)

C<- B+geom_point(data=centers,aes(Price.c,Area.c),color="red",pch=4,size=10)
C

fviz_cluster(kout3,Khi_Cluster, geom=c("point","text"))

remove(kout3)


################## K means clustering ends ###############################

####################Predicitive Regression #########################################

Khi_Class <- Khi

Khi_Class <- Khi_Class[,-6]

Khi_Class <- Khi_Class[,-2]


Khi_Class$Price <- ordered(cut(Khi_Class$Price, 
                               + c(0, 13000000, 20500000, 66000000, 550000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Khi_Class$Area <- ordered(cut(Khi_Class$Area, 
                              + c(0, 6.10, 8, 17.10, 200)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Khi_Class))
Khi_Class<- na.omit(Khi_Class)

set.seed(234) 

Khi_Class$Location <- as.factor(Khi_Class$Location)

#taking 65% observations in dataset for training
1147*.65

Khi_Class$Price <- as.character(Khi_Class$Price)

Khi_Class$Price <- as.factor(Khi_Class$Price)

Pprice <- as.factor(Khi_Class[,1])

train <- sample(1:nrow(Khi_Class), 746) 
test <- -train
train
test
training_data <-Khi_Class[train,]
testing_data <- Khi_Class[test,]
testing_price <- Pprice[test]

testing_price <- as.factor(testing_price)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Price~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx AREA xxxxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
1147*.65

Khi_Class$Area <- as.character(Khi_Class$Area)

Khi_Class$Area <- as.factor(Khi_Class$Area)


PArea <- as.factor(Khi_Class[,4])

train <- sample(1:nrow(Khi_Class), 746) 
test <- -train
train
test
training_data <-Khi_Class[train,]
testing_data <- Khi_Class[test,]
testing_price <- PArea[test]

testing_price <- as.factor(testing_price)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 5)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)



############################Predictive Ends####################################



Khi_2 <- Khi

Khi_2 <- Khi_2[,c(-12,-13,-14,-15)]

Khi_2$Healthcare.Recreational<- as.factor(Khi_2$Healthcare.Recreational)

summary(Khi_2$Main.Features)

Khi_2 <- separate(
  Khi_2,
  Healthcare.Recreational,
  into = c("A","B","C","D","E"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)

Khi_2$A <- as.factor(Khi_2$A)
Khi_2$B <- as.factor(Khi_2$B)
Khi_2$C <- as.factor(Khi_2$C)
Khi_2$D <- as.factor(Khi_2$D)
Khi_2$E <- as.factor(Khi_2$E)

summary(Khi_2[,11:15])

Khi_2$`Lawn or Garden` <- ifelse(Khi_2$A == " Lawn or Garden", 1,0) 
Khi_2$`Jacuzzi` <- ifelse(Khi_2$A == " Jacuzzi" | Khi_2$B == "Jacuzzi" | Khi_2$C == "Jacuzzi"| Khi_2$D == "Jacuzzi", 1,0) 
Khi_2$`Swimming Pool` <- ifelse(Khi_2$A == " Swimming Pool" | Khi_2$B == "Swimming Pool" | Khi_2$C == "Swimming Pool"| Khi_2$D == "Swimming Pool", 1,0) 
Khi_2$`Sauna` <- ifelse(Khi_2$A == " Sauna" | Khi_2$B == "Sauna" | Khi_2$C == "Sauna"| Khi_2$D == "Sauna", 1,0) 
Khi_2$`Other Healthcare and Recreation Facilities` <- ifelse(Khi_2$A == " Other Healthcare and Recreation Facilities" | Khi_2$B == "Other Healthcare and Recreation Facilities" | Khi_2$C == "Other Healthcare and Recreation Facilities"| Khi_2$D == "Other Healthcare and Recreation Facilities" | Khi_2$E == "Other Healthcare and Recreation Facilities", 1,0) 


Khi_2$`Lawn or Garden` <- as.factor(Khi_2$`Lawn or Garden`)
Khi_2$Jacuzzi <- as.factor(Khi_2$Jacuzzi)
Khi_2$`Swimming Pool` <- as.factor(Khi_2$`Swimming Pool`)
Khi_2$Sauna <- as.factor(Khi_2$Sauna)
Khi_2$`Other Healthcare and Recreation Facilities` <- as.factor(Khi_2$`Other Healthcare and Recreation Facilities`)

Khi_2$Jacuzzi[is.na(Khi_2$Jacuzzi)] = 0
Khi_2$`Swimming Pool`[is.na(Khi_2$`Swimming Pool`)] = 0
Khi_2$Sauna[is.na(Khi_2$Sauna)] = 0
Khi_2$`Other Healthcare and Recreation Facilities` <- as.character(Khi_2$`Other Healthcare and Recreation Facilities`)
Khi_2$`Other Healthcare and Recreation Facilities`[is.na(Khi_2$`Other Healthcare and Recreation Facilities`)] = 0
Khi_2$`Other Healthcare and Recreation Facilities` <- as.factor(Khi_2$`Other Healthcare and Recreation Facilities`)



Khi_2 <- Khi_2[,c(1,2,3,4,5,17,18,19,20,21)]

Khi_Class2 <- Khi_2

Khi_Class <- Khi

Khi_Class <- Khi[,c(-7,-8,-9,-11,-12,-13,-14,-15)]

Khi_Class$Price <- ordered(cut(Khi_Class$Price, 
                               + c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Khi_Class$Area <- ordered(cut(Khi_Class$Area, 
                              + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

Khi_Class$Baths <- as.factor(Khi_Class$Baths)
Khi_Class$Bedroom.s.<- as.factor(Khi_Class$Bedroom.s.)

Khi_Class$Healthcare.Recreational <- ifelse(Khi_Class$Healthcare.Recreational == " ", NA, Khi_Class$Healthcare.Recreational)

colSums(is.na(Khi_Class))
Khi_Class<- na.omit(Khi_Class)

Khi_Class <- Khi_Class[,-2]

Khi_Class <- Khi_Class[,-5]

Khi_Class <- Khi_Class[,c(-9,-6,-7,-8)]

Khi_Class$`Lawn or Garden` <- ifelse(Khi_Class$`Lawn or Garden` == 1, "Yes","No")
Khi_Class$`Lawn or Garden` <- as.factor(Khi_Class$`Lawn or Garden`)

summary(Khi_Class)

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Khi_Class$Price <- as.character(Khi_Class$Price)

Khi_Class$Price <- as.factor(Khi_Class$Price)

Pprice <- as.factor(Khi_Class[,1])

train <- sample(1:nrow(Khi_Class), 337) 
test <- -train
train
test
training_data <-Khi_Class[train,]
testing_data <- Khi_Class[test,]
testing_price <- Pprice[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx AREA xxxxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Khi_Class$Area <- as.character(Khi_Class$Area)

Khi_Class$Area <- as.factor(Khi_Class$Area)


PArea <- as.factor(Khi_Class[,3])

train <- sample(1:nrow(Khi_Class), 337) 
test <- -train
train
test
training_data <-Khi_Class[train,]
testing_data <- Khi_Class[test,]
testing_price <- PArea[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)


#######################################################################

#####################################A Rules 2.0##########################

Khi <- Khi_Class2

Khi$Price <- ordered(cut(Khi$Price, 
                         + c(0, 13000000, 20500000, 66000000, 550000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Khi$Area <- ordered(cut(Khi$Area, 
                        + c(0, 6.10, 8, 17.10, 200)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Khi))
Khi_C<- na.omit(Khi)

Khi_C <- Khi_C[,-6]

Khi_C$Price <- as.factor(Khi_C$Price)
Khi_C$Location <- as.factor(Khi_C$Location)
Khi_C$Baths<- as.factor(Khi_C$Baths)
Khi_C$Area <- as.factor(Khi_C$Area)
Khi_C$Bedroom.s. <- as.factor(Khi_C$Bedroom.s.)
#Khi_C$Added <- as.factor(Khi_C$Added)

Khi_T <- as(Khi_C,"transactions")

inspect(Khi_T[10:14])

Grule <- apriori(Khi_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Khi_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Other Healthcare and Recreation Facilities=1", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
Grule
plot(Grule)
inspect(Grule[1:10])


############################################################################################
####################################Starting with ISlAMABAD##########################################


library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(arules)
library(arulesViz)

library(cluster)
library(NbClust)

library(ISLR) 
library(tree) 


Isb <- read.csv("PropertyPortal-islamabad.csv", header = T)
Isb <- Isb[,c(-1,-2,-3,-4)]
Isb$Price <- substr(Isb$Price,7,20)

Isb <- separate(
  Isb,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)
Isb$Number <- as.numeric(Isb$Number)
Isb$Price <- ifelse(Isb$Label == "Crore", Isb$Number*10000000, Isb$Number*100000)

#dropped number and label
Isb <- Isb[,c(-2,-3)]

Isb$Baths <- ifelse(Isb$Baths == "-", NA, Isb$Baths)

Isb <- separate(
  Isb,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Isb$Number <- as.numeric(Isb$Number)
Isb$Area <- ifelse(Isb$Label == "Kanal", Isb$Number*20, Isb$Number)

#dropped number and label
Isb <- Isb[,c(-5,-6)]

#dropped purpose
Isb <- Isb[,-5]

Isb$Bedroom.s. <- ifelse(Isb$Bedroom.s.== "-", NA, Isb$Bedroom.s.)

#removing description
Isb <- Isb[,-7]

Isb <- Isb[,c(-7,-8,-9,-10,-11,-12,-13,-14,-15)]
#summary(Isb$Price)
Isb$Price <- ordered(cut(Isb$Price, 
                         + c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb$Area <- ordered(cut(Isb$Area, 
                        + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Isb))
Isb_C<- na.omit(Isb)
Isb_C <- Isb_C[,-6]

Isb_C$Price <- as.factor(Isb_C$Price)
Isb_C$Location <- as.factor(Isb_C$Location)
Isb_C$Baths<- as.factor(Isb_C$Baths)

Isb_C$Area <- as.factor(Isb_C$Area)
Isb_C$Bedroom.s. <- as.factor(Isb_C$Bedroom.s.)
#Isb_C$Added <- as.factor(Isb_C$Added)

Isb_T <- as(Isb_C,"transactions")

inspect(Isb_T[10:14])


Grule <- apriori(Isb_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Isb_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Price=High Price", default = "lhs"))
# changes here
Grule
Grule <- sort (Grule, by = "lift")
plot(Grule)
inspect(Grule[1:10])

########################################## A rules done ###########################

###################################### HClust not possible ####
################################## Kmeans how to run ###########
summary(Isb)

Isb <- Isb[,-6]

dev.off()

Isb_Cluster_all <- Isb


Isb_Cluster_all$Baths <- as.numeric(Isb_Cluster_all$Baths)
Isb_Cluster_all$Bedroom.s. <- as.numeric(Isb_Cluster_all$Bedroom.s.)

colSums(is.na(Isb_Cluster_all))
Isb_Cluster_all<- na.omit(Isb_Cluster_all)

#Isb_Cluster <- Isb_Cluster_all[,-2]
na.rm(Isb_Cluster_all)
Isb_Cluster <- data.frame(scale(Isb_Cluster_all[,-2]))

nb<- NbClust(Isb_Cluster, distance = "euclidean", min.nc = 2,max.nc=10, method = "kmeans", index ="all")

fviz_nbclust(nb)

kout3 <- kmeans(Isb_Cluster, centers = 3, nstart = 10) 
kout3
kout3$size
table(kout3$size)

table(Isb_Cluster_all$Location, kout3$cluster)

library(ggplot2)
A <- ggplot(data=Isb_Cluster, aes(Price, Area))
A+geom_point(aes(col=factor(kout3$cluster)))


Price.c <- tapply(Isb_Cluster$Price,kout3$cluster, mean)
Area.c <- tapply(Isb_Cluster$Area,kout3$cluster, mean)
centers <- data.frame(Price.c,Area.c)

C<- B+geom_point(data=centers,aes(Price.c,Area.c),color="red",pch=4,size=10)
C

fviz_cluster(kout3,Isb_Cluster, geom=c("point","text"))

remove(kout3)


################## K means clustering ends ###############################

####################Predicitive Regression #########################################

Isb_Class <- Isb

Isb_Class <- Isb_Class[,-6]

Isb_Class <- Isb_Class[,-2]


Isb_Class$Price <- ordered(cut(Isb_Class$Price, 
                               + c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb_Class$Area <- ordered(cut(Isb_Class$Area, 
                              + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Isb_Class))
Isb_Class<- na.omit(Isb_Class)

set.seed(234) 

Isb_Class$Location <- as.factor(Isb_Class$Location)

#taking 65% observations in dataset for training
1147*.65

Isb_Class$Price <- as.character(Isb_Class$Price)

Isb_Class$Price <- as.factor(Isb_Class$Price)

Pprice <- as.factor(Isb_Class[,1])

train <- sample(1:nrow(Isb_Class), 746) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_price <- Pprice[test]


testing_price <- as.factor(testing_price)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Price~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)


tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx AREA xxxxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
1147*.65

Isb_Class$Area <- as.character(Isb_Class$Area)

Isb_Class$Area <- as.factor(Isb_Class$Area)


PArea <- as.factor(Isb_Class[,4])

train <- sample(1:nrow(Isb_Class), 746) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_price <- PArea[test]

testing_price <- as.factor(testing_price)

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 5)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)



############################Predictive Ends####################################



Isb_2 <- Isb

Isb_2 <- Isb_2[,c(-12,-13,-14,-15)]

Isb_2$Healthcare.Recreational<- as.factor(Isb_2$Healthcare.Recreational)

summary(Isb_2$Main.Features)

Isb_2 <- separate(
  Isb_2,
  Healthcare.Recreational,
  into = c("A","B","C","D","E"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)

Isb_2$A <- as.factor(Isb_2$A)
Isb_2$B <- as.factor(Isb_2$B)
Isb_2$C <- as.factor(Isb_2$C)
Isb_2$D <- as.factor(Isb_2$D)
Isb_2$E <- as.factor(Isb_2$E)

summary(Isb_2[,11:15])

Isb_2$`Lawn or Garden` <- ifelse(Isb_2$A == " Lawn or Garden", 1,0) 
Isb_2$`Jacuzzi` <- ifelse(Isb_2$A == " Jacuzzi" | Isb_2$B == "Jacuzzi" | Isb_2$C == "Jacuzzi"| Isb_2$D == "Jacuzzi", 1,0) 
Isb_2$`Swimming Pool` <- ifelse(Isb_2$A == " Swimming Pool" | Isb_2$B == "Swimming Pool" | Isb_2$C == "Swimming Pool"| Isb_2$D == "Swimming Pool", 1,0) 
Isb_2$`Sauna` <- ifelse(Isb_2$A == " Sauna" | Isb_2$B == "Sauna" | Isb_2$C == "Sauna"| Isb_2$D == "Sauna", 1,0) 
Isb_2$`Other Healthcare and Recreation Facilities` <- ifelse(Isb_2$A == " Other Healthcare and Recreation Facilities" | Isb_2$B == "Other Healthcare and Recreation Facilities" | Isb_2$C == "Other Healthcare and Recreation Facilities"| Isb_2$D == "Other Healthcare and Recreation Facilities" | Isb_2$E == "Other Healthcare and Recreation Facilities", 1,0) 


Isb_2$`Lawn or Garden` <- as.factor(Isb_2$`Lawn or Garden`)
Isb_2$Jacuzzi <- as.factor(Isb_2$Jacuzzi)
Isb_2$`Swimming Pool` <- as.factor(Isb_2$`Swimming Pool`)
Isb_2$Sauna <- as.factor(Isb_2$Sauna)
Isb_2$`Other Healthcare and Recreation Facilities` <- as.factor(Isb_2$`Other Healthcare and Recreation Facilities`)

Isb_2$Jacuzzi[is.na(Isb_2$Jacuzzi)] = 0
Isb_2$`Swimming Pool`[is.na(Isb_2$`Swimming Pool`)] = 0
Isb_2$Sauna[is.na(Isb_2$Sauna)] = 0

Isb_2$`Other Healthcare and Recreation Facilities` <- as.character(Isb_2$`Other Healthcare and Recreation Facilities`)
Isb_2$`Other Healthcare and Recreation Facilities`[is.na(Isb_2$`Other Healthcare and Recreation Facilities`)] = 0
Isb_2$`Other Healthcare and Recreation Facilities` <- as.factor(Isb_2$`Other Healthcare and Recreation Facilities`)



Isb_2 <- Isb_2[,c(1,2,3,4,5,17,18,19,20,21)]

Isb_Class2 <- Isb_2

Isb_Class <- Isb

Isb_Class <- Isb[,c(-7,-8,-9,-11,-12,-13,-14,-15)]


Isb_Class$Price <- ordered(cut(Isb_Class$Price, 
                               + c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb_Class$Area <- ordered(cut(Isb_Class$Area, 
                              + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

Isb_Class$Baths <- as.factor(Isb_Class$Baths)
Isb_Class$Bedroom.s.<- as.factor(Isb_Class$Bedroom.s.)

Isb_Class$Healthcare.Recreational <- ifelse(Isb_Class$Healthcare.Recreational == " ", NA, Isb_Class$Healthcare.Recreational)

colSums(is.na(Isb_Class))
Isb_Class<- na.omit(Isb_Class)

Isb_Class <- Isb_Class[,-2]

Isb_Class <- Isb_Class[,-5]

Isb_Class <- Isb_Class[,c(-9,-6,-7,-8)]

Isb_Class$`Lawn or Garden` <- ifelse(Isb_Class$`Lawn or Garden` == 1, "Yes","No")
Isb_Class$`Lawn or Garden` <- as.factor(Isb_Class$`Lawn or Garden`)

summary(Isb_Class)

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Isb_Class$Price <- as.character(Isb_Class$Price)

Isb_Class$Price <- as.factor(Isb_Class$Price)

Pprice <- as.factor(Isb_Class[,1])

train <- sample(1:nrow(Isb_Class), 337) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_price <- Pprice[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx AREA xxxxxxxxxxxxxx

set.seed(234) 

#taking 65% observations in dataset for training
519*.65

Isb_Class$Area <- as.character(Isb_Class$Area)

Isb_Class$Area <- as.factor(Isb_Class$Area)


PArea <- as.factor(Isb_Class[,3])

train <- sample(1:nrow(Isb_Class), 337) 
test <- -train
train
test
training_data <-Isb_Class[train,]
testing_data <- Isb_Class[test,]
testing_price <- PArea[test]

#creates a model, target variable is High~. 
# dot is representation of all other variables to make the model
tree_model <- tree(Area~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)

cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_model <- prune.misclass(tree_model, best = 7)
plot(prune_model)
text(prune_model, pretty = 0)

tree_pred <- predict(prune_model, testing_data, type = 'class')
mean(tree_pred != testing_price)
mean(tree_pred == testing_price)
table(testing_price, tree_pred)


#######################################################################

#####################################A Rules 2.0##########################

Isb <- Isb_Class2

Isb$Price <- ordered(cut(Isb$Price, 
                         + c(0, 15500000, 27000000, 47500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Isb$Area <- ordered(cut(Isb$Area, 
                        + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Isb))
Isb_C<- na.omit(Isb)

Isb_C <- Isb_C[,-6]

Isb_C$Price <- as.factor(Isb_C$Price)
Isb_C$Location <- as.factor(Isb_C$Location)
Isb_C$Baths<- as.factor(Isb_C$Baths)
Isb_C$Area <- as.factor(Isb_C$Area)
Isb_C$Bedroom.s. <- as.factor(Isb_C$Bedroom.s.)
#Isb_C$Added <- as.factor(Isb_C$Added)

Isb_T <- as(Isb_C,"transactions")

inspect(Isb_T[10:14])

Grule <- apriori(Isb_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Isb_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Other Healthcare and Recreation Facilities=1", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
Grule
plot(Grule)
inspect(Grule[1:10])


############################################################################################

###################################### Combining (Running A-rules 3.0) ###########################################

Khi <- read.csv("PropertyPortal-karachi.csv")
Khi$X <- ifelse(Khi$X == 0, "Karachi", 0) 

Isb <-read.csv("PropertyPortal-islamabad.csv")
Isb$X <- ifelse(Isb$X == 0, "Islamabad", 0) 

Lhr <-read.csv("PropertyPortal-lahore.csv")
Lhr$X <- ifelse(Lhr$X == 0, "Lahore", 0) 

Combined <-rbind(Lhr,Khi,Isb)

Combined <- Combined %>% 
  rename(City = X)

names(Combined)

Combined <- Combined[,c(-2,-3,-4,-9,-11,-12,-13,-14,-15,-16,-17,-18,-20,-21)]

Combined$Price <- substr(Combined$Price,7,20)

Combined <- separate(
  Combined,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Combined$Number <- as.numeric(Combined$Number)

Combined$Price <- ifelse(Combined$Label == "Crore", Combined$Number*10000000, Combined$Number*100000)

#dropped number and label
Combined <- Combined[,c(-3,-4)]

Combined$Baths <- ifelse(Combined$Baths == "-", NA, Combined$Baths)

Combined <- separate(
  Combined,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Combined$Number <- as.numeric(Combined$Number)
Combined$Area <- ifelse(Combined$Label == "Kanal", Combined$Number*20, Combined$Number)

#dropped number and label
Combined <- Combined[,c(-6,-7)]

Combined$Bedroom.s. <- ifelse(Combined$Bedroom.s.== "-", NA, Combined$Bedroom.s.)

summary(Combined)

Combined$Price <- ordered(cut(Combined$Price, 
                              + c(0, 14500000, 26000000, 48500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))


Combined$Area <- ordered(cut(Combined$Area, 
                             + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Combined))
Combined_C<- na.omit(Combined)

Combined_C$Popular <- ifelse(Combined_C$Popular == "" , "Normal",Combined_C$Popular)

Combined_C$Popular <- as.factor((Combined_C$Popular))
summary(Combined_C)

Combined_C$City <- as.factor(Combined_C$City)
Combined_C$Price <- as.factor(Combined_C$Price)
Combined_C$Location <- as.factor(Combined_C$Location)
Combined_C$Baths<- as.factor(Combined_C$Baths)
Combined_C$Area <- as.factor(Combined_C$Area)
Combined_C$Bedroom.s. <- as.factor(Combined_C$Bedroom.s.)
#Isb_C$Added <- as.factor(Isb_C$Added)

Combined_T <- as(Combined_C,"transactions")

inspect(Combined_T[10:14])

Grule <- apriori(Combined_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Combined_T, parameter = list (support = 0.008, conf = 0.05, minlen=2), 
                 appearance = list (rhs = "Area=Extra Large", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

inspect(Grule)



##########################################Combining Ends ###############

Isb$Area<- as.factor(Isb$Healthcare.Recreational)

Isb$Purpose<- as.factor(Isb$Purpose)
Isb$Bedroom.s.<- as.factor(Isb$Bedroom.s.)
Isb$Added<- as.factor(Isb$Added)

Isb$Description<- as.factor(Isb$Description)

Isb$Main.Features<- as.factor(Isb$Main.Features)
Isb$Rooms<- as.factor(Isb$Rooms)
Isb$Business.and.Communication<- as.factor(Isb$Business.and.Communication)
Isb$Healthcare.Recreational<- as.factor(Isb$Healthcare.Recreational)
Isb$Nearby.Locations.and.Other.Facilities<- as.factor(Isb$Nearby.Locations.and.Other.Facilities)
Isb$Other.Facilities<- as.factor(Isb$Other.Facilities)

Isb$Popular<- as.factor(Isb$Popular)
Isb$Source<- as.factor(Isb$Source)
Isb$Time.Stamp<- as.factor(Isb$Time.Stamp)

summary(Isb$Popular)

install.packages("stringr")       # Install stringr package
library("stringr") 


Isb_testing <- Isb[,c(1,15)]
Isb_testing$Nearby.Locations.and.Other.Facilities <- str_split(Isb_testing$Rooms, " , ")

substring(Isb_testing$Label, regexpr(",", Isb_testing$Label) + 1)

Isb_testing$Business.and.Communication <- as.factor(Isb_testing$Business.and.Communication)

summary(Isb_testing$Business.and.Communication)

something <- read.transactions(Isb,)

something <- separate(
  Isb_testing,
  Business.and.Communication,
  into = c("Broadband Internet Access", "Satellite or Cable TV Ready", "Intercom", "Other Business and Communication Facilities"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)


something2 <- separate(
  Isb_testing,
  Business.and.Communication,
  into = c("A","B","C","D"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)

something2$`Broadband Internet Access` <- ifelse(something2$A == " Broadband Internet Access", 1,0) 
something2$`Satellite or Cable TV Ready` <- ifelse(something2$A == " Satellite or Cable TV Ready" | something2$B == "Satellite or Cable TV Ready", 1, 0 )


############################################################################################
#########################Starting with Combined Data Set####################################

###################################### Combining (Running A-rules 3.0) ###########################################

Khi <- read.csv("PropertyPortal-karachi.csv")
Khi$X <- ifelse(Khi$X == 0, "Karachi", 0) 
Khi$X <- ifelse(Khi$X == 0, "Karachi", 0) 


Isb <-read.csv("PropertyPortal-islamabad.csv")
Isb$X <- ifelse(Isb$X == 0, "Islamabad", 0) 
Isb$X <- ifelse(Isb$X == 0, "Islamabad", 0) 


Lhr <-read.csv("PropertyPortal-lahore.csv")
Lhr$X <- ifelse(Lhr$X == 0, "Lahore", 0) 
Lhr$X <- ifelse(Lhr$X == 0, "Lahore", 0) 


Combined <-rbind(Lhr,Khi,Isb)

Combined <- Combined %>% 
  rename(City = X)

names(Combined)

Combined <- Combined[,c(-2,-3,-4,-9,-11,-12,-13,-14,-15,-17,-18,-20,-21)]

Combined$Price <- substr(Combined$Price,7,20)

Combined <- separate(
  Combined,
  Price,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

options(scipen = 999)

Combined$Number <- as.numeric(Combined$Number)

Combined$Price <- ifelse(Combined$Label == "Crore", Combined$Number*10000000, Combined$Number*100000)

#dropped number and label
Combined <- Combined[,c(-3,-4)]

Combined$Baths <- ifelse(Combined$Baths == "-", NA, Combined$Baths)

Combined <- separate(
  Combined,
  Area,
  into = c("Number", "Label"),
  sep = " ",
  remove = F,
)

Combined$Number <- as.numeric(Combined$Number)
Combined$Area <- ifelse(Combined$Label == "Kanal", Combined$Number*20, Combined$Number)

#dropped number and label
Combined <- Combined[,c(-6,-7)]


Combined$Bedroom.s. <- ifelse(Combined$Bedroom.s.== "-", NA, Combined$Bedroom.s.)

summary(Combined)

Combined$Price <- ordered(cut(Combined$Price, 
                              + c(0, 14500000, 26000000, 48500000, 850000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))


Combined$Area <- ordered(cut(Combined$Area, 
                             + c(0, 7, 10, 20, 210)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Combined))
Combined_C<- na.omit(Combined)

Combined_C$Popular <- ifelse(Combined_C$Popular == "" , "Normal",Combined_C$Popular)

Combined_C$Popular <- as.factor((Combined_C$Popular))
summary(Combined_C)

Combined_C$City <- as.factor(Combined_C$City)
Combined_C$Price <- as.factor(Combined_C$Price)
Combined_C$Location <- as.factor(Combined_C$Location)
Combined_C$Baths<- as.factor(Combined_C$Baths)
Combined_C$Area <- as.factor(Combined_C$Area)
Combined_C$Bedroom.s. <- as.factor(Combined_C$Bedroom.s.)
#Isb_C$Added <- as.factor(Isb_C$Added)

Combined_C$Healthcare.Recreational<- as.factor(Combined_C$Healthcare.Recreational)

Combined_C <- separate(
  Combined_C,
  Healthcare.Recreational,
  into = c("A","B","C","D","E"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)

Combined_C$A <- as.factor(Combined_C$A)
Combined_C$B <- as.factor(Combined_C$B)
Combined_C$C <- as.factor(Combined_C$C)
Combined_C$D <- as.factor(Combined_C$D)
Combined_C$E <- as.factor(Combined_C$E)

summary(Combined_C[,8:12])

Combined_C$`Lawn or Garden` <- ifelse(Combined_C$A == " Lawn or Garden", 1,0) 
Combined_C$`Jacuzzi` <- ifelse(Combined_C$A == " Jacuzzi" | Combined_C$B == "Jacuzzi" | Combined_C$C == "Jacuzzi"| Combined_C$D == "Jacuzzi", 1,0) 
Combined_C$`Swimming Pool` <- ifelse(Combined_C$A == " Swimming Pool" | Combined_C$B == "Swimming Pool" | Combined_C$C == "Swimming Pool"| Combined_C$D == "Swimming Pool", 1,0) 
Combined_C$`Sauna` <- ifelse(Combined_C$A == " Sauna" | Combined_C$B == "Sauna" | Combined_C$C == "Sauna"| Combined_C$D == "Sauna", 1,0) 
Combined_C$`Other Healthcare and Recreation Facilities` <- ifelse(Combined_C$A == " Other Healthcare and Recreation Facilities" | Combined_C$B == "Other Healthcare and Recreation Facilities" | Combined_C$C == "Other Healthcare and Recreation Facilities"| Combined_C$D == "Other Healthcare and Recreation Facilities" | Combined_C$E == "Other Healthcare and Recreation Facilities", 1,0) 


Combined_C$`Lawn or Garden` <- as.factor(Combined_C$`Lawn or Garden`)
Combined_C$Jacuzzi <- as.factor(Combined_C$Jacuzzi)
Combined_C$`Swimming Pool` <- as.factor(Combined_C$`Swimming Pool`)
Combined_C$Sauna <- as.factor(Combined_C$Sauna)
Combined_C$`Other Healthcare and Recreation Facilities` <- as.factor(Combined_C$`Other Healthcare and Recreation Facilities`)

Combined_C$Jacuzzi[is.na(Combined_C$Jacuzzi)] = 0
Combined_C$`Swimming Pool`[is.na(Combined_C$`Swimming Pool`)] = 0
Combined_C$Sauna[is.na(Combined_C$Sauna)] = 0

Combined_C$`Other Healthcare and Recreation Facilities` <- as.character(Combined_C$`Other Healthcare and Recreation Facilities`)
Combined_C$`Other Healthcare and Recreation Facilities`[is.na(Combined_C$`Other Healthcare and Recreation Facilities`)] = 0
Combined_C$`Other Healthcare and Recreation Facilities` <- as.factor(Combined_C$`Other Healthcare and Recreation Facilities`)

Combined_C <- Combined_C[,c(-7,-8,-9,-10,-11,-12,-13)]

######## Transactionz begin
Combined_T <- as(Combined_C,"transactions")

#detach(package:tm, unload=TRUE)
library(arules)

inspect(Combined_T[10:14])

Grule <- apriori(Combined_T, parameter = list(support = 0.01, conf = 0.5))
#Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Combined_T, parameter = list (support = 0.008, conf = 0.05, minlen=2), 
                 appearance = list (rhs = "Other Healthcare and Recreation Facilities=1", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
#Grule
inspect(Grule[1:10])

####################################################KMEANS ##################
summary(Khi)

Combined <- Combined[,c(-1,-7,-8)]

dev.off()

Combined_Cluster_all <- Combined


Combined_Cluster_all$Baths <- as.numeric(Combined_Cluster_all$Baths)
Combined_Cluster_all$Bedroom.s. <- as.numeric(Combined_Cluster_all$Bedroom.s.)

colSums(is.na(Combined_Cluster_all))
Combined_Cluster_all<- na.omit(Combined_Cluster_all)

#Khi_Cluster <- Khi_Cluster_all[,-2]
Combined_Cluster <- data.frame(scale(Combined_Cluster_all[,-2]))

nb<- NbClust(Combined_Cluster, distance = "euclidean", min.nc = 2,max.nc=10, method = "kmeans", index ="all")

fviz_nbclust(nb)

kout3 <- kmeans(Combined_Cluster, centers = 3, nstart = 10) 
kout3
kout3$size
table(kout3$size)

table(Combined_Cluster_all$Location, kout3$cluster)

library(ggplot2)
A <- ggplot(data=Combined_Cluster, aes(Price, Area))
A+geom_point(aes(col=factor(kout3$cluster))) + scale_x_log10() + scale_y_log10()


Price.c <- tapply(Combined_Cluster$Price,kout3$cluster, mean)
Area.c <- tapply(Combined_Cluster$Area,kout3$cluster, mean)
centers <- data.frame(Price.c,Area.c)

C<- B+geom_point(data=centers,aes(Price.c,Area.c),color="red",pch=4,size=10) 

C

fviz_cluster(kout3,Combined_Cluster, geom=c("point","text"))

remove(kout3)

################################################################################


################################################################################

Combined <- Combined_Class2

Lhr$Price <- ordered(cut(Lhr$Price, 
                         + c(0, 16500000, 29000000, 45000000, 390000000)), labels = c("Low Price","Medium Price", "High Price", "Very High Price"))

Lhr$Area <- ordered(cut(Lhr$Area, 
                        + c(0,8, 10, 20, 120)), labels = c("Small","Medium", "Large", "Extra Large"))

colSums(is.na(Lhr))
Lhr_C<- na.omit(Lhr)

Lhr_C <- Lhr_C[,-6]

Lhr_C$Price <- as.factor(Lhr_C$Price)
Lhr_C$Location <- as.factor(Lhr_C$Location)
Lhr_C$Baths<- as.factor(Lhr_C$Baths)
Lhr_C$Area <- as.factor(Lhr_C$Area)
Lhr_C$Bedroom.s. <- as.factor(Lhr_C$Bedroom.s.)
#Lhr_C$Added <- as.factor(Lhr_C$Added)

Lhr_T <- as(Lhr_C,"transactions")

inspect(Lhr_T[10:14])

Grule <- apriori(Lhr_T, parameter = list(support = 0.01, conf = 0.5))
Grule
summary (Grule)

Grule <- sort (Grule, by = "lift")
inspect(Grule[1:10])

plot(Grule)

dev.off()
Grule <- apriori(Lhr_T, parameter = list (support = 0.008, conf = 0.5, minlen=2), 
                 appearance = list (rhs = "Other Healthcare and Recreation Facilities=1", default = "lhs"))
# changes here

Grule <- sort (Grule, by = "lift")
plot (Grule)
inspect(Grule[1:10])



##########################################Combining Ends ###############

Isb$Area<- as.factor(Isb$Healthcare.Recreational)

Isb$Purpose<- as.factor(Isb$Purpose)
Isb$Bedroom.s.<- as.factor(Isb$Bedroom.s.)
Isb$Added<- as.factor(Isb$Added)

Isb$Description<- as.factor(Isb$Description)

Isb$Main.Features<- as.factor(Isb$Main.Features)
Isb$Rooms<- as.factor(Isb$Rooms)
Isb$Business.and.Communication<- as.factor(Isb$Business.and.Communication)
Isb$Healthcare.Recreational<- as.factor(Isb$Healthcare.Recreational)
Isb$Nearby.Locations.and.Other.Facilities<- as.factor(Isb$Nearby.Locations.and.Other.Facilities)
Isb$Other.Facilities<- as.factor(Isb$Other.Facilities)

Isb$Popular<- as.factor(Isb$Popular)
Isb$Source<- as.factor(Isb$Source)
Isb$Time.Stamp<- as.factor(Isb$Time.Stamp)

summary(Isb$Popular)

install.packages("stringr")       # Install stringr package
library("stringr") 


Isb_testing <- Isb[,c(1,15)]
Isb_testing$Nearby.Locations.and.Other.Facilities <- str_split(Isb_testing$Rooms, " , ")

substring(Isb_testing$Label, regexpr(",", Isb_testing$Label) + 1)

Isb_testing$Business.and.Communication <- as.factor(Isb_testing$Business.and.Communication)

summary(Isb_testing$Business.and.Communication)

something <- read.transactions(Isb,)

something <- separate(
  Isb_testing,
  Business.and.Communication,
  into = c("Broadband Internet Access", "Satellite or Cable TV Ready", "Intercom", "Other Business and Communication Facilities"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)


something2 <- separate(
  Isb_testing,
  Business.and.Communication,
  into = c("A","B","C","D"),
  sep = " , ",
  remove = F,
  convert = T,
  #  extra = "merge",
  #  fill = "left",
)

something2$`Broadband Internet Access` <- ifelse(something2$A == " Broadband Internet Access", 1,0) 
something2$`Satellite or Cable TV Ready` <- ifelse(something2$A == " Satellite or Cable TV Ready" | something2$B == "Satellite or Cable TV Ready", 1, 0 )

################################################### CODE ENDS ##########################
