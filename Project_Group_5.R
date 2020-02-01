##**Project Group-5**##
##**Samrita Ghosh, Ruying Situ, Vivek Advani, Nishit Patel, Aniruddhsinh Vashi**##


##**Data Cleaning****************************************************************##
df <- read.csv('listings_summary.csv')

#*Drop columns with texts
df1 <- df[, -c(2:25,28,30,31,35,38:40,42:48,60,62,63,70:74,76,78,79,81:89,91,93:96)]
names(df1)
nrow(df1)
ncol(df1)

#*Convert blank cells to NA
df1[df1 == ""] <- NA
summary(df1)

#*Fill zero into NA values in price-variable columns
df1$security_deposit <- as.character(df1$security_deposit)
df1$security_deposit[is.na(df1$security_deposit)] <- "$0.00"
df1$cleaning_fee[is.na(df1$cleaning_fee)] <- "$0.00"
df1$extra_people[is.na(df1$extra_people)] <- "$0.00"

#*Check missing data
colSums(is.na(df1))

#*Drop columns with too many missing data & NA
library(DataCombine)
df2 <- df1[ ,-c(2:7,9,31,36)]
summary(df2)

#*Remove rows with NA's in some variables
df2 <- DropNA(df2, Var = "bathrooms")
df2 <- DropNA(df2, Var = "bedrooms")
df2 <- DropNA(df2, Var = "beds")

#*Double check if there are still missing data
colSums(is.na(df2))
sapply(df2,class)
ncol(df2)
summary(df2)


#*Explore data by plotting bar charts of some categorical variables
library(ggplot2)
g<-ggplot(df2, aes(x=property_type)) + geom_bar(color='orange') 
g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#here most of the instances are apartment, so we drop this column

ggplot(df, aes(x=bed_type)) + geom_bar(color='blue') 
#here most of the instances are real bed, so we drop this column

df3 <- df2[, -c(2,7,13)]
names(df3)

df3$price <- as.numeric(gsub('[$,]', '', df3$price))
df3$security_deposit <- as.numeric(gsub('[$,]', '', df3$security_deposit))
df3$cleaning_fee <- as.numeric(gsub('[$,]', '', df3$cleaning_fee))
df3$extra_people <- as.numeric(gsub('[$,]', '', df3$extra_people))


##**Feature ENGG 1*******************************************************##
library("lattice")
library("dummies")

df3$Less_than_6_minimum_nights  <- df3$minimum_nights %in% c(1,2,3,4,5) 
summary(df3)
df3$Less_than_2000_maximum_nights  <- df3$maximum_nights %in% c(1:1999)
summary(df3)


##**Feature ENGG 2*******************************************************##
a <- data.frame(sort(table(unlist(strsplit(tolower(df3$amenities), ","))),decreasing=TRUE))
b <- a[1:30,]
b

pal <- colorRampPalette(colors = c("black","brown"))(30)
q<-barplot(b$Freq,  names.arg = b$Var1, 
      ylab="count",main="Frequency of amenities provided",las=2, col = pal)


df3$TV <- grepl("TV",df3$amenities )
df3$Family_kid_friendly <- grepl("Family/kid friendly",df3$amenities)
df3$Street_parking <- grepl("Free street parking",df3$amenities)
df3$Refrigerator <- grepl("Refrigerator",df3$amenities)
df3$Elevator <- grepl("Elevator",df3$amenities)

summary(df3)


##*Feature ENGG 3*******************************************************##
#*Distance from middle of berlin
library(sp)
library(rgeos)
library(geosphere)
library(dplyr)
dis <- df3  %>%  mutate(CTD = distHaversine(cbind(13.404166666666667, 52.5027778), cbind(df3$longitude, df3$latitude)))
df3 <- dis
df3$CTD <- df3$CTD/1000
summary(df3)
head(df3)


##**Data Exploration******************************************************************##

#*Price difference by neighbourhood
library(stringr)
library(car)
library(faraway)
library(ggplot2)
library(corrgram)
library(leaps)
library(gridExtra)
library(corrplot)
library(MASS)
library(plyr) 
library(dplyr)

#*Showing total number of neighbourhood group cleansed
pal <- colorRampPalette(colors = c("black","Green"))(12)
ggplot(df3, aes(x=neighbourhood_group_cleansed)) + geom_bar(col=pal) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#*Histogram of price
ggplot(data = df3, 
       aes(df3$price)) + 
  geom_histogram(bins = 50, 
                 col = "#000000", 
                 fill = "#99FFFF", 
                 alpha = .5) + 
  labs(x = "Price", y = "Frequency") + 
  scale_x_continuous(breaks = c(seq(0, 800, 200),1000, 1500, 2000)) +
  scale_y_continuous(breaks = c(seq(0, 4000, 1000), 4600)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11))

##We get a left skewed graph. To rectify that we take the log value of variable price

ggplot(data = df3, 
       aes(log(df3$price))) + 
  geom_histogram(bins = 50, 
                 col = "#000000", 
                 fill = "#99FFFF", 
                 alpha = .5) + 
  labs(x = "Price", y = "Frequency") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11))

##Price according to the mapping coordinates(darker the colour higher the price)

plot(df3$longitude,df3$latitude,col=df3$price,xlab="longitude",ylab="latitude")


##Side-by-side boxplots without using 0-500 price, for each neighbourhood.
ggplot(df3, 
       aes(x = as.numeric(df3$neighbourhood_group_cleansed), 
           y = df3$price, 
           fill = df3$neighbourhood_group_cleansed)) + 
  geom_boxplot(alpha = .6, outlier.alpha = .4) +
  scale_y_continuous(name = "Price",limits = c(0, 500)) + 
  scale_x_discrete(name = "Neighbourhoods") + 
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom") + 
  theme(legend.text = element_text(size = 8)) + 
  labs(fill = "")


#*CTD for price 0-500
ggplot(df3, aes(x=CTD, y=price)) + geom_point() +scale_y_continuous(limits = c(0, 500))


#*Boxplots of price 0-500, for each room type.
ggplot(df3, 
       aes(x = as.numeric(df3$room_type), 
           y = df3$price,  
           fill = df3$room_type)) + 
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.4) + 
  scale_y_continuous(name = "Price",limits = c(0, 500)) + 
  scale_x_discrete(name = "Room Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1") +
  labs(fill = "")


#*Scatterplots of logarithm of price with respect to accommodates, bathrooms, bedrooms, and beds, respectively.
#p1 = ggplot(df3, 
#            aes(x = df3$accommodates, 
#                y = log(df3$price))) + 
#  geom_point(size=2,shape=3) + scale_y_discrete(name = "Price",limits = c(0, 500))+
#  labs( x = 'Accommodates', y = 'price') 

#p2 = ggplot(df3, 
#            aes(x = df3$bathrooms, 
 #               y = log(df3$price))) + 
  #geom_point(size=2,shape=4) +scale_y_discrete(name = "Price",limits = c(0, 500))+
  #labs(x = 'Bathrooms', y = 'price') 


#p3 = ggplot(df3, 
 #           aes(x = df3$bedrooms, 
  #              y = log(df3$price))) + 
  #geom_point(size=2,shape=2) + scale_y_discrete(name = "Price",limits = c(0, 500))+
  #labs(x = 'Bedrooms', y = 'price') 
 

#p4 = ggplot(df3, 
 #           aes(x = df3$beds, 
  #              y = log(df3$price))) + 
  #geom_point(size=2,shape=1) + scale_y_discrete(name = "Price",limits = c(0,500))+
  #labs(x = 'Beds', y = 'price') 

#grid.arrange(p1,p2,p3,p4)


#*Scatterplot of price and cleaning fee(0-500).
p5 = ggplot(df3, 
            aes(x = df3$cleaning_fee, 
                y = df3$price)) + 
  geom_point(color='darkblue') + scale_y_continuous(name = "Price",limits = c(0, 500))+
  scale_x_continuous(name = "Cleaning Fee",limits = c(0, 500))+
  theme_minimal() +
  labs(x = 'Cleaning Fee', y = 'Log of Price') +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 2))
p5


##**Data Modelling******************************************************************##

Airbnb <- df3[,-c(1,2,3,4,5,6,11,18)]
Airbnb1 <- dummy.data.frame(Airbnb, sep = ".")
set.seed(1)
train.rows<-sample(rownames(Airbnb1),dim(Airbnb1)[1]*0.6)
valid.rows <- sample(setdiff(rownames(Airbnb1), train.rows), 
                     dim(Airbnb1)[1]*0.2)
test.rows <- setdiff(rownames(Airbnb1), union(train.rows, valid.rows))
train.data <- Airbnb1[train.rows, ]
valid.data <- Airbnb1[valid.rows, ]
test.data <- Airbnb1[test.rows, ]
dim(train.data)
dim(valid.data)
dim(test.data)

library(forecast)
REG.TRAIN <- lm(price ~. ,data = train.data)
summary(REG.TRAIN)

pred <- predict(REG.TRAIN, newdata = valid.data)
accuracy(pred,valid.data$price)


REG.TRAIN1 <- lm(price ~ accommodates + 
                  bathrooms + 
                  bedrooms + 
                  beds + 
                  security_deposit +
                  cleaning_fee + 
                  guests_included +
                  extra_people +
                  availability_365 +
                   number_of_reviews+
                  instant_bookable.f+   
                  Less_than_6_minimum_nights +cancellation_policy.super_strict_30 +
                  CTD, data = train.data)
summary(REG.TRAIN1)

pred1 <- predict(REG.TRAIN1, newdata = valid.data)
accuracy(pred1,valid.data$price)


#*Exhaustive search
install.packages("leaps")
library(leaps)
search <- regsubsets(price ~ ., data = Airbnb1, nbest = 1, nvmax = dim(Airbnb1)[2],
                     method = "exhaustive")
sum <- summary(search)
sum$which
sum$rsq
sum$adjr2
sum$cp

model1<-lm(price ~ accommodates + 
             bedrooms + 
             beds + 
             cleaning_fee + 
             guests_included +
             extra_people +
             minimum_nights+
             availability_365 +
             number_of_reviews+
             instant_bookable.t+ 
             cancellation_policy.flexible+ 
             cancellation_policy.moderate+
             cancellation_policy.strict_14_with_grace_period+  
             Less_than_6_minimum_nights +
             cancellation_policy.super_strict_60 +
             Family_kid_friendly+  
             CTD+
             Street_parking+
             Elevator,
             data = train.data)
summary(model1)

model2<-lm(price ~ accommodates + 
             bedrooms + 
             beds + 
             cleaning_fee + 
             guests_included +
             extra_people +minimum_nights+
             availability_365 +
             number_of_reviews+
             instant_bookable.f+ 
             cancellation_policy.flexible+ 
             cancellation_policy.moderate+
             cancellation_policy.strict_14_with_grace_period+  
             Less_than_6_minimum_nights +
             cancellation_policy.super_strict_30 +
             Family_kid_friendly+  
             CTD+
             TV +
             Street_parking+
             Elevator,
           data = train.data)
summary(model2)

model3<-lm(price ~ accommodates + 
             bedrooms + 
             beds + 
             cleaning_fee + 
             guests_included +
             extra_people +
             minimum_nights+
             availability_365 +
             number_of_reviews+
             instant_bookable.f+ 
             cancellation_policy.flexible+ 
             cancellation_policy.moderate+
             cancellation_policy.strict_14_with_grace_period+  
             Less_than_6_minimum_nights +
             cancellation_policy.super_strict_30 +
             Family_kid_friendly+ 
             CTD+
             TV +
             Street_parking+
             Refrigerator+
             Elevator,
           data = train.data)
summary(model3)

##Adjusted r square of model 2 is better

library(forecast)
pred1 <- predict(model1, newdata = valid.data)
accuracy(pred1,valid.data$price)
pred2 <- predict(model2, newdata = valid.data)
accuracy(pred2,valid.data$price)
pred3 <- predict(model3, newdata = valid.data)
accuracy(pred3,valid.data$price)

#*Testing model 2 on test data
pred4 <- predict(model2, newdata = test.data)
accuracy(pred4,test.data$price)


##**Regression tree
library(rpart)
library(rpart.plot)
library(caret)
airbnb.tree <- rpart(price ~. , data = train.data)

length(airbnb.tree$frame$var[airbnb.tree$frame$var == "<leaf>"])

#*Plotting tree
prp(airbnb.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(airbnb.tree$frame$var == "<leaf>", 'lightblue', 'lightgreen')) 


pred4 <- predict(airbnb.tree, newdata = valid.data)
accuracy(pred4,valid.data$price)

pred4 <- predict(airbnb.tree, newdata = test.data)
accuracy(pred4,test.data$price)
