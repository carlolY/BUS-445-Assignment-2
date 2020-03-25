#install.packages("readxl")
library(readxl)
library("dplyr")
library("car")
library("forcats")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
source("BCA_functions_source_file.R")

QK = read_excel("QK_2020.xlsx")
QK = as.data.frame(QK)
View(QK)
names(QK)
QK = QK %>% select(-c("...1", "custid", "Pcode"))
QK$LastOrder = as.numeric(QK$LastOrder)

# Convert to numeric in order to show %NA
QK$DA_Over60 = as.numeric(QK$DA_Over60, na.omit = T)
QK$DA_Single = as.numeric(QK$DA_Single, na.omit = T)
QK$DA_Income = as.numeric(QK$DA_Income, na.omit = T)
variable.summary(QK)

QK$MealsPerDeliv = as.factor(QK$MealsPerDeliv)
table(QK$MealsPerDeliv)

# Group levels
QK$MealsPerDlv <-
  recode_factor(QK$MealsPerDeliv,
                `0.531914894`  = "<=1",
                `0.543478261` = "<=1",
                `0.568181818` = "<=1",
                `0.581395349` = "<=1",
                `0.595238095` = "<=1",
                `0.609756098` = "<=1",
                `1` = "<=1",
                `3` = "3+",
                `4` = "3+")
QK$MealsPerDeliv = NULL

QK$SUBSCRIBE = as.factor(QK$SUBSCRIBE)
QK$Disc = as.factor(QK$Disc)
QK$Title = as.factor(QK$Title)
QK$Sample = as.factor(QK$Sample)
variable.summary(QK)

# Impute NAs as NAs are meaningful
QK$Disc <- recode_factor(QK$Disc, # Factor of interest
                            "NA" = "ND") # replacement value

table(QK$Title)
table(QK$Weeks3Meals)
QK$Weeks3Meals = NULL
QK$Title = NULL

QK2 <- na.omit(QK)

##### Random Forests #####
paste(names(QK), collapse = " + ")

QKForestAll = randomForest(SUBSCRIBE ~ Disc + LastOrder + DA_Income + DA_Under20 + 
                        DA_Over60 + DA_Single + NumDeliv + NumMeals + Healthy + Veggie + 
                        Meaty + Special + TotPurch +  MealsPerDlv,
                        data = filter(QK2, Sample == "Estimation"),
                        importance = TRUE,
                        ntree = 500, mtry = 4)

## Contingency Table
QKForestAll[["confusion"]]

# Variable importance
varImpPlot(QKForestAll,type = 2,
           main="WesForestAllv", # title
           cex =0.7) # font size
