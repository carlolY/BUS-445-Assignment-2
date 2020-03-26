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
library(gplots)
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

corrMatrix <- cor(select_if(QK2, is.numeric)) # see ?dplyr::select_if

corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

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

par(mfrow = c(1,3))
partial(QKForestAll, pred.var = "NumDeliv", # target and predictor
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

partial(QKForestAll, pred.var = "TotPurch",
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = T, # plot decile hashmarks
        plot.engine = "ggplot2")        

partial(QKForestAll, pred.var = "LastOrder",
        prob = TRUE, # probabilities on yaxis
        which.class = 2, # predict level 2, "Y"
        plot = TRUE, # generate plot
        rug = TRUE, # plot decile hashmarks
        plot.engine = "ggplot2")

#Trim the top 10%
QKForestAll.trim <- partial(QKForestAll, pred.var = "NumDeliv",
                              prob = TRUE,
                              which.class = 2,
                              quantiles = TRUE, # prepare data trimming
                              probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                              plot= FALSE) # generate data, no plot
plotPartial(QKForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(QK2, Sample == "Estimation"))

QKForestAll.trim <- partial(QKForestAll, pred.var = "TotPurch",
                            prob = TRUE,
                            which.class = 2,
                            quantiles = TRUE, # prepare data trimming
                            probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                            plot= FALSE) # generate data, no plot
plotPartial(QKForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(QK2, Sample == "Estimation"))

QKForestAll.trim <- partial(QKForestAll, pred.var = "LastOrder",
                            prob = TRUE,
                            which.class = 2,
                            quantiles = TRUE, # prepare data trimming
                            probs = seq(from = 0.0, to = 0.9, by = 0.02), # of bottom 90%
                            plot= FALSE) # generate data, no plot
plotPartial(QKForestAll.trim, # and pass data to plotting function
            rug = TRUE,
            train = filter(QK2, Sample == "Estimation"))


##### Logistic Regression #####
QK$SUBSCRIBE.num = ifelse(QK$SUBSCRIBE == "N", 0,1)
QK$DA_Income.Cat <- binVariable(QK$DA_Income, bins = 10,
                             method = "interval",
                             labels = NULL)
QK$NumDeliv.Cat <- binVariable(QK$NumDeliv, bins = 10,
                                method = "interval",
                                labels = NULL)


plotmeans(SUBSCRIBE.num ~ DA_Income.Cat, data = QK)
plotmeans(SUBSCRIBE.num ~ NumDeliv.Cat, data = QK)

QK2$DA_Income2 = I(QK2$DA_Income^2)
QK2$NumDeliv2 = I(QK2$NumDeliv^2)

QK.log = glm(SUBSCRIBE ~ Disc + LastOrder + DA_Income2 + DA_Under20 + DA_Over60 + 
            DA_Single + NumDeliv2 + Healthy + Veggie + Meaty + Special + 
            TotPurch + MealsPerDlv, 
            data = filter(QK2, Sample =="Estimation"),
            family = binomial(logit))
summary(QK.log)


# Calculate and print McFadden R square (See Logistic Regression Chapter)
paste("McFadden R^2:", round(1 - (QK.log$deviance / QK.log$null.deviance),3))

#Run a stepwise regression using the "Weslogis" model
QKStep <- step(QK.log, direction = "both")
summary(QKStep)

## Lift Chart
lift.chart(modelList = c("QK.log", "QKStep"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01, type = "cumulative",
           sub = "Validation")

# Compare forests and regression models
lift.chart(modelList = c("QKStep","QKForestAll"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01, type = "cumulative",
           sub = "Validation")


##### Neural Network #####
# 4 node nnet
QKNnet4 <- Nnet(formula = SUBSCRIBE ~ Disc + LastOrder + DA_Income + DA_Under20 + 
                  NumDeliv + Healthy + Meaty + Special + MealsPerDlv, 
                data = filter(QK2, Sample == "Estimation"),
                 decay = 0.15, size = 4)

lift.chart(modelList = c("QKStep","QKNnet4"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01, type = "cumulative",
           sub = "Validation")

## All variables in nnet
QKNetAllv <- Nnet(formula = SUBSCRIBE ~ Disc + LastOrder + DA_Income + DA_Under20 + 
                    DA_Over60 + DA_Single + NumDeliv + NumMeals + Healthy + Veggie + 
                    Meaty + Special + TotPurch + Sample + MealsPerDlv,
                   data = filter(QK2, Sample =="Estimation"),
                   decay = 0.15, size = 4)

lift.chart(modelList = c("QKStep","QKNetAllv"),
           data = filter(QK2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.01, type = "cumulative",
           sub = "Validation")
