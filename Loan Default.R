## Week 6 - In Class - Loan Default

## Install new R packages
install.packages("gplots") # for plotMeans()
install.packages("corrplot")

## Load required R packages
library("dplyr")
library("car")
library("forcats")
library("gplots")
library("effects")
library("corrplot")

source("BCA_functions_source_file.R")

# Read file to R
LoanDefault <- read.csv(file="LoanDefault.csv", stringsAsFactors = TRUE)

# Set CustID variable as record column name
rownames(LoanDefault)<-LoanDefault$CustID
LoanDefault$CustID<-NULL

# Delete "BranchID" variable
LoanDefault$BranchID<-NULL

# Check to make sure it is correct
View(LoanDefault)

# Look at summary statistics
variable.summary(LoanDefault)

# Check the correlations among continuous predictor variables
corLD<-cor(select_if(LoanDefault, is.numeric))
View(corLD)
corrplot(corLD, method="number", type="lower",diag=FALSE,number.cex=0.7)

# Split data into "estimation" and "validation" samples
# Create Estimation (Training) and Validation (Test) sample
LoanDefault$Sample <- create.samples(LoanDefault,
                             est = 0.50, # allocate 50% to estimation sample
                             val = 0.50, # 50% to validation sample
                             rand.seed = 1) # for reproducibility

# Run several logistic models using backward approach (incl. all variables, and 
# removing those insignificant variables) using estimation sample

## MODEL 1
LinearLDALL <- glm(formula = DEFAULT ~ yrsaddrs + age + crcdebt +
                yremploy + income + mrtgebal + ncustbrch +
                Education + ATMpcent + onlinefreq + othdebt + carvalue,
              data = filter(LoanDefault, Sample == "Estimation"),
              family = binomial(logit))

summary(LinearLD)

# Create ANOVA table
Anova(LinearLD)

## MODEL 2
LinearLD2 <- glm(formula = DEFAULT ~ yrsaddrs + age + crcdebt +
                  yremploy + income + mrtgebal + ncustbrch +
                  ATMpcent + onlinefreq + othdebt + carvalue,
                data = filter(LoanDefault, Sample == "Estimation"),
                family = binomial(logit))

summary(LinearLD2)

## MODEL 3
LinearLD3 <- glm(formula = DEFAULT ~ yrsaddrs + age + crcdebt +
                   yremploy + income + mrtgebal + ncustbrch +
                   onlinefreq + othdebt + carvalue,
                 data = filter(LoanDefault, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearLD3)

## MODEL 4
LinearLD4 <- glm(formula = DEFAULT ~ yrsaddrs + crcdebt +
                   yremploy + income + mrtgebal + ncustbrch +
                   onlinefreq + othdebt + carvalue,
                 data = filter(LoanDefault, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearLD4)

## MODEL 5
LinearLD5 <- glm(formula = DEFAULT ~ yrsaddrs + crcdebt +
                   yremploy + income + mrtgebal + ncustbrch +
                   onlinefreq + othdebt,
                 data = filter(LoanDefault, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearLD5)

## MODEL 6
LinearLD6MinAIC <- glm(formula = DEFAULT ~ yrsaddrs + crcdebt +
                   yremploy + income + mrtgebal + ncustbrch +
                   onlinefreq,
                 data = filter(LoanDefault, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearLD6)

## MODEL 7
LinearLD7 <- glm(formula = DEFAULT ~ yrsaddrs + crcdebt +
                   yremploy + income + mrtgebal + onlinefreq,
                 data = filter(LoanDefault, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearLD7)

## MODEL 8
LinearLD8 <- glm(formula = DEFAULT ~ yrsaddrs + crcdebt +
                   yremploy + income + mrtgebal,
                 data = filter(LoanDefault, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearLD8)

## MODEL 9
LinearLD9Sig <- glm(formula = DEFAULT ~ yrsaddrs + crcdebt +
                   yremploy + mrtgebal,
                 data = filter(LoanDefault, Sample == "Estimation"),
                 family = binomial(logit))

summary(LinearLD9)


# select your best models with minimized AIC and create lift charts

# Function: lift.chart()
# Usage: lift.chart(modelList, data, targLevel, trueResp, type, sub/title) - see ?BCA::lift.# Create Cumulative Lift Chart for Estimation (training) sample
# "LogCCS" and "MixedCCS2" perform best

## Run with estimation sample
lift.chart(modelList = c("LinearLDALL", "LinearLD6MinAIC", "LinearLD9Sig"),
           data = filter(LoanDefault, Sample == "Estimation"), # or "Validation"
           targLevel = "Yes", # Desired Level of target variable "MonthGive"
           trueResp = 0.053, # True response rate in original dataset
           type = "cumulative", # Or "incremental"
           sub = "Estimation Set") # Specify chart's subtitle

## Run with validation sample
lift.chart(modelList = c("LinearLDALL", "LinearLD6MinAIC", "LinearLD9Sig"),
           data = filter(LoanDefault, Sample == "Validation"),
           targLevel = "Yes", 
           trueResp = 0.053, 
           type = "cumulative", 
           sub = "Validation Set") 

## LinearLD9 did best in validation sample as its more parsimonious

# Approving John and Mary -------------------------

# read csv file 
JandM <- read.csv(file="JohnAndMary.csv", stringsAsFactors = TRUE)

# Unadjusted probabilities 
JandM$prob_u<-predict(LinearLD9Sig, JandM, type="response")

# Calculate the adjusted probabilities 
PP_R<-0.053/0.3833  #positive ratio between true dataset and resampled data - DEFAULT
PN_R<-(1-0.053)/(1-0.3833)  #negative ratio between true dataset and resampled data - NOT DEFAULT

JandM$prob_a<-JandM$prob_u*PP_R/((JandM$prob_u)*PP_R+(1-JandM$prob_u)*PN_R)

# Write the new dataset into a csv file.
write.csv(JandM,"JohnandMary_Prob.csv")

# raw probability
predict(LinearLD9Sig, JandM, type="response")

# read new csv file 
JandMprob <- read.csv(file="JohnAndMary_Prob.csv", stringsAsFactors = TRUE)

# adjusted probability 
predict(LinearLD9Sig, JandMprob, type="response")



