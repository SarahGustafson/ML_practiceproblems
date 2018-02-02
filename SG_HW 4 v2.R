
#######################################################################################################
########################## CH. 1, Questions 1-4 #######################################################


###  Q1
###############################################################

#install.packages("freqparcoord")
require(freqparcoord)
data(mlb)


xvalpart <- function(data, p) {
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace = FALSE)
  list(train = data[trainidxs,], valid = data[-trainidxs,])
}

xvallm <- function(data, ycol, predvars, p, meanabs = TRUE) {
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  #fit model to training data
  trainy <- train[,ycol]
  trainpreds <- train[,predvars]
  # using matrix form in lm() call
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  # apply fitted model to validation data; not that %*% works only on matrices, not data frames
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1, validpreds)%*%coef(lmout)
  realy <- valid[,ycol]
  if(meanabs)
    return (mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}


#Check with 80-20 split

xvallm(mlb,5 ,c(4 ,6) ,0.8) #12.46
xvallm(mlb,5 ,c(4 ,6) ,0.7) #12.68
xvallm(mlb,5 ,c(4 ,6) ,0.6) #13.68
xvallm(mlb,5 ,c(4 ,6) ,0.5) #13.50


### Checking the same for KNN algorithm 

xvalknn <- function(data, ycol, predvars, k, p, meanabs = TRUE) {
  data <- data[,c(predvars, ycol)]
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol], k)
  kout <- knnest(train[,ycol], xd, k)
  predy <- predict(kout, valid[,-ycol], TRUE)
  realy <- valid[,ycol]
  if(meanabs) return (mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

#install.packages("regtools")
require(regtools)
set.seed(9999)
xvalknn(mlb, 5, c(4,6), 25, 0.7)

####similar results to our linear regression
set.seed(9999)
xvalknn(mlb, 5, c(4,6), 17, 0.7)  ##slightly better cross validation results for a different k 


####  Q2
######################################################


library(freqparcoord)
data(prgeng)

prgeng$age2<-prgeng$age^2
edu<-prgeng$educ
prgeng$ms<-as.integer(edu==14)
prgeng$phd<-as.integer(edu==16)
prgeng$fem<-prgeng$sex-1
tmp<-prgeng[edu>= 13,]
pe<-tmp[,c(1,12,9,13,14,15,8)]
pe<-as.matrix(pe)

reg1<-lm(wageinc~age+age2+wkswrkd+ms+phd+fem,data=prgeng)
summary(reg1)


prgeng$msfem<-prgeng$ms*prgeng$fem
prgeng$phdfem<-prgeng$phd*prgeng$fem


reg2<-lm(wageinc~age+age2+wkswrkd+ms+phd+fem+msfem+phdfem,data=prgeng)
summary(reg2)

##another way to create interaction terms. confirm same output.

reg3<-lm(wageinc~age+age2+wkswrkd+ms+phd+fem+ms:fem+phd:fem,data=prgeng)
summary(reg3)


#### Q3
##################################################

#install.packages("mfp")
require(mfp)
data(bodyfat)
df <- bodyfat[,4:17]

reg_bf1 <- lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + 
              forearm + wrist, data = bodyfat)

summary(reg_bf1)

###need to iteratively remove variables with high p-values (alpha = 0.1). Removed height, chest, knee, ankle, biceps

reg_bf2 <- lm(density ~ age + weight + neck + abdomen + hip + thigh + forearm + wrist, data = bodyfat)

summary(reg_bf2)

### 72% of the variation in bodyfat percentage is explained by 
### variation in age, weight, neck, abdomen, hip, thigh, forearm and wrist measurements.

outlierTest(reg_bf2) # Bonferonni p-value for most extreme obs. 
qqPlot(reg_bf2, main="QQ Plot") #qq plot for studentized resid.  Not too bad, no serious indication of non-normality.
leveragePlots(reg_bf2) ##shows the unique effect of each term in the model.

### We can actually drop everything except for weight, abdomen, and thigh and our model's predictive power stays almost the same.

reg_bf3<-lm(density ~ weight + abdomen + thigh, data = bodyfat)
summary(reg_bf3)

###  In fact, 71% of variation in density is explained by these 3 variables.



#### Q4
##################################################

# 4a. The overall mean height is the weighted average of the gender mean height, with the weight for each gender being 
# its proportion of the total population

# 4b. The overall proportion of people taller than 70 inches is the weighted average of the proportion of people 
# over 70 inches in each gender, with weight for each gender being its proportion of the total population taller than 70 inches.



#######################################################################################################
########################## CH. 2, Questions 1-4 #######################################################

#### Q1
##################################################

library(freqparcoord)
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[ ,c(1 ,12 ,9 ,13 ,14 ,15 ,8)]
pe <- as.matrix(pe)

reg<-lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = prgeng)
reg


### 1a. Form an approximate 95% confidence interval for Beta6 in the model

fem_coeff <- summary(reg)$coefficients[7, 1]
fem_se <- summary(reg)$coefficients[7, 2]

lcl_fem <- fem_coeff - 1.96*fem_se
ucl_fem <- fem_coeff + 1.96*fem_se

lcl_fem  ## -12,867
ucl_fem  ## -10,102

### 1b. Form an approximate 95% confidence interval for the gender effect
### for Master's degree holders, Beta6+Beta7, in the model.

prgeng$msfem <- prgeng$ms * prgeng$fem
prgeng$phdfem <- prgeng$phd * prgeng$fem
reg2 <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + msfem + phdfem, data = prgeng )

summary(reg2)

fem_coeff2<-summary(reg2)$coefficients[7, 1]
fem_se2<-summary(reg2)$coefficients[7, 2]

msfem_coeff<-summary(reg2)$coefficients[8, 1]
msfem_se<-summary(reg2)$coefficients[8, 2]


lcl_msfem <- (fem_coeff2 - 1.96*fem_se2) + (msfem_coeff - 1.96*msfem_se)
ucl_msfem <- (fem_coeff2 + 1.96*fem_se2) + (msfem_coeff + 1.96*msfem_se)

lcl_msfem ## -19,398
ucl_msfem  ## -9,469


#### Q2
##################################################

shar <- read.csv("day.csv", sep = ",")
shar$temp2 <- shar$temp^2
reg_shar <- lm(registered ~ temp + temp2 + season + workingday + windspeed + yr, data = shar)
summary(reg_shar)





