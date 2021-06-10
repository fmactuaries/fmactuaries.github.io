# Basics of prediction and classification in R
# Customer Analytics BU.450.760
# Prof M. Hermosilla

########################### 1. R Basics ###################################

## 1.1 setting working directory, list files in directory 
## this is the folder where all our data is (and where we can save files to)
## setwd(" ***path specific to your computer*** ") # this path is specific to each of our computers. You need to determine it for your specific case

### 1.2 clear workspace (all your scripts should have this before the body of codes)
## rm(list = ls()) 

### 1.3 create a variable called "x", modify it, print it on the console
x <- 3  # define variable (could use "<-" or "=")
x <- 2*x # modify variable
print(x) # print variable in console


########################### 2. Loading & preparing data ###################################

### 2.1 Clear workspace and load data into data a frame called "ds"  
### rm(list = ls()) 
ds <- read.csv("SouthAfricanBank_experiment.csv") # for different formats (eg, xls, txt) you will need a different command

### 2.2 quick descriptive statistics
summary(ds) # describes all variables in inputed dataframe

### 2.3 access specific variable in dataframe using "$"
summary(ds$apply) # decribe specific variable in data frame
summary(ds$education)

### 2.4 convert "yes/no" outcome variable into 1/0 format. Call the resulting variable "y"
ds$y <- ifelse(ds$apply=="Yes",1,0) #note that we're using two equal signs (logical condition)

### 2.5 declare education as a factor variable. Store it in "feduc" 
ds$feduc <- as.factor(ds$education)

### 2.6 training/validation split of data
set.seed(177) # fix your machine's "random seed" (for replication)
idx <- sample(2,nrow(ds),replace=TRUE,prob=c(.6,.4)) # "idx" is an index, which tells us which observations were assigned to the training (idx=1) and validation (idx=2) samples 
table(idx) # confirm 60/40 split



########################### 3. Model estimation ###################################
# Recall: estimate on training sample + predict on full sample + evalate based on predictions of validation sample

########## Model 1
### 3.1 Estimate linear model
# y = b0+b1*femalephoto+b2*example 
lm1 <- lm(y ~ femalephoto+example, data=ds[idx==1,]) # "lm1" contains all relevant estimation results
summary(lm1) # view estimation results

### 3.2 predict application probability in the complete sample
ds$prob_apply1 <- predict(lm1,newdata=ds) # the new variable "prob_apply1" is attached to the data frame
hist(ds$prob_apply1[idx==1]) # these are all the predicted probs in the training sample

### 3.3 determine threshold for "Yes/No" prediction based on continuous probability predictions
# There is no one single right way to do this (you could try alternatives that make sense).
# The one necessary requirement, is that the threshold is determined solely on the information of the training sample
# For simplicity, we will say the threshold is the mean of the outcome variable in the test sample
summary(ds$y[idx==1]) # -> threshold will be 0.221 use meannnnnnn


### 3.4 create predicted dichotomic application variable
ds$pred_y1 <- ifelse(ds$prob_apply1>=0.221,1,0)

### 3.5 confusion matrix
table(ds$y[idx==2],ds$pred_y1[idx==2],dnn = c("observed","predicted")) # first variable indexes rows


########## 3.6 Model 2
# y = b0+b1*femalephoto+b2*example+b3*femalecustomer+b4*dormancy+(education factors) 
#       +b7*femalephotoXdormancy+b8*femalephotoXcustomer        
### Generate interacted variables
ds$femalephotoXdormancy <-  ds$femalephoto*ds$dormancy
ds$femalephotoXfemalecustomer <- ds$femalephoto*ds$femalecustomer

### Remaining estimation+prediction+evaluation steps
lm2 <- lm(y ~ femalephoto+example+femalecustomer+dormancy+feduc+femalephotoXdormancy+femalephotoXfemalecustomer, data=ds[idx==1,])  
summary(lm2) # view estimation results
ds$prob_apply2 <- predict(lm2,newdata=ds)  
ds$pred_y2 <- ifelse(ds$prob_apply2>=0.221,1,0) # using the same threshold as for model 1
table(ds$y[idx==2],ds$pred_y2[idx==2],dnn = c("observed","predicted"))  

########## 3.7 Confusion matrices, side by side
table(ds$y[idx==2],ds$pred_y1[idx==2],dnn = c("observed","predicted")) # model 1
table(ds$y[idx==2],ds$pred_y2[idx==2],dnn = c("observed","predicted")) # model 2



############################# 4. Export results ###################################
### 4.1 Suppose that we conclude that we prefer model 2. We will re-estimate the model with all 
# the data, and then export the probability predictions to a csv file

### 4.2 Re-estimate final model using all the data
lm3 <- lm(y ~ femalephoto+example+femalecustomer+dormancy+feduc+femalephotoXdormancy+femalephotoXfemalecustomer, data=ds)  
ds$prob_apply3 <- predict(lm3,newdata=ds)  
hist(ds$prob_apply3)

### 4.3 We want to insure that all predicted probabilities are within [0,1]
ds$prob_apply3 <- ifelse(ds$prob_apply3<0,0,ds$prob_apply3)
ds$prob_apply3 <- ifelse(ds$prob_apply3>1,1,ds$prob_apply3)

### 4.4 Export to csv file
write.csv(ds, file = "C_analytics_class4.csv",row.names=TRUE) 











 