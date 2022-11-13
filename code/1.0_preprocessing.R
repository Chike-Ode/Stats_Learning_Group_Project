##### hii


# Setting the working directory
setwd("C:\\Users\\helle\\OneDrive\\Documents\\R_folder")

df_train = read.csv("GroupProject/Data/CreditGame_TRAIN.csv")
df_test = read.csv("GroupProject/Data/CreditGame_Applications.csv")



## Replace empty by NA in ST_EMPL
df_train$ST_EMPL[df_train$ST_EMPL == ''] <- NA

# Change to categorical variables
df_train$TYP_RES <- as.factor(df_train$TYP_RES)
df_train$ST_EMPL <- as.factor(df_train$ST_EMPL)

# Create variable for

## Print summary of all variables

colSums(is.na(df_train))
summary(df_train)
# We notice that only two variables have missing value: AGE_D and ST_EMPL

## Plot histogram (or barplot) of both variables to understand the distribution
hist(df_train$AGE_D)
barplot(prop.table(table(df_train$ST_EMPL)))

## These two variables might be MAR or MNAR, so we are going to identify their process


###########################################################################################################
###########################################################################################################
################## PRE-PROCESSING ########################################################################


################################ IMPUTATION ###############################

################# CREATING Z VARIABLE

##### Creating z variable for age
df_train$z_age = 0
df_train$z_age[is.na(df_train$AGE_D)] <- 1

##### Creating z variable for employment
df_train$z_emp = 0
df_train$z_emp[is.na(df_train$ST_EMPL)] <- 1

############### REGRESSION OF Y~Z

#### Regression of default ~ z_emp (logit as the response variable is binary)

reg_emp =glm(DEFAULT~z_emp,data=df_train, family='binomial' )
summary(reg_emp)
  # Our z variable for employment is highly statistically significant (p-value of ~0)
  # As seen in class, if the coefficient of Z is significant there is a case of MNAR; default is explained by Z.


#### Regression of default~ z_age (logit as the response variable is binary)

reg_age =glm(DEFAULT~z_age,data=df_train, family='binomial')
summary(reg_age)
  # Our z variable for AGE is not significant at any level
  # As seen in class, we can now perform a regression of the other x variables on z 
  # (this, to identify if it is MAR or MCAR)


#### Regression of z_age ~ other x (except default nd the original age variable)


#reg_age_2 =glm(z_age ~ . -DEFAULT - AGE_D -z_emp -TYP_FIN, data=df_train, family='binomial',na.action=na.omit)
#summary(reg_age_2)
  # this gives an error... so doing it manually below

 
reg_age_2b =glm(z_age ~ NB_EMPT +R_ATD +DUREE +PRT_VAL+REV_BT+REV_NET+TYP_RES+ST_EMPL+MNT_EPAR
               +NB_ER_6MS+NB_ER_12MS+NB_DEC_12MS+NB_OPER+NB_COUR+NB_INTR_1M+NB_INTR_12M +PIR_DEL+
               NB_DEL_30+NB_DEL_60+NB_DEL_90+MNT_PASS+MNT_ACT+MNT_AUT_REN+MNT_UTIL_REN+NB_SATI+MNT_DEMANDE+PROFIT_LOSS,
               data=df_train, family='binomial',na.action=na.omit)
summary(reg_age_2b)

  # We find significance in 3 coefficients: R_ATD, at 90% confidence level, which is the total debt amortization ratio
#NB_INTR_1M at 95% (number of inquiries in the last month), and MNT_UTIL_REN at 95% (total used amount of revolving credit).

  # AGE is MAR, some of the other variables explain Z



###########################################################################################################
###########################################################################################################
################## EXPLORATORY DATA ANALYSIS ############################################################

install.packages("Hmisc")
library(Hmisc)

install.packages("ggplot2")




