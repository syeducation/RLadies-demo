##############################################################
##### This is a Key for conducting all analyses for      #####
##### Learning Phase 3 - Application with Real Data      #####
##### of the Getting Started With R When You Know        #####
##### Absolutely Nothing tutorial, https://osf.io/9gq4a/ #####
##############################################################

##############################################################
####  beginning notes: #### 
# 1. set working directory
# 2. install needed packages if you have not already
# do both of these in the console not in this script


##############################################################
#### step 1: load packages required for tutorial #### 
# including these at the beginning helps you stay organized

library(psych)
library(summarytools)
library(dplyr)
library(ggplot2)


##############################################################
#### step 2: read in csv file and check for errors ####
# because the file is in the working directory no need to specify path

eid_dat <- read.csv("EID_Data_MCAE2016.csv")

# several different ways to check if the data read in correctly

dim(eid_dat)

names(eid_dat)

head(eid_dat, n = 5)

View(eid_dat)

# everything looks good. 


##############################################################
#### step 3: use the summarytools package to examine data ####
# checking frequencies and descriptives
# note the good habit of specificying the package prior to the function using ::

summarytools::freq(eid_dat)

summarytools::descr(eid_dat)

# if you want to check the descriptives for just one variable in a dataset

summarytools::descr(eid_dat$meim01)

# this is not in the tutorial instructions, but below is the command to get a cross-classification table

summarytools::ctable(eid_dat$usborn, eid_dat$firstgen)


##############################################################
#### step 4: use the dplyr package to caculate scale scores for MEIM and SWL ####
#and saving them in the dataset

eid_dat <- eid_dat %>% 
  dplyr::mutate(swl_mean = (swl01+swl02+swl03+swl04+swl05)/5, 
                            meim_ex_mean = (meim01+meim02+meim04+meim08+meim10)/5,
                            meim_co_mean = (meim03+meim05+meim06+meim07+meim09+meim11+meim12)/7)


# always good to check that things are as they should when you add/change anything in the dataset

dim(eid_dat)

head(eid_dat, n = 5)

View(eid_dat)

# you may also want to  export to a new data file that you can use for other purposes. The code below will save the file in your working directory with variable names in the top row. 

write.csv(eid_dat, "EID_Data_MCAE2016_Scales.csv")

# examine descriptives of newly created variables
# here  we are combining the dplyr and summarytools packages to subset the data and then get descriptives

eid_dat %>% dplyr::select(swl_mean, meim_ex_mean, meim_co_mean) %>% summarytools::descr()


##############################################################
#### step 5: use the psych package to get a correlation matrix for the three scale scores ####
# again using dplyr as well as psych

eid_dat %>%  dplyr::select(swl_mean, meim_ex_mean, meim_co_mean) %>% psych::corr.test()

# or specify a specific correlation

psych::corr.test(eid_dat$swl_mean, eid_dat$meim_ex_mean)

# in the spirit of the fourth law of learning R, here is another way to do the same thing using dplyr

eid_dat %>% dplyr::select(swl_mean, meim_ex_mean) %>% cor(use="pairwise")

# and yet another way using base R

cor(x = eid_dat$swl_mean, y = eid_dat$meim_ex_mean, use="pairwise")


##############################################################
#### step 6: multiple regression with exploration and commitment predicting SWL ####
# this function is in base R so no package to call

regression <- lm(swl_mean ~ meim_ex_mean + meim_co_mean, data=eid_dat) 
summary(regression)
confint(regression)


##############################################################
#### step 7: create scattterplots using ggplot2 ####

# scatterplot of exploration with SWL
# there are many more options you can play with for customization. I just included some basics here

ex_swl_scatter <-  ggplot2::ggplot(eid_dat, aes(meim_ex_mean, swl_mean)) +
  geom_point() +
  ggtitle("My Scatterplot") +
  xlab("Ethnic Identity Exploration (mean)") +
  ylab("Satisfaction with Life (mean)") +
  geom_smooth(method="lm")
ex_swl_scatter

#scatterplot of commitment with SWL

co_swl_scatter <-  ggplot2::ggplot(eid_dat, aes(meim_co_mean, swl_mean)) +
  geom_point() +
  ggtitle("My Scatterplot") +
  xlab("Ethnic Identity Commitment (mean)") +
  ylab("Satisfaction with Life (mean)") +
  geom_smooth(method="lm")
co_swl_scatter


##############################################################
#### step 8: indepedent samples t-tests ####
# also in base R. note that Welch's t-test is the default

# this set is testing for mean differences by whether they were born in the U.S. 

t.test(eid_dat$meim_ex_mean ~ eid_dat$usborn)
t.test(eid_dat$meim_co_mean ~ eid_dat$usborn)
t.test(eid_dat$swl_mean ~ eid_dat$usborn)

# this set is testing for mean differences by whether they were first gen college students
# note this difference between this code and the previous - only the final variable name

t.test(eid_dat$meim_ex_mean ~ eid_dat$firstgen)
t.test(eid_dat$meim_co_mean ~ eid_dat$firstgen)
t.test(eid_dat$swl_mean ~ eid_dat$firstgen)


##############################################################
#### step 9: create a series of boxplots to go with the previous t-tests ####

born_ex_boxplot <- ggplot2::ggplot(eid_dat, aes(usborn, meim_ex_mean, fill=usborn)) +
  geom_boxplot(alpha = 0.5)
born_ex_boxplot

born_co_boxplot <- ggplot2::ggplot(eid_dat, aes(usborn, meim_co_mean, fill=usborn)) +
  geom_boxplot(alpha = 0.5)
born_co_boxplot

born_swl_boxplot <- ggplot2::ggplot(eid_dat, aes(usborn, swl_mean, fill=usborn)) +
  geom_boxplot(alpha = 0.5)
born_swl_boxplot

born_ex_boxplot <- ggplot2::ggplot(eid_dat, aes(firstgen, meim_ex_mean, fill=firstgen)) +
  geom_boxplot(alpha = 0.5)
born_ex_boxplot

born_co_boxplot <- ggplot2::ggplot(eid_dat, aes(firstgen, meim_co_mean, fill=firstgen)) +
  geom_boxplot(alpha = 0.5)
born_co_boxplot

born_swl_boxplot <- ggplot2::ggplot(eid_dat, aes(firstgen, swl_mean, fill=firstgen)) +
  geom_boxplot(alpha = 0.5)
born_swl_boxplot


##############################################################
#### step 10: run a series of 2x2 ANOVAs ####
# also in base R

anova_ex <- aov(meim_ex_mean ~ firstgen*usborn, data=eid_dat)
summary(anova_ex)

anova_co <- aov(meim_co_mean ~ firstgen*usborn, data=eid_dat)
summary(anova_co)

anova_swl <- aov(swl_mean ~ firstgen*usborn, data=eid_dat)
summary(anova_swl)


##############################################################
#### step 11: create a series of boxplots to go with the previous ANOVAs ####

# note that the code here is only slightly different from the one variable case
# you can/should add elements to format the plots, as you did previously

borngen_ex_boxplot <- ggplot2::ggplot(eid_dat, aes(usborn, meim_ex_mean, fill=firstgen)) +
  geom_boxplot(alpha = 0.5)
borngen_ex_boxplot

borngen_co_boxplot <- ggplot2::ggplot(eid_dat, aes(usborn, meim_co_mean, fill=firstgen)) +
  geom_boxplot(alpha = 0.5)
borngen_co_boxplot

borngen_swl_boxplot <- ggplot2::ggplot(eid_dat, aes(usborn, swl_mean, fill=firstgen)) +
  geom_boxplot(alpha = 0.5)
borngen_swl_boxplot

