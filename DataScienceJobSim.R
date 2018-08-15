# Exploring and analyze the data for the Data Analytics Job Simulation Project
# Here are the instructions:
# https://intersectjobsims.com/library/using-data-to-make-predictions/
# 8/2/18 4pm

# Before I import the data into R, I first looked at all the data in excel
# I made a few changes:
# 1. I noticed that the date column showed the data as date and time
#    All the times were shown as 12am, so I reformatted the column to be date only
# 2. The phone number, address lines and postal code info was hard to compare,
#    because different countries have different systems.
#    Since the state and country codes provide location information, I deleted those columns
# 3. I save the modifies data as a csv file for easier import

# Prep the environment
library(tidyverse)
library(gridExtra)
library(lme4)
setwd("C:/Users/Sheng/Dropbox/JOB/InterSECT_job_sim")

# Import the data
dat_sale <- read_csv("sales_data_sample-1.csv")
str(dat_sale)
dat_sale <- dat_sale %>% mutate (orderdate = parse_date(ORDERDATE, "%m/%d/%Y"))

# Data exploration
# Let's first look at the data distribution for some of the quantitative variables:
phs <- hist(dat_sale$SALES) 
# Mostly normal, with single peak around 3000 and slightly skewed to the right.
phq <- hist(dat_sale$QUANTITYORDERED, breaks = 40) 
# Not normal at all, more like uniform distibution between 20-50 with some outliers
phpr <- hist(dat_sale$PRICEEACH)
# Not normal either, the bar of 100 is much higher than all others
pbn <- ggplot(dat_sale, aes(ORDERLINENUMBER))+geom_bar()
# Very strange, monotonic decrease of counts as orderline# increases
pbd <- ggplot(dat_sale, aes(orderdate))+geom_bar()
# Possibly some seasonal pattern, that there is more sale at the end of the season
pbq <- ggplot(dat_sale, aes(QTR_ID))+geom_bar()
# Yes, more sales in the 4th quarter
pby <- ggplot(dat_sale, aes(YEAR_ID))+geom_bar()
range(dat_sale$orderdate)
# More sales in 2004, but the 2015 data only extend to 2005-05-31
# So I will need to be careful when interpreting data about year
pbc <- ggplot(dat_sale, aes(PRODUCTLINE))+geom_bar()
# A lot more classic cars and much less trains
pbcus <- ggplot(dat_sale, aes(CUSTOMERNAME))+geom_bar()
# Two outlier customers that made >150 orders
pbcity <- ggplot(dat_sale, aes(CITY))+geom_bar()
# Three cities made the most orders
pbcountry <- ggplot(dat_sale, aes(COUNTRY))+geom_bar()
# Most orders from the US
pbdeal <- ggplot(dat_sale, aes(DEALSIZE))+geom_bar()
# Fewer large purchases, more large purchases.

# I can probably make a list of figures automatically using an apply function
# But I will do that later

# Before I model anything, check whether correlations are as expected:
ggplot(dat_sale, aes(I(QUANTITYORDERED*PRICEEACH), SALES))+geom_point()
# There is linear lower limit, but not a higher one
ggplot(dat_sale, aes(SALES, color = DEALSIZE))+geom_bar()
# Sale size is just a categorical division of SALES, so it cannot be part of the model
dat_sale %>%
  group_by(MSRP,PRODUCTCODE) %>%
  summarise(n = n())
length(levels(as.factor(dat_sale$PRODUCTCODE)))
length(levels(as.factor(dat_sale$MSRP)))
# So MSRP is nested in PRODUCTCODE... cannot have both of them in the same model

dat_sale %>%
  group_by(ORDERNUMBER) %>%
  summarise(n = n(), a = mean(SALES), b = sd(SALES))
# Why are order numbers not unique?

# Let's plug everything into a model:
FIT1 <- lmer(data = dat_sale %>% filter (STATUS %in% c("In Process", "Resolved", "Shipped")), 
             SALES ~ scale(QUANTITYORDERED)*scale(PRICEEACH) + scale(ORDERLINENUMBER) + 
                     as.factor(QTR_ID) + as.factor(YEAR_ID) + 
                    (1|PRODUCTLINE) + (1|PRODUCTCODE) + (1|CUSTOMERNAME) + (1|CITY) + (1|STATE) + (1|COUNTRY))
summary(FIT1)
# CustormorName or location (city, state, country) has no effect
# QuantityOrdered and PriceEach had the most significant effects
# Sales in the the fourth quarter was higher, but the effect may not be significant.
# OrderlineNumber was somewhat important. 
# Year was not important.

FIT2 <- update(FIT1, .~. -as.factor(QTR_ID) + as.factor(MONTH_ID))
summary(FIT2)
anova(FIT1, FIT2)
# The two models are essentially the same in terms of predicative power. FIT1 might be slightly better.

FIT3 <- update(FIT1, .~. - scale(ORDERLINENUMBER) + (1|ORDERLINENUMBER))
anova(FIT1, FIT3) # same power
summary(FIT3) # ORDERLINENUMBER has no effect as a random factor

FITR <- lmer( data = dat_sale, 
             SALES ~ scale(QUANTITYORDERED)*scale(PRICEEACH) + (1|ORDERLINENUMBER) + 
                     (1|QTR_ID) + (1|YEAR_ID) + (1|STATUS) +
                    (1|PRODUCTLINE) + (1|PRODUCTCODE) + (1|CUSTOMERNAME) + (1|CITY) + (1|STATE) + (1|COUNTRY))
summary(FITR)
# STATUS is actually kinda important, 
# but PRODUCTCODE AND PRODUCTLINE by far explains more variation than other factors

# FIT1 will be the learning algorithm for the company.

# PART 2. MY OWN QUESTION-----------------------------------------------------------------------------
# I want know which orders are more likely to be canceled. 

