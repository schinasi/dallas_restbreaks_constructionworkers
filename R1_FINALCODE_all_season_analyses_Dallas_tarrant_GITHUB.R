
library(dplyr)
library(Epi)
library(scales)
library(ggplot2)
library(zoo)
library(splines)
library(multcomp)


setwd("C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/Data")

# read in the annual claims data. This was prepared at the bottom of the code file titled v2.texas_heatinjury_data_prep_withTarrant.R
data<-read.csv('dallas_tarrant_annual_claimsemployment.csv')

#Prep the data a little bit

#create a time in study variable

# Create a named vector to map month names to numeric values

data <- data %>%
  mutate(
    year = Year,
    pre_post = if_else(year >= 2016, 1, 0)
  ) %>%
  filter(county %in% c("Dallas", "Harris", "Tarrant"))


#adjust the total employed number because not all employers provided workers compensation 

#use the following estimates:
#2012: 81%
#2014: 80%
#2016: 82%
#2018: 82%

#data come from the figure labeled: "Percentage of Texas Private
#Sector Employers That Are
#Subscribers and Texas Employees Employed by Subscribers:
# 1993 2018
#in the following document: https://www.tdi.texas.gov/reports/wcreg/documents/nonsub2022.pdf

data$adj_employ<-ifelse(data$year==2013, (data$Employment*(0.805)),
                        ifelse( data$year==2014, (data$Employment*(0.80)),
                                ifelse(data$year==2015, (data$Employment*(0.81)),
                                       ifelse(data$year==2016, (data$Employment*(0.81)),
                                              ifelse(data$year==2017, (data$Employment*(0.82)),
                                                     ifelse(data$year==2018, (data$Employment*(0.82)),
                                                            ifelse(data$year==2019, (data$Employment*(0.82)),
                                                                   NA)))))))
#subset to before year 2019
data<-subset(data, year<2019)


#keep only the variables we need, to keep things simpler and easier to read 

data<-data[c("year", "pre_post", "adj_employ", "n_claims", "county", "average_tmean", "average_pr" )]


data$intervention_group<-ifelse(data$county=="Dallas", 1,0)

#create a time in study variable 

# Sort the data by the Date variable (if not already sorted)
data <- data %>% arrange(year)

# Calculate the time variable incrementally from 1 to xxx
data <- data %>% 
  group_by(county) %>% 
  mutate(time_since_start=row_number())


#minus one so time since start begins at zero
data$T = data$time_since_start - 1

#minus 3 because we want all dates prior to January 2016 to be coded as 0, 
#and we want incrementally increasing values for time (year) since intervention start past this point

data$time_since_intervention = ifelse(data$pre_post==1, data$T - 3,0)


data <- data %>%
  mutate(
    Z = intervention_group,
    X = pre_post,
    TI=time_since_intervention,
    XT = pre_post * time_since_intervention,
    ZX = pre_post * intervention_group,
    ZXT = XT * intervention_group,
    ZT = intervention_group * time_since_start)

check<-data[c("county",  "year", "T", "X","TI", "Z", "XT", "ZX", "ZT", "ZXT" )]

#######################################################################################
# Start out with the simplest ITS: No control group

dallas<-data[which(data$county=="Dallas"),]

# 

m1 <-glm(n_claims ~ T + X + XT + average_tmean
          , offset=log(adj_employ), 
         data = dallas, family=quasipoisson)

r.m1<-as.data.frame(round(ci.lin(m1,Exp=T),3))

#write.csv(r.m1, 'PATH/20240313_R1.ALLSEASON_simpleITS.csv')

## calculate post intervention slope as sum of b1 + b3, obtain CI.

post.slope.simple<- glht(m1, linfct = c('T + XT = 0'))
o<-confint(post.slope.simple)

#### PASTE OUTPUT : 

      #Linear Hypotheses:
             
#Linear Hypotheses:
 # Estimate  lwr       upr      
#T + XT == 0 -0.059301 -0.120116  0.001514
#RR = exp(-0.059301)
# LL=exp(-0.120116)
# UL=exp(0.001514)

###############################################################################################################################
                                               # CITS: Tarrant county as control #
###############################################################################################################################

dallas_tarrant<-subset(data, county %in% c("Dallas", "Tarrant"))

m2 <-  glm(n_claims ~  T + X + XT + 
             Z + ZT + 
             ZX + ZXT +
            average_tmean , 
           offset=log(adj_employ), 
           data = dallas_tarrant, family=quasipoisson)

r.m2<-as.data.frame(round(ci.lin(m2,Exp=T),3))

#linear combinations

b1<-r.m2[2, 5]
b1_ll<-r.m2[2, 6]
b1_ul<-r.m2[2, 7]

b2<-r.m2[3, 5]
b2_ll<-r.m2[3, 6]
b2_ul<-r.m2[3, 7]

b3<-r.m2[4, 5]
b3_ll<-r.m2[4, 6]
b3_ul<-r.m2[4, 7]

b4<-r.m2[5, 5]
b4_ll<-r.m2[5, 6]
b4_ul<-r.m2[5, 7]

b5<-r.m2[6, 5]
b5_ll<-r.m2[6, 6]
b5_ul<-r.m2[6, 7]

b6<-r.m2[7, 5]
b6_ll<-r.m2[7, 6]
b6_ul<-r.m2[7, 7]

b7<-r.m2[8, 5]
b7_ll<-r.m2[8, 6]
b7_ul<-r.m2[8, 7]

b5_b1 <- glht(m2, linfct = c('ZT + T = 0'))
b5_b1_ci<-confint(b5_b1)

b1_b3 <- glht(m2, linfct = c('T +  XT = 0'))
b1_b3_ci<-confint(b1_b3)

#B1 + B3 + B5 + B7#
b1b3b5b7<-glht(m2, linfct = c('T + 
                              XT + 
                              ZT + 
                              ZXT = 0'))

b1b3b5b7_ci<-confint(b1b3b5b7)

#b5 + B7
b5b7<-glht(m2, linfct = c('ZT + ZXT = 0'))
b5b7_ci<-confint(b5b7)

b3b7<-glht(m2, linfct = c('XT + ZXT = 0'))
b3b7_ci<-confint(b3b7)


# Create a matrix with 10 rows and 3 columns, initialized with NA
num_rows <- 10
num_cols <- 3
result_matrix <- matrix(NA, nrow = num_rows, ncol = num_cols)

rownames(result_matrix)<-c(
  "Pre-intervention trend: control", 
  "Pre-intervention trend: treated",
  "Difference pre-intervention: treated vs control",
  "Difference immediatly after intervention: treated vs. control",
  "Post-intervention trend: control", 
  "Post-intervention trend: treated",
  "Difference post-intervention: treated vs. control", 
  "Difference pre- versus post-intervention: control",
  "Difference pre- versus post-intervention: treated", 
  "Difference pre-versus post-intervention: treated vs. control")

colnames(result_matrix)<-c( "Model parameter", 
                            "Point estimate: Harris control",  "95% CI")

result_matrix[,1]<-c("B1", "B5+B1", "B5", "B6", "B1+B3", "B1+B3+B5+B7",
                     "B5+B7", "B3", "B3+B7", "B7")

#[row, column]

#B1
result_matrix[1,2]<-b1
result_matrix[1,3]<-paste("(", b1_ll, "-", b1_ul, ")")
#B5+B1 (ZT + T)
result_matrix[2,2]<-round(exp(coef(b5_b1)),3)
result_matrix[2,3]<-paste("(", round(exp(b5_b1_ci$confint[,"lwr"]),3), "-", round(exp(b5_b1_ci$confint[,"upr"]),3), ")")

#B5 (ZT)

result_matrix[3,2]<-b5
result_matrix[3,3]<-paste("(", b5_ll, "-", b5_ul, ")")

#B6 (ZX)
result_matrix[4,2]<-b6
result_matrix[4,3]<-paste("(", b6_ll, "-", b6_ul, ")")

#B1+B3 (T+XT)
result_matrix[5,2]<-round(exp(coef(b1_b3)),3)
result_matrix[5,3]<-paste("(", round(exp(b1_b3_ci$confint[,"lwr"]),3), "-", round(exp(b1_b3_ci$confint[,"upr"]),3), ")")
#B1 + B3 + B5 + B7#
result_matrix[6,2]<-round(exp(coef(b1b3b5b7)),3)
result_matrix[6,3]<-paste("(", round(exp(b1b3b5b7_ci$confint[,"lwr"]),3), "-", round(exp(b1b3b5b7_ci$confint[,"upr"]),3), ")")
#b5+B7
result_matrix[7,2]<-round(exp(coef(b5b7)),3)
result_matrix[7,3]<-paste("(", round(exp(b5b7_ci$confint[,"lwr"]),3), "-", round(exp(b5b7_ci$confint[,"upr"]),3), ")")


#B3
result_matrix[8,2]<-b3
result_matrix[8,3]<-paste("(", b3_ll, "-", b3_ul, ")")


#b3b7
result_matrix[9,2]<-round(exp(coef(b3b7)),3)
result_matrix[9,3]<-paste("(", round(exp(b3b7_ci$confint[,"lwr"]),3), "-", round(exp(b3b7_ci$confint[,"upr"]),3), ")")
#b7
result_matrix[10,2]<-b7
result_matrix[10,3]<-paste("(", b7_ll, "-", b7_ul, ")")


####################################################################################################################
#write.csv(r.m2, 'PATH/R1.allseason_CITS_dallastarrant.csv')

#write.csv(result_matrix, 'PATH/R1.Allseason_CITS_dallastarrantresultmatrix.csv' )
####################################################################################################################

#############################################################################################################################
                                         
                                           ### SENSITIVITY ANALYSES #######

###############################################################################################################################

# start intervention 2015 rather than 2016

data<-read.csv('dallas_tarrant_annual_claimsemployment.csv')


#Prep the data a little bit

#create a time in study variable

# Create a named vector to map month names to numeric values

data <- data %>%
  mutate(
    year = Year,
    pre_post = if_else(year >= 2015, 1, 0)
  ) %>%
  filter(county %in% c("Dallas",  "Tarrant"))


#adjust the total employed number because not all employers provided workers compensation 

#use the following estimates:
#2012: 81%
#2014: 80%
#2016: 82%
#2018: 82%

#data come from the figure labeled: "Percentage of Texas Private
#Sector Employers That Are
#Subscribers and Texas Employees Employed by Subscribers:
# 1993 2018
#in the following document: https://www.tdi.texas.gov/reports/wcreg/documents/nonsub2022.pdf

data$adj_employ<-ifelse(data$year==2013, (data$Employment*(0.805)),
                        ifelse( data$year==2014, (data$Employment*(0.80)),
                                ifelse(data$year==2015, (data$Employment*(0.81)),
                                       ifelse(data$year==2016, (data$Employment*(0.81)),
                                              ifelse(data$year==2017, (data$Employment*(0.82)),
                                                     ifelse(data$year==2018, (data$Employment*(0.82)),
                                                            ifelse(data$year==2019, (data$Employment*(0.82)),
                                                                   NA)))))))
#subset to before year 2019
data<-subset(data, year<2019)


#keep only the variables we need, to keep things simpler and easier to read 

data<-data[c("year", "pre_post", "adj_employ", "n_claims", "county", "average_tmean")]


data$intervention_group<-ifelse(data$county=="Dallas", 1,0)

#create a time in study variable 

# Sort the data by the Date variable (if not already sorted)
data <- data %>% arrange(year)

# Calculate the time variable incrementally from 1 to xxx
data <- data %>% 
  group_by(county) %>% 
  mutate(time_since_start=row_number())


#minus one so time since start begins at zero
data$T = data$time_since_start - 1

#minus 2 because we want all dates prior to January 2015 to be coded as 0, 
#and we want incrementally increasing values for time (year) since intervention start past this point

data$time_since_intervention = ifelse(data$pre_post==1, data$T - 2,0)


data <- data %>%
  mutate(
    Z = intervention_group,
    X = pre_post,
    TI=time_since_intervention,
    XT = pre_post * time_since_intervention,
    ZX = pre_post * intervention_group,
    ZXT = XT * intervention_group,
    ZT = intervention_group * time_since_start)

check<-data[c("county",  "year", "T", "X","TI", "Z", "XT", "ZX", "ZT", "ZXT", "adj_employ", "n_claims", "average_tmean" )]

#######################################################################################
# Start out with the simplest ITS: No control group

dallas<-data[which(data$county=="Dallas"),]

# 

m1 <-glm(n_claims ~ T + X + XT + average_tmean 
         , offset=log(adj_employ), 
         data = dallas, family=poisson)

r.m1<-as.data.frame(round(ci.lin(m1,Exp=T),3))

#write.csv(r.m1, 'PATH/20240313_R1.ALLSEASON_simpleITS_SENSITIVITYANALYSIS2015start.csv')

## calculate post intervention slope as sum of b1 + b3, obtain CI.
post.slope.simple<- glht(m1, linfct = c('T + XT = 0'))
o<-confint(post.slope.simple)

####
#Linear Hypotheses:

#Linear Hypotheses:
              #Estimate  lwr       upr      
#T + XT == 0 -0.027361 -0.061033  0.006311

# RR=exp(-0.027361)
# LL=exp(-0.061033)
# UL=exp(0.006311)

# COMPARATIVE ITS ## 

####################################################################################################################

# Tarrant county as control #

dallas_tarrant<-subset(data, county %in% c("Dallas", "Tarrant"))

m2 <-  glm(n_claims ~  T + X + XT + 
             Z + ZT + 
             ZX + ZXT +
            average_tmean , 
           offset=log(adj_employ), 
           data = dallas_tarrant, family=poisson)

r.m2<-as.data.frame(round(ci.lin(m2,Exp=T),3))

#linear combinations

b1<-r.m2[2, 5]
b1_ll<-r.m2[2, 6]
b1_ul<-r.m2[2, 7]

b2<-r.m2[3, 5]
b2_ll<-r.m2[3, 6]
b2_ul<-r.m2[3, 7]

b3<-r.m2[4, 5]
b3_ll<-r.m2[4, 6]
b3_ul<-r.m2[4, 7]

b4<-r.m2[5, 5]
b4_ll<-r.m2[5, 6]
b4_ul<-r.m2[5, 7]

b5<-r.m2[6, 5]
b5_ll<-r.m2[6, 6]
b5_ul<-r.m2[6, 7]

b6<-r.m2[7, 5]
b6_ll<-r.m2[7, 6]
b6_ul<-r.m2[7, 7]

b7<-r.m2[8, 5]
b7_ll<-r.m2[8, 6]
b7_ul<-r.m2[8, 7]

b5_b1 <- glht(m2, linfct = c('ZT + T = 0'))
b5_b1_ci<-confint(b5_b1)

b1_b3 <- glht(m2, linfct = c('T +  XT = 0'))
b1_b3_ci<-confint(b1_b3)

#B1 + B3 + B5 + B7#
b1b3b5b7<-glht(m2, linfct = c('T + 
                              XT + 
                              ZT + 
                              ZXT = 0'))

b1b3b5b7_ci<-confint(b1b3b5b7)

#b5 + B7
b5b7<-glht(m2, linfct = c('ZT + ZXT = 0'))
b5b7_ci<-confint(b5b7)

b3b7<-glht(m2, linfct = c('XT + ZXT = 0'))
b3b7_ci<-confint(b3b7)


# Create a matrix with 10 rows and 3 columns, initialized with NA
num_rows <- 10
num_cols <- 3
result_matrix <- matrix(NA, nrow = num_rows, ncol = num_cols)

rownames(result_matrix)<-c(
  "Pre-intervention trend: control", 
  "Pre-intervention trend: treated",
  "Difference pre-intervention: treated vs control",
  "Difference immediatly after intervention: treated vs. control",
  "Post-intervention trend: control", 
  "Post-intervention trend: treated",
  "Difference post-intervention: treated vs. control", 
  "Difference pre- versus post-intervention: control",
  "Difference pre- versus post-intervention: treated", 
  "Difference pre-versus post-intervention: treated vs. control")

colnames(result_matrix)<-c( "Model parameter", 
                            "Point estimate: Harris control",  "95% CI")

result_matrix[,1]<-c("B1", "B5+B1", "B5", "B6", "B1+B3", "B1+B3+B5+B7",
                     "B5+B7", "B3", "B3+B7", "B7")

#[row, column]

#B1
result_matrix[1,2]<-b1
result_matrix[1,3]<-paste("(", b1_ll, "-", b1_ul, ")")
#B5+B1 (ZT + T)
result_matrix[2,2]<-round(exp(coef(b5_b1)),3)
result_matrix[2,3]<-paste("(", round(exp(b5_b1_ci$confint[,"lwr"]),3), "-", round(exp(b5_b1_ci$confint[,"upr"]),3), ")")

#B5 (ZT)

result_matrix[3,2]<-b5
result_matrix[3,3]<-paste("(", b5_ll, "-", b5_ul, ")")

#B6 (ZX)
result_matrix[4,2]<-b6
result_matrix[4,3]<-paste("(", b6_ll, "-", b6_ul, ")")

#B1+B3 (T+XT)
result_matrix[5,2]<-round(exp(coef(b1_b3)),3)
result_matrix[5,3]<-paste("(", round(exp(b1_b3_ci$confint[,"lwr"]),3), "-", round(exp(b1_b3_ci$confint[,"upr"]),3), ")")
#B1 + B3 + B5 + B7#
result_matrix[6,2]<-round(exp(coef(b1b3b5b7)),3)
result_matrix[6,3]<-paste("(", round(exp(b1b3b5b7_ci$confint[,"lwr"]),3), "-", round(exp(b1b3b5b7_ci$confint[,"upr"]),3), ")")
#b5+B7
result_matrix[7,2]<-round(exp(coef(b5b7)),3)
result_matrix[7,3]<-paste("(", round(exp(b5b7_ci$confint[,"lwr"]),3), "-", round(exp(b5b7_ci$confint[,"upr"]),3), ")")


#B3
result_matrix[8,2]<-b3
result_matrix[8,3]<-paste("(", b3_ll, "-", b3_ul, ")")


#b3b7
result_matrix[9,2]<-round(exp(coef(b3b7)),3)
result_matrix[9,3]<-paste("(", round(exp(b3b7_ci$confint[,"lwr"]),3), "-", round(exp(b3b7_ci$confint[,"upr"]),3), ")")
#b7
result_matrix[10,2]<-b7
result_matrix[10,3]<-paste("(", b7_ll, "-", b7_ul, ")")


####################################################################################################################
#write.csv(r.m2, 'PATH/R1.allseason_CITS_dallastarrant_SENSITIVITYSTART2015.csv')

#write.csv(result_matrix, 'PATH/R1.Allseason_CITS_dallastarrantresultmatrixSENSITIVITYSTART2015.csv' )
####################################################################################################################
                              # SENSITIVITY 2: Start intervention in 2017 ###


data<-read.csv('dallas_tarrant_annual_claimsemployment.csv')
###############################################
#Prep the data a little bit

#create a time in study variable

# Create a named vector to map month names to numeric values

data <- data %>%
  mutate(
    year = Year,
    pre_post = if_else(year >= 2017, 1, 0)
  ) %>%
  filter(county %in% c("Dallas","Tarrant"))


#adjust the total employed number because not all employers provided workers compensation 

#use the following estimates:
#2012: 81%
#2014: 80%
#2016: 82%
#2018: 82%

#data come from the figure labeled: "Percentage of Texas Private
#Sector Employers That Are
#Subscribers and Texas Employees Employed by Subscribers:
# 1993 2018
#in the following document: https://www.tdi.texas.gov/reports/wcreg/documents/nonsub2022.pdf

data$adj_employ<-ifelse(data$year==2013, (data$Employment*(0.805)),
                        ifelse( data$year==2014, (data$Employment*(0.80)),
                                ifelse(data$year==2015, (data$Employment*(0.81)),
                                       ifelse(data$year==2016, (data$Employment*(0.81)),
                                              ifelse(data$year==2017, (data$Employment*(0.82)),
                                                     ifelse(data$year==2018, (data$Employment*(0.82)),
                                                            ifelse(data$year==2019, (data$Employment*(0.82)),
                                                                   NA)))))))
#subset to before year 2019
data<-subset(data, year<2019)


#keep only the variables we need, to keep things simpler and easier to read 

data<-data[c("year", "pre_post", "adj_employ", "n_claims", "county", "average_tmean")]


data$intervention_group<-ifelse(data$county=="Dallas", 1,0)

#create a time in study variable 

# Sort the data by the Date variable (if not already sorted)
data <- data %>% arrange(year)

# Calculate the time variable incrementally from 1 to xxx
data <- data %>% 
  group_by(county) %>% 
  mutate(time_since_start=row_number())


#minus one so time since start begins at zero
data$T = data$time_since_start - 1

#minus 4 because we want all dates prior to January 2017 to be coded as 0, 
#and we want incrementally increasing values for time (year) since intervention start past this point

data$time_since_intervention = ifelse(data$pre_post==1, data$T - 4,0)


data <- data %>%
  mutate(
    Z = intervention_group,
    X = pre_post,
    TI=time_since_intervention,
    XT = pre_post * time_since_intervention,
    ZX = pre_post * intervention_group,
    ZXT = XT * intervention_group,
    ZT = intervention_group * time_since_start)

check<-data[c("county",  "year", "T", "X","TI", "Z", "XT", "ZX", "ZT", "ZXT" )]

#######################################################################################
# Start out with the simplest ITS: No control group

dallas<-data[which(data$county=="Dallas"),]

# 

m1 <-glm(n_claims ~ T + X + XT +average_tmean 
         , offset=log(adj_employ), 
         data = dallas, family=quasipoisson)

r.m1<-as.data.frame(round(ci.lin(m1,Exp=T),3))

#write.csv(r.m1, 'C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/results/Revision_1_ALLSEASON/20240313_R1.ALLSEASON_simpleITS_SENSITIVITYANALYSIS2017start.csv')

## calculate post intervention slope as sum of b1 + b3, obtain CI.
post.slope.simple<- glht(m1, linfct = c('T + XT = 0'))
o<-confint(post.slope.simple)

####
#Linear Hypotheses:

#Linear Hypotheses:
              #Estimate lwr      upr     
#T + XT == 0 -0.06053 -0.28430  0.16323

# RR=exp(-0.06053)
# LL=exp(-0.28430)
# UL=exp(0.16323)

# COMPARATIVE ITS ## 

# Tarrant county as control #
dallas_tarrant<-subset(data, county %in% c("Dallas", "Tarrant"))

m2 <-  glm(n_claims ~  T + X + XT + 
             Z + ZT + 
             ZX + ZXT +
            average_tmean , 
           offset=log(adj_employ), 
           data = dallas_tarrant, family=quasipoisson)

r.m2<-as.data.frame(round(ci.lin(m2,Exp=T),3))

#linear combinations

b1<-r.m2[2, 5]
b1_ll<-r.m2[2, 6]
b1_ul<-r.m2[2, 7]

b2<-r.m2[3, 5]
b2_ll<-r.m2[3, 6]
b2_ul<-r.m2[3, 7]

b3<-r.m2[4, 5]
b3_ll<-r.m2[4, 6]
b3_ul<-r.m2[4, 7]

b4<-r.m2[5, 5]
b4_ll<-r.m2[5, 6]
b4_ul<-r.m2[5, 7]

b5<-r.m2[6, 5]
b5_ll<-r.m2[6, 6]
b5_ul<-r.m2[6, 7]

b6<-r.m2[7, 5]
b6_ll<-r.m2[7, 6]
b6_ul<-r.m2[7, 7]

b7<-r.m2[8, 5]
b7_ll<-r.m2[8, 6]
b7_ul<-r.m2[8, 7]

b5_b1 <- glht(m2, linfct = c('ZT + T = 0'))
b5_b1_ci<-confint(b5_b1)

b1_b3 <- glht(m2, linfct = c('T +  XT = 0'))
b1_b3_ci<-confint(b1_b3)

#B1 + B3 + B5 + B7#
b1b3b5b7<-glht(m2, linfct = c('T + 
                              XT + 
                              ZT + 
                              ZXT = 0'))

b1b3b5b7_ci<-confint(b1b3b5b7)

#b5 + B7
b5b7<-glht(m2, linfct = c('ZT + ZXT = 0'))
b5b7_ci<-confint(b5b7)

b3b7<-glht(m2, linfct = c('XT + ZXT = 0'))
b3b7_ci<-confint(b3b7)


# Create a matrix with 10 rows and 3 columns, initialized with NA
num_rows <- 10
num_cols <- 3
result_matrix <- matrix(NA, nrow = num_rows, ncol = num_cols)

rownames(result_matrix)<-c(
  "Pre-intervention trend: control", 
  "Pre-intervention trend: treated",
  "Difference pre-intervention: treated vs control",
  "Difference immediatly after intervention: treated vs. control",
  "Post-intervention trend: control", 
  "Post-intervention trend: treated",
  "Difference post-intervention: treated vs. control", 
  "Difference pre- versus post-intervention: control",
  "Difference pre- versus post-intervention: treated", 
  "Difference pre-versus post-intervention: treated vs. control")

colnames(result_matrix)<-c( "Model parameter", 
                            "Point estimate: Harris control",  "95% CI")

result_matrix[,1]<-c("B1", "B5+B1", "B5", "B6", "B1+B3", "B1+B3+B5+B7",
                     "B5+B7", "B3", "B3+B7", "B7")

#[row, column]

#B1
result_matrix[1,2]<-b1
result_matrix[1,3]<-paste("(", b1_ll, "-", b1_ul, ")")
#B5+B1 (ZT + T)
result_matrix[2,2]<-round(exp(coef(b5_b1)),3)
result_matrix[2,3]<-paste("(", round(exp(b5_b1_ci$confint[,"lwr"]),3), "-", round(exp(b5_b1_ci$confint[,"upr"]),3), ")")

#B5 (ZT)

result_matrix[3,2]<-b5
result_matrix[3,3]<-paste("(", b5_ll, "-", b5_ul, ")")

#B6 (ZX)
result_matrix[4,2]<-b6
result_matrix[4,3]<-paste("(", b6_ll, "-", b6_ul, ")")

#B1+B3 (T+XT)
result_matrix[5,2]<-round(exp(coef(b1_b3)),3)
result_matrix[5,3]<-paste("(", round(exp(b1_b3_ci$confint[,"lwr"]),3), "-", round(exp(b1_b3_ci$confint[,"upr"]),3), ")")
#B1 + B3 + B5 + B7#
result_matrix[6,2]<-round(exp(coef(b1b3b5b7)),3)
result_matrix[6,3]<-paste("(", round(exp(b1b3b5b7_ci$confint[,"lwr"]),3), "-", round(exp(b1b3b5b7_ci$confint[,"upr"]),3), ")")
#b5+B7
result_matrix[7,2]<-round(exp(coef(b5b7)),3)
result_matrix[7,3]<-paste("(", round(exp(b5b7_ci$confint[,"lwr"]),3), "-", round(exp(b5b7_ci$confint[,"upr"]),3), ")")


#B3
result_matrix[8,2]<-b3
result_matrix[8,3]<-paste("(", b3_ll, "-", b3_ul, ")")


#b3b7
result_matrix[9,2]<-round(exp(coef(b3b7)),3)
result_matrix[9,3]<-paste("(", round(exp(b3b7_ci$confint[,"lwr"]),3), "-", round(exp(b3b7_ci$confint[,"upr"]),3), ")")
#b7
result_matrix[10,2]<-b7
result_matrix[10,3]<-paste("(", b7_ll, "-", b7_ul, ")")


####################################################################################################################
#write.csv(r.m2, 'PATH/R1.allseason_CITS_dallastarrant_SENSITIVITYSTART2017.csv')

#write.csv(result_matrix, 'PATH/Revision_1_ALLSEASON/R1.Allseason_CITS_dallastarrantresultmatrixSENSITIVITYSTART2017.csv' )
####################################################################################################################





