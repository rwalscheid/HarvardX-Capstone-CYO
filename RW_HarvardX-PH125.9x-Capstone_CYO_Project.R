
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")	
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")	
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")	
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")	
#if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")	
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")	
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")	
#if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")	
#if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")	
#if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")	

# Load the necessary libraries for this project.	
library(tidyverse)	
library(caret)	
library(data.table)	
library(corrplot)
#library(scales)	
#library(lubridate)	
library(dplyr)	
#library(gridExtra)	
#library(kableExtra)	
#library(recosystem)	


dl <- tempfile()	
download.file("https://github.com/rwalscheid/HarvardX-Capstone-CYO/blob/main/dataset.csv", dl)	


rawdata <- read.csv("dataset.csv", quote = "",
                        row.names = NULL,
                        stringsAsFactors = FALSE)

# Summary information of the "rawdata" dataset	
summary(rawdata)	

# Structure of the "rawdata" dataset	
str(rawdata)	

# The training dataset, "productdata", and validation test dataset, "temp", will be created 	
# from a 80%/20% split of the data, respectively.	
set.seed(1, sample.kind="Rounding")	
test_index <- createDataPartition(y = rawdata$score, times = 1, p = 0.2, list = FALSE)	
productdata <- rawdata[-test_index,]	
validation <- rawdata[test_index,]	

# Remove all unnecessary variables	
rm(test_index)

rawdata %>% group_by(user.gender) %>%
  ggplot(aes(x=user.gender)) +
  geom_bar(aes(fill=user.gender),
           position="dodge")

rawdata %>% group_by(user.nationality) %>%
  ggplot(aes(x=user.nationality)) +
  geom_bar(aes(fill=user.nationality),
           position="dodge")

rawdata %>% group_by(user.loyalty) %>%
  ggplot(aes(x=user.loyalty)) +
  geom_bar(aes(fill=user.loyalty),
           position="dodge")

rawdata %>% group_by(user.riskAversion) %>%
  ggplot(aes(x=user.riskAversion)) +
  geom_bar(aes(fill=user.riskAversion),
           position="dodge")

rawdata %>% group_by(user.knowledge) %>%
  ggplot(aes(x=user.knowledge)) +
  geom_bar(aes(fill=user.knowledge),
           position="dodge")

rawdata %>% group_by(product.risk) %>%
  ggplot(aes(x=product.risk)) +
  geom_bar(aes(fill=product.risk),
           position="dodge")


numConvGender <- function(x){
  ifelse(x=="male",1,0)
}

productdata_num <- productdata %>% mutate(user.gender=ifelse(user.gender=="male", 1, 0), 
                                          user.nationality=ifelse(user.gender=="local", 1, 0), 
                                          user.knowledge=as.numeric(recode(productdata_num$user.knowledge, "low" = 1, "medium" = 2, "high" = 3)),
                                          user.loyalty=as.numeric(recode(productdata_num$user.loyalty, "new" = 1, "sporadic" = 2, "long-term" = 3)),
                                          user.loan=ifelse(user.loan=="yes", 1, 0),
                                          user.riskAversion=ifelse(user.riskAversion=="high", 1, 0),
                                          user.marital=as.numeric(recode(productdata_num$user.marital, "single" = 1, "divorced" = 2, "married" = 3)))

productdata_num$user.knowledge <- as.numeric(recode(productdata_num$user.knowledge, 'low' = 1, 'medium' = 2, 'high'= 3))



str(productdata_num)




rawdata_num <- as.matrix(sapply(rawdata, as.numeric))
correlation <- cor(rawdata_num, method = c("pearson"))
corrplot(correlation, method = "circle", type = 'upper')


















