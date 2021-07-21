
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")	
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")	
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")	
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")	
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")	
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
library(scales)	
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

# The training dataset, "productdata", and validation test dataset, "temp", will be created 	
# from a 80%/20% split of the data, respectively.	
set.seed(1, sample.kind="Rounding")	
test_index <- createDataPartition(y = rawdata$score, times = 1, p = 0.2, list = FALSE)	
productdata <- rawdata[-test_index,]	
validation <- rawdata[test_index,]	

# Remove all unnecessary variables	
rm(test_index)


# Summary information of the "rawdata" dataset	
summary(rawdata)	

# Structure of the "rawdata" dataset	
str(rawdata)	

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



productdata_num <- productdata %>% mutate(user.id = as.numeric(str_replace(user.id,"ID","" )),
                                          user.gender=ifelse(user.gender=="male", 1, 0), 
                                          user.nationality=ifelse(user.nationality=="local", 1, 0), 
                                          user.knowledge=as.numeric(recode(productdata$user.knowledge, "low" = 1, "medium" = 2, "high" = 3)),
                                          user.loyalty=as.numeric(recode(productdata$user.loyalty, "new" = 1, "sporadic" = 2, "long-term" = 3)),
                                          user.loan=ifelse(user.loan=="yes", 1, 0),
                                          user.riskAversion=ifelse(user.riskAversion=="high", 1, 0),
                                          user.marital=as.numeric(recode(productdata$user.marital, "single" = 1, "divorced" = 2, "married" = 3)),
                                          product.risk=as.numeric(recode(productdata$product.risk, "low" = 1, "medium" = 2, "high" = 3)),
                                          product.yield=as.numeric(recode(productdata$product.yield, "low" = 1, "medium" = 2, "high" = 3)),
                                          product.type=as.numeric(recode(productdata$product.type, "bond" = 1, "etf" = 2, "stock" = 3)),
                                          transaction.id = as.numeric(str_replace(transaction.id,"T","" )))


rm(productdata1, productdata_num, correlation, numConvGender, pear_correlation,spear_correlation, do_plot_scatter)
str(productdata_num)
summary(productdata_num)

productdata_num_mat <- as.matrix(sapply(productdata_num, as.numeric))

spear_correlation <- cor(productdata_num_mat, method = c("spearman"))
corrplot(spear_correlation, method = "circle", type = 'upper')

pear_correlation <- cor(productdata_num_mat, method = c("pearson"))
corrplot(pear_correlation, method = "circle", type = 'upper')

productdata_num %>% ggplot(aes(x = user.income, y = score)) +
  geom_point(aes(color=score), alpha=0.1) +
  theme_bw()

productdata_num %>% ggplot(aes(x = user.savings, y = score)) +
  geom_point(aes(color=score), alpha=0.1) +
  theme_bw()

productdata_num %>% ggplot(aes(x = score, y = user.age)) +
  geom_point(aes(color=score), alpha=0.1) +
  geom_smooth() + 
  theme_bw()

productdata_num %>% ggplot(aes(x = score, y = user.pension)) +
  geom_point(aes(color=score), alpha=0.1) +
  geom_smooth(method=loess, color="dodgerblue4") +
  scale_y_continuous(labels = comma) + 
  geom_hline(aes(yintercept=mean(user.pension)), color="orange", linetype='dashed', size=1) +	
  theme(panel.border = element_rect(color="black", fill=NA)) 	





d <- dist(productdata_num)
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))
cor(productdata_num)


rawdata_num <- as.matrix(sapply(rawdata, as.numeric))
correlation <- cor(rawdata_num, method = c("pearson"))
corrplot(correlation, method = "circle", type = 'upper')








do_plot_scatter <- function(i) {
  productdata_num %>% ggplot(aes(x = productdata_num[,i], y = productdata_num[,i+1])) +
    geom_point() +
    xlab(paste0(colnames(productdata_num)[i])) +
    ylab(paste0(colnames(productdata_num)[i+1])) +
    theme_bw() +
    scale_color_manual(name = "Correlation?", values = c("black", "red"))
                       # labels = c("Legitimate" = "No", "Fraud" = "Yes"))
}
sapply(seq(2, 20, by = 2), function(x) {plot(do_plot_scatter(x))})







cor(productdata_num[,c("user.id", "user.age", "user.gender", "user.income", "product.type", "product.risk", "transaction.id", "year", "score")])

library(psych)
pairs.panels(productdata_num)

productdata_num_test <- productdata_num %>% select(user.id,
                                                   user.age,
                                                   user.gender,
                                                   user.income,
                                                   user.riskAversion,
                                                   user.marital,
                                                   user.dependents,
                                                   product.type,
                                                   product.risk,
                                                   product.yield,
                                                   year,
                                                   score)
pairs.panels(productdata_num_test)





dt_model <- train(cd_train_labels ~ ., data = cd_train_all, method = "rpart")
# to predict using test dataset
Prediction_dt <-
  predict(dt_model, newdata = cd_test)
# to calculate model accuracy
dt_acc <-
  mean(round(as.numeric(Prediction_dt)) == cd_test_labels)
# append results to the table
model_results <- bind_rows(model_results,
                           tibble(Model = "Decision Tree Model",
                                  Dataset = "cd_test",
                                  Accuracy = round(dt_acc * 100,
                                                   digits = 2)))
model_results %>%
  kable("latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
  row_spec(0, bold = T) %>%
  # to highlight the last row
  row_spec(2:2, bold = T, color = "white", background = "#D7261E")
39



cor(productdata_num_test$score, productdata_num_test$user.age, method = "pearson")
cor.test(productdata_num_test$score, productdata_num_test$user.age, method = "pearson")


cor(productdata_num_test$score, productdata_num_test$user.gender, method = "spearman")
cor.test(productdata_num_test$score, productdata_num_test$user.gender, method = "spearman")

productdata_num_test %>% ggplot(aes(x=user.age, y=score)) +
  geom_point() +
  geom_smooth(method = "loess")

pairs(productdata_num_test[,c("user.age", "score")])


productdata_num_test %>% filter(user.age==41) %>% group_by(user.age)