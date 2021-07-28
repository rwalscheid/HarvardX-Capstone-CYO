#######################################################################################
# Title: HarvardX PH125.9x Data Science: Capstone - CYO - Stroke Prediction System
# Author: Robert Walscheid	
# Date: 07/28/2021
#######################################################################################
#
#######################################################################################
# 1.) PREREQUISITES	###################################################################
#######################################################################################
#
# Download and install the necessary packages for this project.	
if(!require(tidyverse)) + 	
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")	
if(!require(caret)) +	
  install.packages("caret", repos = "http://cran.us.r-project.org")	
if(!require(data.table)) +	
  install.packages("data.table", repos = "http://cran.us.r-project.org")	
if(!require(scales)) +	
  install.packages("scales", repos = "http://cran.us.r-project.org")	
if(!require(corrplot)) +	
  install.packages("corrplot", repos = "http://cran.us.r-project.org")	
if(!require(dplyr)) +	
  install.packages("dplyr", repos = "http://cran.us.r-project.org")	
if(!require(grid)) +	
  install.packages("gridExtra", repos = "http://cran.us.r-project.org")	
if(!require(grid)) +	
  install.packages("gridExtra", repos = "http://cran.us.r-project.org")	
if(!require(kableExtra)) +	
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")	
if(!require(stats)) 	
  install.packages("stats", repos = "http://cran.us.r-project.org")	
	
# Load the necessary libraries for this project.	
library(tidyverse)	
library(caret)	
library(data.table)	
library(scales)	
library(corrplot)	
library(dplyr)	
library(grid)	
library(gridExtra)	
library(kableExtra)	
library(stats)	

# Download the Stroke-Data.csv file from GitHub, create the data frame "rawdata", 	
# and fill it with the 12 columns of data that were in the  file, labeling them	
# id, gender, age, hypertension, heart_disease, ever_married, work_type,	
# Residence_type, bmi, avg_glucose_level, smoking_status, and stroke, 	
# respectively.	
rawdata <- read.csv("https://raw.githubusercontent.com/rwalscheid/HarvardX-Capstone-CYO/main/Stroke-Data.csv",	
                    quote = "",	
                    row.names = NULL,	
                    stringsAsFactors = FALSE)	

# The .csv file will automatically be downloaded to your computer's temporary directory
# and read into a data frame, which contains the necessary records for this project.	

# Summary information of the "rawdata" dataset	
summary(rawdata)	

# Structure of the "rawdata" dataset	
str(rawdata)	

# View the multiple categories for each applicable variable to determine factor levels 	
# to convert to.	
unique(rawdata$gender)	
unique(rawdata$ever_married)	
unique(rawdata$work_type)	
unique(rawdata$Residence_type)	
unique(rawdata$smoking_status)	

# View the gender count for the category "Other".	
rawdata %>% filter(gender %in% "Other") %>% group_by(gender) %>% summarize(n=n())	

# Create Factor variable table with ordered labels and levels	
factor_variables <- c("gender","",	
                      "hypertension","",	
                      "heart_disease","",	
                      "ever_married","",	
                      "work_type","","","","",	
                      "Residence_type","",	
                      "smoking_status","","","",	
                      "stroke","")	
factor_labels_and_levels <- c("Male","Female",	
                              "wo_hypertension = 0","w_hypertension = 1",	
                              "wo_heart_disease = 0", "w_heart_disease = 1",	
                              "Yes", "No",	
                              "Underage_child = children", 	
                              "Never_worked",	
                              "Self-employed",	
                              "Private_company = Private",	
                              "Govt_job",	
                              "Rural", "Urban",	
                              "never smoked",	
                              "formerly smoked",	
                              "smokes",	
                              "Unknown",	
                              "no_stroke = 0", "stroke = 1")	
kable(tibble(factor_variables, factor_labels_and_levels),	
      col.names = c("Variables", "Factor Levels")) %>%	
  row_spec(0,background="#104E8B", color = "white") %>% 	
  column_spec(1, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position = "center",	
                latex_options="HOLD_position")	

# Remove the single "Other" gender row to not skew results, convert the BMI to numeric,  	
# and convert "gender", "hypertension", "heart_disease", "ever_married", "work_type", 	
# "Residence_type", "smoking_status", and "stroke" all to factors with their respective 	
# levels.	
stroke_data <- rawdata %>% filter(gender!="Other") %>% 	
  mutate(gender = factor(gender, levels = c("Male", "Female")),	
         hypertension = factor(hypertension, 	
                               labels = c("wo_hypertension", "w_hypertension"), 	
                               levels = c("0","1")),	
         heart_disease = factor(heart_disease, 	
                                labels = c("wo_heart_disease", "w_heart_disease"), 	
                                levels = c("0","1")),	
         ever_married = factor(ever_married, 	
                               labels = c("never_married", "is_or_was_married"), 	
                               levels = c("No","Yes")),	
         work_type = factor(work_type, 	
                            labels = c("Underage_child", 	
                                       "Never_worked", 	
                                       "Self-employed", 	
                                       "Private_company", 	
                                       "Govt_job"),	
                            levels = c("children",	
                                       "Never_worked", 	
                                       "Self-employed", 	
                                       "Private", 	
                                       "Govt_job")),	
         Residence_type = factor(Residence_type, 	
                                 levels = c("Rural", "Urban")),	
         bmi = as.numeric(bmi),	
         smoking_status = factor(smoking_status, 	
                                 levels = c("never smoked", 	
                                            "formerly smoked", 	
                                            "smokes", 	
                                            "Unknown")),	
         stroke = factor(stroke, 	
                         labels = c("no_stroke", "stroke"), 	
                         levels = c("0","1")))	

# Summary information of the "stroke_data" dataset	
summary(stroke_data)	

# Structure of the "stroke_data" dataset	
str(stroke_data)	

# Calculate the number of bmi N/A entries as a percentage of the total.	
bmi_na_num <- sum(is.na(stroke_data$bmi))	
bmi_na_num	
	
bmi_na_pct_tot <- (bmi_na_num/NROW(stroke_data$bmi))*100	
bmi_na_pct_tot	

# Calculate statistical significance the stroke-positive vs. stroke-negative NA's 	
# would have on the predictability.	
bmi_na_pct_stroke <- 	
  (length(which(is.na(stroke_data$bmi) & stroke_data$stroke == "stroke"))/	
     NROW(stroke_data$bmi))*100     	
bmi_na_pct_stroke #Stroke-Positive Percentage	
	
bmi_na_pct_nostroke <- 	
  (length(which(is.na(stroke_data$bmi) & stroke_data$stroke == "no_stroke"))/	
     NROW(stroke_data$bmi))*100	
bmi_na_pct_nostroke #Stroke-Negative Percentage	

# Calculate the mean bmi in the dataset for all non-"NA" bmi entries that were also 	
# stroke-negative.	
ns_bmi_mu <- as.numeric(stroke_data %>% filter(!is.na(bmi) & stroke=="no_stroke") %>% 	
  summarize(bmi_mu=mean(bmi)))	
	
# Calculate the mean bmi in the dataset for all non-"NA" bmi entries that were also 	
# stroke-positive.	
s_bmi_mu <- as.numeric(stroke_data %>% filter(!is.na(bmi) & stroke=="stroke") %>% 	
  summarize(bmi_mu=mean(bmi)))	
	
# Replace all bmi "NA" entries with the stroke-positive or stroke-negative mean bmi 	
# values, accordingly.	
stroke_data <- stroke_data %>% 	
  mutate(bmi = case_when(is.na(bmi) & stroke == "stroke" ~ s_bmi_mu, 	
                         is.na(bmi) & stroke == "no_stroke" ~ ns_bmi_mu, 	
                         TRUE ~ bmi))	

# While the factor variables in the `stroke_data` dataset allow for easier graphical
# analysis, correlation coefficient functions like `cor()`, `cor.test()`, and 
# `corrplot()` will be used in later sections for analyzing variable importance.  
# This requires that the factors be converted into numeric format.  The `stroke_data`
# dataset will be copied into a new dataset called `stroke_data_num` with the
# exception of the `id` column, since that variable is not necessary for this 
# purpose, and all necessary variables will be converted accordingly.	
# 	
# Create the fully-numeric "stroke_data_num" dataset, copying the original
# "stroke_data" dataset and convert all factor variables.
stroke_data_num <- stroke_data %>% mutate(gender=as.numeric(gender),	
                                          hypertension=as.numeric(hypertension),	
                                          heart_disease=as.numeric(heart_disease),	
                                          ever_married=as.numeric(ever_married),	
                                          work_type=as.numeric(work_type),	
                                          Residence_type=as.numeric(Residence_type),	
                                          smoking_status=as.numeric(smoking_status),	
                                          stroke=as.numeric(stroke)) %>%	
  select(-id)	 #Leave out the "id" column as it is not needed for correlation analysis

# Structure of the "stroke_data_num" dataset	
str(stroke_data_num)	

# The training dataset, "stroke_train", and validation test dataset, 	
# "stroke_test", will be created from an 80%/20% split of the "stroke_data"	
# data frame, respectively.	
set.seed(1, sample.kind="Rounding")	
test_index <- createDataPartition(y = stroke_data$stroke, times = 1, p = 0.2, list = FALSE)	
stroke_train <- stroke_data[-test_index,]	
stroke_test <- stroke_data[test_index,]	

# Summary information for the "stroke_train" dataset.	
summary(stroke_train)	
	
# Summary information for the "stroke_test" dataset.	
summary(stroke_test)	

#######################################################################################
# 2.) DATASET ANALYSIS ################################################################
#######################################################################################
#'
#' Reviewing the datasets in raw format, along with creating correlative visual aids is
#' necessary when comparing and contrasting the data.  Basic analysis of each dataset
#' has been done following their creation in previous sections.  This section will 
#' focus on studying the data itself and any correlations that can aid in model selection.	

# Get a quick glimpse of the data in stroke_data.	
glimpse(stroke_data, width=80)	

# In **Figure 4.4.1** below, a grid of eight bar graphs shows a graphical breakdown of
# all factors and their individual category (level) quantities.  The Male/Female ratio 
# is split fairly close.  The distribution of individuals diagnosed with hypertension,
# heart_disease, and/or stroke is very imbalanced, which will require further 
# correlative review among the other predictors to determine any statistical 
# significance.  The `smoking_status` factor has a fairly large quantity of
# individuals in the `Unknown` category (where their status was unavailable at time
# of entry), which could skew the correlation coefficient between it and `stroke`. 
# The residence type is split almost 50/50 between `Rural` and `Urban`, which means 
# that predictor will have little to no impact on the prediction model.

# Gender Count Chart	
chart_gender_count <- stroke_data %>% group_by(gender) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=gender, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 4000, by=1000), labels = comma, limits = c(0,3500)) +		
  labs(x = "Gender Count",		
       y = "",		
       title = "Gender Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
#Hypertension Count Chart	
chart_hypertension_count <- stroke_data %>% group_by(hypertension) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=hypertension, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 6000, by=1000), labels = comma, limits = c(0,6000)) +		
  labs(x = "Hypertension Count",		
       y = "",		
       title = "Hypertension Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Heart Disease Count Chart	
chart_heart_disease_count <- stroke_data %>% group_by(heart_disease) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=heart_disease, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 6000, by=1000), labels = comma, limits = c(0,6000)) +		
  labs(x = "Heart Disease Cases",		
       y = "",		
       title = "Heart Disease Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Ever Married Count Chart	
chart_ever_married_count <- stroke_data %>% group_by(ever_married) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=ever_married, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 5000, by=1000), labels = comma, limits = c(0,5000)) +		
  labs(x = "History of Marriage",		
       y = "",		
       title = "History of Marriage Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
#Work Type Count Chart	
chart_work_type_count <- stroke_data %>% group_by(work_type) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=work_type, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 4000, by=1000), labels = comma, limits = c(0,3500)) +		
  labs(x = "Type of Work",		
       y = "",		
       title = "Work Type Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Residence Type Count Chart	
chart_Residence_type_count <- stroke_data %>% group_by(Residence_type) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=Residence_type, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 4000, by=1000), labels = comma, limits = c(0,3500)) +		
  labs(x = "Type of Residence",		
       y = "",		
       title = "Residence Type Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Smoking Status Count Chart	
chart_smoking_status_count <- stroke_data %>% group_by(smoking_status) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=smoking_status, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 4000, by=1000), labels = comma, limits = c(0,3500)) +		
  labs(x = "Smoking Status",		
       y = "",		
       title = "Smoking Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Stroke Count Chart	
chart_stroke_count <- stroke_data %>% group_by(stroke) %>%		
  summarize(n=n()) %>%		
  ggplot(aes(x=stroke, y=n)) +		
  geom_bar(stat="identity", color = "orange", fill = "dodgerblue4") + 		
  scale_y_continuous(breaks = seq(0, 6000, by=1000), labels = comma, limits = c(0,6000)) +		
  labs(x = "Stroke History",		
       y = "",		
       title = "Stroke Distribution") +		
  geom_text(aes(label=n), vjust=-0.5, size=6) +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Arrange the eight charts into 2-column/4-row structure and add a caption.
grid.arrange(chart_gender_count, 	
             chart_hypertension_count, 	
             chart_heart_disease_count,	
             chart_ever_married_count,	
             chart_work_type_count,	
             chart_Residence_type_count,	
             chart_smoking_status_count,	
             chart_stroke_count,	
             ncol = 2,	
             nrow = 4,	
             bottom = textGrob(	
               "Source Data: stroke_data\nFigure 4.4.1",	
               gp = gpar(fontface = 3, fontsize = 10),	
               hjust = 1,	
               x = 1	
               )	
             )	

# In **Figure 4.4.2** below, a grid of six charts shows a graphical comparison of
# various predictors as they relate to patient age.  When reviewing the charts 
# showing hypertension, heart disease, and history of stroke as it relates to 
# patient age, those predictors have little prevalence in patients under 35 years 
# old.  	
# 	
# The `Unknown` smoking status category, when stratified by age, shows a trend in
# minors that can be explained by the parent/guardian of the patient viewing the 
# smoking section of the doctor's/hospital admission forms as "not applicable".  
# The relevant age of the `smoking_status` predictor, unlike hypertension and 
# heart disease, starts around age 15 as that is when the `smokes` category 
# increases rapidly.  	
# 	
# The age distribution by gender shows a similar curve between Male/Female from 
# ages 30 and up, but there are more Male patients that are minors.  	

# Histogram of all patients by their age.	
chart_age <- stroke_data %>% group_by(age) %>% 	
  ggplot(aes(age)) +		
  geom_histogram(binwidth = 1, color = "orange", fill = "dodgerblue4") +		
  scale_x_continuous(breaks = seq(0, 100, by=5)) +	
  labs(x = "Age (in years)",	
       y = " ",	
       title = "Age Distribution (all patients)") + 	
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Bar graph of those with hypertension distributed by their age.	
chart_hypertension_by_age <- stroke_data %>% filter(hypertension=="w_hypertension") %>%			
  ggplot(aes(age)) +		
  geom_bar(color = "orange", fill = "dodgerblue4") +		
  scale_x_continuous(breaks = seq(0, 100, by=5)) +		
  labs(x = "Age (in years)" , 		
       y = "Patients with Hypertension", 		
       title = "Hypertension Distribution (by age in years)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Bar graph of those with heart_disease distributed by their age.	
chart_heart_disease_by_age <- stroke_data %>% filter(heart_disease=="w_heart_disease") %>% 	
  ggplot(aes(age)) +		
  geom_bar(color = "orange", fill = "dodgerblue4") +		
  scale_x_continuous(breaks = seq(0, 100, by=5)) +		
  labs(x = "Age (in years)" , 		
       y = "Patients with Heart Disease", 		
       title = "Heart Disease Distribution (by age in years)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Graph of age by gender	
chart_age_by_gender <- stroke_data %>% ggplot(aes(age)) +	
  geom_histogram(binwidth=1, color = "orange", fill = "dodgerblue4") +	
  scale_x_continuous(breaks = seq(0, 100, by=10)) +	
  facet_wrap(~gender) +	
  labs(x = "Age (in years)",	
       y = " ",	
       title = "Age Distribution (in years, by gender)") + 	
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Bar graph of stroke by age.	
chart_stroke_by_age <- stroke_data %>% filter(stroke=="stroke") %>% 	
  ggplot(aes(age)) +		
  geom_bar(color = "orange", fill = "dodgerblue4") +		
  scale_x_continuous(breaks = seq(0, 100, by=5)) +		
  labs(x = "Age (in years)" , 		
       y = "Patients with Stroke", 		
       title = "Stroke Distribution (by age in years)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Density plot of smoking status by age	
chart_smoking_status_by_age <- stroke_data %>% 	
  ggplot(aes(age, fill = smoking_status, color = smoking_status)) +		
  geom_density(alpha = 0.3) +	
  scale_fill_manual(values = c("Unknown" = "gray",	
                             "never smoked" = "springgreen4", 	
                             "formerly smoked" = "dodgerblue4", 	
                             "smokes" = "firebrick4")) + 	
  scale_color_manual(values = c("Unknown" = "gray",	
                              "never smoked" = "springgreen4", 	
                              "formerly smoked" = "dodgerblue4", 	
                              "smokes" = "firebrick4")) + 	
  scale_x_continuous(breaks = seq(0, 100, by=5)) + 	
  scale_y_continuous(breaks = seq(0,0.025,by=0.01)) +	
  labs(x = "Age (in years)" , 		
       y = " ", 		
       title = "Smoking Status Distribution (by age in years)",	
       color = "Smoking Status",	
       fill = "Smoking Status") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Arrange the six charts into 2-column/3-row structure and add a caption.
grid.arrange(chart_age,	
             chart_age_by_gender,	
             chart_hypertension_by_age, 	
             chart_heart_disease_by_age,	
             chart_smoking_status_by_age,	
             chart_stroke_by_age,	
             ncol = 2,	
             nrow = 3,	
             bottom = textGrob(	
               "Source Data: stroke_data\nFigure 4.4.2",	
               gp = gpar(fontface = 3, fontsize = 10),	
               hjust = 1,	
               x = 1	
               )	
)	

# The average blood sugar (glucose) level and BMI distribution charts 
# (**Figure 4.4.3 below**) both show anticipated trends (though both are fairly
# flat) where the body's ability to manage blood sugar lowers over time (causing 
# the average levels to increase with age), and the BMI increases as you reach 
# middle-age, then lowers a little in the very late stages of life.	
# 	
# Plot of average glucose level by Age	
chart_avg_glucose_level_by_age <- stroke_data %>% group_by(avg_glucose_level) %>%		
  ggplot(aes(age, avg_glucose_level)) +		
  geom_point(alpha = 0.35, color = "dodgerblue4") + 		
  geom_smooth(method = loess, color = "orange") +		
  geom_hline(aes(yintercept = mean(avg_glucose_level)), color = "orange", linetype = 'dashed', size=1) +		
  labs(x = "Age  (in years)" , 		
       y = "Average Glucose Level", 		
       title = "Average Glucose Level Distribution",	
       subtitle = "(by age in years)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Plot of BMI by Age	
chart_bmi_by_age <- stroke_data %>% group_by(bmi) %>%		
  ggplot(aes(age, bmi)) +		
  geom_point(alpha = 0.35, color = "dodgerblue4") + 		
  geom_smooth(method = loess, color = "orange") +		
  geom_hline(aes(yintercept = mean(bmi)), color = "orange", linetype = 'dashed', size=1) +		
  labs(x = "Age (in years)" , 		
       y = "Body Mass Index (BMI)", 		
       title = "BMI Distribution",	
       subtitle = "(by age in years)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Scatter Plot of BMI by type of work	
chart_bmi_by_work_type <- stroke_data %>% ggplot(aes(id,bmi)) +	
  geom_point(aes(color = work_type), alpha = 0.3) + 	
  scale_color_manual(values = c("Underage_child" = "purple", 	
                                "Never_worked" = "red", 	
                                "Self-employed" = "green",	
                                "Private_company" = "dodgerblue4",	
                                "Govt_job" = "orange")) +	
  labs(x = "Patient (id)",	
       y = "Body Mass Index (bmi)",	
       title = "BMI Distribution",		
       subtitle = "(by type of work)",	
       color = "Type of Work") + 	
  theme(panel.border = element_rect(color = "black", fill = NA)) 		

# Arrange the three charts into 2-column/2-row structure, centering the 3rd 
# chart on the second row, and add a caption.
grid.arrange(chart_avg_glucose_level_by_age,	
             chart_bmi_by_age,	
             chart_bmi_by_work_type,	
             ncol = 2,	
             nrow = 2,	
             layout_matrix = rbind(c(1,2),	
                                   c(3,3)),	
             bottom = textGrob(	
               "Source Data: stroke_data\nFigure 4.4.3",	
               gp = gpar(fontface = 3, fontsize = 10),	
               hjust = 1,	
               x = 1	
               )	
)	

# In **Figure 4.4.4** below, a grid of eight charts shows a graphical comparison 
# of various predictors as they relate to stroke occurrence.  Since **Figure 4.4.1** 
# showed a stroke count of only `r NROW(stroke_data %>% filter(stroke=="stroke"))`, 
# out of a possible `r NROW(stroke_data)`, it is unsurprising that they appear highly 
# imbalanced.  All charts show an evenly-scaled distribution of stroke/no_stroke 
# occurrences among each predictor (not previously charted) or its categories.	
# 	
# Bar chart of those with a history of stroke by their gender.	
chart_stroke_by_gender <- stroke_data %>% group_by(gender) %>% 	
  ggplot(aes(gender, fill = stroke)) +		
  geom_bar(color = "orange", position = "dodge") +	
  scale_fill_manual(values = c("no_stroke" = "dodgerblue4", "stroke" = "dodgerblue")) + 	
  scale_y_continuous(breaks = seq(0, 4000, by=500), labels = comma) + 	
  labs(x = "Gender" , 		
       y = "Number of Occurances", 		
       title = "Stroke Distribution (by gender)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Bar chart of stroke distribution by hypertension history.	
chart_stroke_by_hypertension <- stroke_data %>% group_by(hypertension) %>% 	
  ggplot(aes(hypertension, fill = stroke)) +		
  geom_bar(color = "orange", position = "dodge") +	
  scale_fill_manual(values = c("no_stroke" = "dodgerblue4", "stroke" = "dodgerblue")) + 	
  scale_y_continuous(breaks = seq(0, 6000, by=500), labels = comma) + 	
  labs(x = "Hypertension History" , 		
       y = "Number of Occurances", 		
       title = "Stroke Distribution",	
       subtitle = "(by hypertension diagnosis)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Bar chart of stroke distribution by heart disease history.	
chart_stroke_by_heart_disease <- stroke_data %>% group_by(heart_disease) %>% 	
  ggplot(aes(heart_disease, fill = stroke)) +		
  geom_bar(color = "orange", position = "dodge") +	
  scale_fill_manual(values = c("no_stroke" = "dodgerblue4", "stroke" = "dodgerblue")) + 	
  scale_y_continuous(breaks = seq(0, 6000, by=500), labels = comma) + 	
  labs(x = "Hypertension History" , 		
       y = "Number of Occurances", 		
       title = "Stroke Distribution (by hypertension diagnosis)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
#Bar chart of stroke distribution by marriage history.	
chart_stroke_by_ever_married <- stroke_data %>% group_by(ever_married) %>% 	
  ggplot(aes(ever_married, fill = stroke)) +		
  geom_bar(color = "orange", position = "dodge") +	
  scale_fill_manual(values = c("no_stroke" = "dodgerblue4", "stroke" = "dodgerblue")) + 	
  scale_y_continuous(breaks = seq(0, 4000, by=500), labels = comma) + 	
  labs(x = "Marriage History" , 		
       y = "Number of Occurances", 		
       title = "Stroke Distribution (by marriage history)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Bar chart of stroke distribution by type of work.	
chart_stroke_by_work_type <- stroke_data %>% group_by(work_type) %>% 	
  ggplot(aes(work_type, fill = stroke)) +		
  geom_bar(color = "orange", position = "dodge") +	
  scale_fill_manual(values = c("no_stroke" = "dodgerblue4", "stroke" = "dodgerblue")) + 	
  scale_y_continuous(breaks = seq(0, 3000, by=500), labels = comma) + 	
  labs(x = "Type of Work" , 		
       y = "Number of Occurances", 		
       title = "Stroke Distribution (by type of work)") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 		
	
# Density chart of Stroke Distribution by BMI	
chart_stroke_by_bmi <- stroke_data %>% ggplot(aes(bmi, fill = stroke, color = stroke)) +		
  geom_density(alpha = 0.4) +	
  scale_fill_manual(values = c("stroke" = "dodgerblue", "no_stroke" = "dodgerblue4")) + 	
  scale_color_manual(values = c("stroke" = "dodgerblue", "no_stroke" = "dodgerblue4")) + 	
  scale_x_continuous(breaks = seq(0, 100, by=5)) + 	
  scale_y_continuous(breaks = seq(0,0.2,by=0.05)) +	
  labs(x = "Body Mass Index (BMI)" , 		
       y = " ", 		
       title = "Stroke Distribution (by bmi)",	
       color = "Stroke",	
       fill = "Stroke") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 	
	
# Density chart of Stroke Distribution by average glucose level	
chart_stroke_by_avg_glucose_level <- stroke_data %>% 	
  ggplot(aes(avg_glucose_level, 	
             fill = stroke, color = stroke)) +		
  geom_density(alpha = 0.5) +	
  scale_fill_manual(values = c("stroke" = "dodgerblue", "no_stroke" = "dodgerblue4")) + 	
  scale_color_manual(values = c("stroke" = "dodgerblue", "no_stroke" = "dodgerblue4")) + 	
  scale_x_continuous(breaks = seq(0, 300, by=20)) + 	
  scale_y_continuous(breaks = seq(0,0.025,by=0.01)) +	
  labs(x = "Average Glucose Level" , 		
       y = " ", 		
       title = "Stroke Distribution (by average glucose level)",	
       color = "Stroke",	
       fill = "Stroke") +		
  theme(panel.border = element_rect(color = "black", fill = NA)) 	
	
#Bar chart of stroke history by smoking status	
chart_stroke_by_smoking_status <- stroke_data %>% group_by(smoking_status) %>% 	
  ggplot(aes(smoking_status, fill = stroke)) +		
  geom_bar(color = "orange", position = "dodge") +	
  scale_fill_manual(values = c("no_stroke" = "dodgerblue4", "stroke" = "dodgerblue")) + 	
  scale_y_continuous(breaks = seq(0, 4000, by=500), labels = comma) + 	
  labs(x = "Smoking Status" , 		
       y = "Number of Occurances", 		
       title = "Stroke Distribution (by smoking status)") +		
  theme(panel.border = element_rect(color = "black", fill = NA))	

# Arrange the eight charts into 2-column/4-row structure and add a caption.
grid.arrange(chart_stroke_by_gender,	
             chart_stroke_by_hypertension,	
             chart_stroke_by_heart_disease,	
             chart_stroke_by_ever_married,	
             chart_stroke_by_work_type,	
             chart_stroke_by_bmi,	
             chart_stroke_by_avg_glucose_level,	
             chart_stroke_by_smoking_status,	
             ncol = 2,	
             nrow = 4,	
             bottom = textGrob(	
               "Source Data: stroke_data\nFigure 4.4.4",	
               gp = gpar(fontface = 3, fontsize = 10),	
               hjust = 1,	
               x = 1	
               )	
)	

# Create the correlation matrix using the Pearson method.	
correlation <- cor(stroke_data_num, method = c("pearson"))	
# Create a correlation coefficient visualization to determine relevant factors.	
corrplot(correlation, 	
         type = "upper", 	
         order = "AOE", 	
         tl.col = "black", 	
         tl.srt = 45)	

# Create correlation variables for each correlation coefficient's output
cor_gender <- cor(stroke_data_num$gender, stroke_data_num$stroke)	
cor_age <- cor(stroke_data_num$age, stroke_data_num$stroke)	
cor_hypertension <- cor(stroke_data_num$hypertension, stroke_data_num$stroke)	
cor_heart_disease <- cor(stroke_data_num$heart_disease, stroke_data_num$stroke)	
cor_ever_married <- cor(stroke_data_num$ever_married, stroke_data_num$stroke)	
cor_work_type <- cor(stroke_data_num$work_type, stroke_data_num$stroke)	
cor_Residence_type <- cor(stroke_data_num$Residence_type, stroke_data_num$stroke)	
cor_bmi <- cor(stroke_data_num$bmi, stroke_data_num$stroke)	
cor_avg_glucose_level <- cor(stroke_data_num$avg_glucose_level, stroke_data_num$stroke)	
cor_smoking_status <- cor(stroke_data_num$smoking_status, stroke_data_num$stroke)	
	
# Create a table title variable with the list of dataset variable names
cor_table_titles <- c("gender","age","hypertension","heart_disease","ever_married",	
                       "work_type","Residence_type","bmi","avg_glucose_level",	
                      "smoking_status")	

# Create a variable with a list of dataset correlation coefficient results.
cor_table_coeffs <- c(cor_gender,cor_age,cor_hypertension,cor_heart_disease,	
                      cor_ever_married,cor_work_type,cor_Residence_type,cor_bmi,	
                      cor_avg_glucose_level,cor_smoking_status)	

# Create a table with the "cor_table_titles" contents in the left column, and 
# the "cor_table_coeffs" contents in the right column.
kable(tibble(cor_table_titles, cor_table_coeffs),	
      col.names = c("Variable Name", "Correlation Coefficient to Stroke")) %>%	
  row_spec(0,background="#104E8B", color="white") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

# The table above shows the correlation coefficient amounts for each 
# applicable variable.  The `id` column was removed during dataset creation 
# since there's no direct correlation with that field, and running the `cor()`
# command against the `stroke` variable would always be 1, so it was not 
# added to the table.	
#
# The data shows that the `age`, `heart_disease`, `avg_glucose_level`, and
# `hypertension` are the most relevant predictors to `stroke`.	


#######################################################################################
# 3.) MODELING ########################################################################
#######################################################################################
#
# Throughout the data analysis performed above, some data point comparisons show the 
# possibility of higher correlations while others appear they would have little 
# impact to the overall predictability, and thus the model.  To determine the impact 
# of each predictor, various models will be tested, starting with logistic regression
# and then including Naive Bayes, linear discriminant analysis, classification and 
# regression trees, random forest, and k-Nearest Neighbor.  Each resultant accuracy 
# calculation will be summarized in a list with previous results as to make it easy 
# to compare all results and determine which model performs the best (having the 
# highest accuracy score).	

# Model cross-validation training options variable creation.	
fitControl <- trainControl(method = "cv",  #cross-validation	
                           number = 10)    #10-fold cross-validation 	

#######################################################################################
# 3a.) MODEL 1: Logistic Regression (GLM)
#######################################################################################
#
# The logistic regression is a very basic and frequently used machine learning model
# where independent variables determine a binary outcome, such as the stroke prediction
# in this dataset.  The conditional probability can be modeled as:	

set.seed(1, sample.kind = "Rounding")	
# Train the logistic regression model with the generalized linear model (GLM) 	
# method, using the fitControl tuning parameters.	
model1_fit <- train(stroke ~ ., data = stroke_train,	
                   method = "glm",	
                   preProcess=c("center", "scale"),	
                   trControl = fitControl)	
model1_fit	
	
# Predict the outcome from the stroke_test dataset	
model1_preds <- predict(model1_fit, stroke_test)	
	
# Calculate the accuracy of the model against the stroke_test dataset.	
model1_accuracy <- confusionMatrix(model1_preds, stroke_test$stroke)$overall["Accuracy"]	
model1_accuracy	

# Create a table containing the Model 1 data results.	
model_table_titles <- "Model 1: Logistic Regression (GLM)"	
model_table_accuracy <- model1_accuracy	
kable(tibble(model_table_titles, model_table_accuracy),	
      col.names = c("Model", "Accuracy")) %>%	
  row_spec(0,background="#104E8B", color="white") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

#######################################################################################
# 3b.) MODEL 2: Naive Bayes
#######################################################################################
#
# The Naive Bayes model, based on Bayes Theorem[^Bayes], estimates the outcome by 
# estimating the conditional distribution of the predictors, which also assumes 
# independence among the predictors.  

set.seed(1, sample.kind = "Rounding")	
# Train the Naive Bayes model using the fitControl tuning parameters.	
model2_fit <- train(stroke ~ ., data = stroke_train,	
                   method = "naive_bayes",	
                   preProcess=c("center", "scale"),	
                   trControl = fitControl)	
model2_fit	
	
# Predict the outcome from the stroke_test dataset	
model2_preds <- predict(model2_fit, stroke_test)	
	
# Calculate the accuracy of the model against the stroke_test dataset.	
model2_accuracy <- confusionMatrix(model2_preds, stroke_test$stroke)$overall["Accuracy"]	
model2_accuracy	

# Create a table containing the Model 2 data results.	
model_table_titles <- c(model_table_titles, "Model 2: Naive Bayes")	
model_table_accuracy <- c(model_table_accuracy, model2_accuracy)	
kable(tibble(model_table_titles, model_table_accuracy),	
      col.names = c("Model", "Accuracy")) %>%	
  row_spec(0,background="#104E8B", color="white") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

#######################################################################################
# 3c.) MODEL 3: Linear Discriminant Analysis (LDA)	
#######################################################################################
#
# The linear discriminant analysis model improved upon Naive Bayes by using a 
# dimensionality reduction technique that estimates the probability that a new set of
# inputs belongs to every class.  The resultant output class is the one that has the 
# highest probability (the prediction).	

set.seed(1, sample.kind = "Rounding")	
# Train the LDA model using the fitControl tuning parameters.	
model3_fit <- train(stroke ~ ., data = stroke_train,	
                   method = "lda",	
                   preProcess=c("center", "scale"),	
                   trControl = fitControl)	
model3_fit	
	
# Predict the outcome from the stroke_test dataset	
model3_preds <- predict(model3_fit, stroke_test)	
	
# Calculate the accuracy of the model against the stroke_test dataset.	
model3_accuracy <- confusionMatrix(model3_preds, stroke_test$stroke)$overall["Accuracy"]	
model3_accuracy	

# Create a table containing the Model 3 data results.	
model_table_titles <- c(model_table_titles, "Model 3: Linear Discriminant Analysis (LDA)")	
model_table_accuracy <- c(model_table_accuracy, model3_accuracy)	
kable(tibble(model_table_titles, model_table_accuracy),	
      col.names = c("Model", "Accuracy")) %>%	
  row_spec(0,background="#104E8B", color="white") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

#######################################################################################
# 3d.) MODEL 4: Classification and Regression Trees Model (CART)	
#######################################################################################
#
# Classification (decision) trees build up a set of decision rules that "branch out" 
# to form a tree structure, similar to a flow chart.  This branching helps predict an 
# outcome based on the input data.	

set.seed(1, sample.kind = "Rounding")	
# Train the Decision Tree model, setting the tuning parameters.	
model4_fit <- train(stroke ~ ., data = stroke_train,	
                   method = "rpart",	
                   preProcess=c("center", "scale"),	
                   tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)))	
model4_fit	
	
# Predict the outcome from the stroke_test dataset	
model4_preds <- predict(model4_fit, stroke_test)	
	
# Calculate the accuracy of the model against the stroke_test dataset.	
model4_accuracy <- confusionMatrix(model4_preds, stroke_test$stroke)$overall["Accuracy"]	
model4_accuracy	

# Create a table containing the Model 4 data results.	
model_table_titles <- c(model_table_titles, "Model 4: Classification and Regression Trees Model (CART)")	
model_table_accuracy <- c(model_table_accuracy, model4_accuracy)	
kable(tibble(model_table_titles, model_table_accuracy),	
      col.names = c("Model", "Accuracy")) %>%	
  row_spec(0,background="#104E8B", color="white") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

#######################################################################################
# 3e.) MODEL 5: Random Forest
#######################################################################################
#
# With the CART model being prone to over-fitting, the random forest model compensates
# for this by using decision trees to generate many predictors, and then averages them 
# until a final prediction is made based on the averages.  The trade-off for its 
# performance gain is the loss of interpret ability of the data.  	

set.seed(1, sample.kind = "Rounding")	
# Train the Random Forest model using the "Rborist" method.  Set basic decision tree	
# tuning parameters.	
model5_fit <- train(stroke ~ ., data = stroke_train,	
                   method = "Rborist",	
                   preProcess=c("center", "scale"),	
                   tuneGrid = expand.grid(predFixed = seq(1,4), minNode = 2))	
model5_fit	
	
# Predict the outcome from the stroke_test dataset	
model5_preds <- predict(model5_fit, stroke_test)	
	
# Calculate the accuracy of the model against the stroke_test dataset.	
model5_accuracy <- confusionMatrix(model5_preds, stroke_test$stroke)$overall["Accuracy"]	
model5_accuracy	

# Create a table containing the Model 5 data results.	
model_table_titles <- c(model_table_titles, "Model 5: Random Forest")	
model_table_accuracy <- c(model_table_accuracy, model5_accuracy)	
kable(tibble(model_table_titles, model_table_accuracy),	
      col.names = c("Model", "Accuracy")) %>%	
  row_spec(0,background="#104E8B", color="white") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

#######################################################################################
# 3f.) MODEL 6: k-Nearest Neighbor (kNN)
#######################################################################################
#
# The k-nearest neighbor model searches its neighboring data, assuming that similar 
# data points exist close by.  The most similar data points to the ones you have to 
# predict are found by averaging the neighboring values, or by most frequent class.  
# Decreasing the value of K yields less accurate predictions, and increasing it too 
# high produces errors which also yields less accurate predictions.	

set.seed(1, sample.kind = "Rounding")	
# Train the k-Nearest Neighbors model, setting the k-value tuning parameters.	
model6_fit <- train(stroke ~ ., data = stroke_train,	
                   method = "knn",	
                   preProcess=c("center", "scale"),	
                   tuneGrid = data.frame(k = seq(1,100,5)))	
model6_fit	
	
# Predict the outcome from the stroke_test dataset	
model6_preds <- predict(model6_fit, stroke_test)	
	
# Calculate the accuracy of the model against the stroke_test dataset.	
model6_accuracy <- confusionMatrix(model6_preds, stroke_test$stroke)$overall["Accuracy"]	
model6_accuracy	

# Create a table containing the Model 6 data results.	
model_table_titles <- c(model_table_titles, "Model 6: k-Nearest Neighbor (kNN)")	
model_table_accuracy <- c(model_table_accuracy, model6_accuracy)	
kable(tibble(model_table_titles, model_table_accuracy),	
      col.names = c("Model", "Accuracy")) %>%	
  row_spec(0,background="#104E8B", color="white") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

#######################################################################################
# 4.) RESULTS #########################################################################
#######################################################################################
#
# Having done all of the initial data analysis in section 4 and pinpointing which 
# factors may contribute to (or at least warrant a model to test) an accurate stroke 
# prediction system, stepping through six (6) different models yielded the most 
# accurate results with the use of the Random Forest model. While testing the various 
# performance tuning options of each model a few models ended up having extended 
# compute-intensive time frames (up to 2 hours on an 8-core hyperthreaded processor 
# with 64GB RAM).  The final tests with the parameters used in this report only took
# around 30 minutes to complete from start to finish.	

# The final accuracy results table below shows the summary list of all prediction 
# models tested for a stroke prediction system using the stroke_train and stroke_test 
# datasets.  	
 	
# Create Final RMSE Results Table for all models	
kable(tibble(model_table_titles, model_table_accuracy),	
      col.names = c("Model", "Accuracy")) %>%	
  row_spec(0,background="#104E8B", color="white") %>%	
  row_spec(5, bold=TRUE, color = "red") %>% 	
  column_spec(2, bold=TRUE) %>% 	
  kable_styling(bootstrap_options="bordered", 	
                full_width=FALSE, 	
                position="center",	
                latex_options="HOLD_position")	

#######################################################################################
# 5.) CONCLUSION ######################################################################
#######################################################################################
#
# The purpose of this project was to create a stroke prediction system, using a public
# dataset from Kaggle, and building multiple machine learning models, selecting the one 
# with the highest accuracy as the final model.  From initial GitHub repository 
# creation, to data wrangling, dataset analysis, data visualizations, and model 
# development, the knowledge and techniques learned throughout the entire course series
# has been used.	
# 	
# Both the logistic regression, Naive Bayes, and linear discriminant models all 
# performed their calculations very quickly, and all used the same tuning parameters
# with results that were all still very high.  The Random Forest and k-Nearest Neighbors 
# tuning parameters were more difficult to test as the calculation times would get 
# extended, and the CPU resources would be locked to the process until completed each 
# time. 	
#
# After testing 6 different predictive models, using their respective calculated 
# accuracy scores as the success indicator for each, the Random Forest model yielded
# the highest score.  Comparing this accuracy score to the 
# initial Logistic Regression model, there was an improvement.	
# 	
# While the dataset contained many of the health factors that were considered risks 
# by the Centers for Disease Control (CDC) and the National Institute of Neurological
# Disorders and Stroke (NINDS), as indicated in the Introduction, limitations to the
# model included several other missing predictors that could be added to future 
# tests, such as Race, geographic location, alcohol drinking habits, drug use, 
# cholesterol level, etc.  Being able to inform both the patient and their doctor 
# of their risk of stroke could lead to proactive treatment, better life choices, 
# and a prolonged life span.  The data in this particular dataset showed `age`,
# `heart_disease`, `avg_glucose_level`, and `hypertension` are the most relevant
# predictors to `stroke`.	
# 	
# Further improvements and future work include an expanded dataset with more samples, 
# additional performance tuning on all of the models in the future (especially in the
# event that additional factors get added).  Additional models, such as the ensemble 
# method, could be developed and tuned to test whether further accuracy can be 
# achieved.	

# Clean up Global Environment
rm(list=ls())