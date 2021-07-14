
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")	
#if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")	
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")	
#if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")	
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")	
#if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")	
#if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")	
#if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")	
#if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")	

# Load the necessary libraries for this project.	
library(tidyverse)	
#library(caret)	
#library(data.table)	
#library(scales)	
#library(lubridate)	
#library(dplyr)	
#library(gridExtra)	
#library(kableExtra)	
#library(recosystem)	

# Download the MovieLens 10M dataset to the computer's temporary directory.	
dl <- tempfile()	
download.file("https://github.com/rwalscheid/HarvardX-Capstone-CYO/blob/main/dataset.csv", dl)	


productdata <- read.csv(dl, header = TRUE)

# Summary information of the "ratings" dataset	
summary(ratings)	

# Structure of the "ratings" dataset	
str(ratings)	

# Create the 'movies' data frame and fill it with the 3 columns of data	
# that were delimited by '::' in the movies.dat file, labeling them moveiId, 	
# title, and genres, respectively:	
#	
# Note: this process could take a couple of minutes.	
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)	
colnames(movies) <- c("movieId", "title", "genres")	

# The movies matrix array needs to be converted to a usable dataframe, 	
# assigning the proper class types for this project to each column.	
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),	
                                           title = as.character(title),	
                                           genres = as.character(genres))	

# Summary information of the "movies" dataset	
summary(movies)	

# Structure of the "movies" dataset	
str(movies)	

# Create the 'movielens' data frame by joining the 'ratings' and 'movies' 	
# datasets by 'movieId'.	
movielens <- left_join(ratings, movies, by = "movieId")	

# Summary information of the "movielens" dataset	
summary(movielens)	

# Structure of the "movielens" dataset	
str(movielens)	

# The training dataset, "edx", and validation test dataset, "temp", will be created 	
# from a 90%/10% split of the MovieLens data, respectively.	
set.seed(1, sample.kind="Rounding")	
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)	
edx <- movielens[-test_index,]	
temp <- movielens[test_index,]	
