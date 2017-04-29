rm (list = ls()) # make a clean table
# setwd("D:/home/02-projekte-MST/Data-Scientist/datacleaning/project")
# getwd()



# get the data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./galaxy-dataset.zip", method = "curl" , mode = "wb")

# unpack zipped data file
unzip(zipfile = "./galaxy-dataset.zip")

# read the data files into data frames
X_train <- read.table(file="./UCI HAR Dataset/train/X_train.txt" , sep = "" , header = FALSE )
y_train <- read.table(file="./UCI HAR Dataset/train/y_train.txt" , sep = "" , header = FALSE )

X_test <- read.table(file="./UCI HAR Dataset/test/X_test.txt" , sep = "" , header = FALSE )
y_test <- read.table(file="./UCI HAR Dataset/test/y_test.txt" , sep = "" , header = FALSE )

# read features
feature_names <-  read.csv("./UCI HAR Dataset/features.txt", sep = "", header = FALSE )
feature_names <- feature_names[,2] # keep only the column with the feature names
names(X_train) <- feature_names
names(X_test) <- feature_names

# read the subject files
subject_train <- read.table(file="./UCI HAR Dataset/train/subject_train.txt")
table(subject_train) # check for plausibility
names(subject_train) <- "subject_ID" # add a meaningful column name

subject_test <- read.table(file="./UCI HAR Dataset/test/subject_test.txt")
table(subject_test) # check for plausibility
names(subject_test) <- "subject_ID"  # add a meaningful column name


# add column name for label files
names(y_train) <- "activity_ID" # add a meaningful column name
names(y_test) <- "activity_ID"  # add a meaningful column name

# merge dataframes
train_data <- cbind(subject_train, y_train, X_train) # at first put the columns together: subject/person IDs, x and y values
test_data <- cbind(subject_test, y_test, X_test) # do the same with the test data
all_data <- rbind(train_data, test_data) # final merge


# which colum names contain ontain "mean()" or "std()" ?
wanted_columns <- grepl ( pattern = "*[Mm]ean*" , names(all_data) )  | grepl( "*[Ss]td*",   names(all_data) ) 
table(wanted_columns) # just a simple plausibility check
wanted_columns [1:2] <- TRUE # also keep columns activity_ID and subject_ID

# we only keep the wanted columns in th data frame
all_data <- all_data [, wanted_columns]
colum_names <- names(all_data)
# braces in names causing trouble with R, -> I remove them
colum_names <- gsub( pattern = "\\()", replacement = "" , x = colum_names )
names (all_data) <- colum_names # write back the tidy names

# read the activities into a data frame
activities <-  read.table(file="./UCI HAR Dataset/activity_labels.txt")
names (activities ) <- c ("activity_ID" , "activity_name")   # add a meaningful column names

# create factor variables for grouping
all_data$activity_name  <- factor(all_data$activity_ID , levels = activities$activity_ID , labels = activities$activity_name )
table(all_data$activity_name) # just a simple plausibility check
all_data$subject_ID <- as.factor(all_data$subject_ID)
all_data$activity_ID <- as.factor(all_data$activity_ID)

library(dplyr) # transformations are easier done with dplyr
# see cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

All_Data <- tbl_df(all_data) # create a dplyr formatted object

tidy_data <- 
All_Data %>%
  # remove unneccesary column
  select ( -(activity_ID) ) %>% 
  # group data set
  group_by (activity_name ,subject_ID ) %>%
  # calulate mean for every group
  summarise_each( funs(mean )) %>%
  print

# write tidy data into a text file
write.table(tidy_data , file = "./tidy-data.txt", row.names=FALSE ) 
  

 