#' A script to transform Human Activity Recognition Using Smartphones Dataset to combine the training and test sets, extract mean and
#' standard deviation of data measured, aggregate by the subject and activities to calculate the mean.
#'
#' Copyright 2020 Hide Inada. All rights reserved.
#'
#' License for the dataset used
#'
#' License:
#' ========
#' Use of this dataset in publications must be acknowledged by referencing the following publication [1] 
#' 
#' [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
#' 
#' This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
#' 
#' Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
#' 

library(tidyverse)
print("Starting ...")

# Subject
print("Reading subject data for training set ...")
s_train <- read_table2("train/subject_train.txt", col_names=FALSE)
row_read <- nrow(s_train)
print(sprintf("Read %d rows", row_read))

s_test <- read_table2("test/subject_test.txt", col_names=FALSE)
row_read <- nrow(s_test)
print(sprintf("Read %d rows", row_read))

print("Combining subject data for training set and test set ...")
s_total <- rbind(s_train, s_test)
row_total <- nrow(s_total)
print(sprintf("Subject data for combined dataset has now %d rows", row_total))
names(s_total)[1] <- "Subject"

# Activity
print("Reading activity data for training set ...")
y_train <- read_table2("train/y_train.txt", col_names=FALSE)
row_read <- nrow(y_train)
print(sprintf("Read %d rows", row_read))

y_test <- read_table2("test/y_test.txt", col_names=FALSE)
row_read <- nrow(y_test)
print(sprintf("Read %d rows", row_read))

print("Combining activity data for training set and test set ...")
y <- rbind(y_train, y_test)
row_total <- nrow(y)
print(sprintf("Activity data for combined dataset has now %d rows", row_total))

y <- y %>% mutate(
        Activity = 
            case_when(
                X1 == 1 ~ "WALKING", 
                X1 == 2 ~ "WALKING_UPSTAIRS",
                X1 == 3 ~ "WALKING_DOWNSTAIRS",
                X1 == 4 ~ "SITTING",
                X1 == 5 ~ "STANDING",
                X1 == 6 ~ "LAYING"
            )
        )

y <- subset(y, select=Activity)

# Measured data
print("Reading training set ...")
d_train <- read_table2("train/X_train.txt", col_names=FALSE)
row_read <- nrow(d_train)
print(sprintf("Read %d rows", row_read))
print(sprintf("Number of columns %d", length(names(d_train))))

print("Reading test set ...")
d_test <- read_table2("test/X_test.txt", col_names=FALSE)
row_read <- nrow(d_test)
print(sprintf("Read %d rows", row_read))
print(sprintf("Number of columns %d", length(names(d_test))))

print("Reading feature names")
d_fn <- read_table2("features.txt", col_names=FALSE)
row_read <- nrow(d_fn)
print(sprintf("Read %d rows", row_read))

print("Combining training set and test set ...")
data_set <- rbind(d_train, d_test)
row_total <- nrow(data_set)
print(sprintf("Combined dataset has now %d rows", row_total))

# Rename feature columns
new_names <- d_fn$X2
l <- length(new_names)
names(data_set) <- new_names

# Combine all data
d <- cbind(s_total, y, data_set)

# Extract mean and sd
d <- cbind(d[,c("Subject", "Activity")], select(d, matches(".*(mean\\(\\)|std\\(\\)).*", ignore.case = TRUE)))

# Group by
d <- as_tibble(d)
d2 <- group_by(d, Subject, Activity) 
d2 <- summarise_all(d2, mean)

# Write to a file
write.table(d2, "summary.tsv", row.name=FALSE)