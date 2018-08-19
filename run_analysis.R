########################## import data from UCI ############################
train <- read.table("X_train.txt", header = FALSE, sep = "")
test <- read.table("X_test.txt", header = FALSE, sep = "")
# the "lable" files identify which activity was performed in each record
train_lb <- read.table("y_train.txt", header = FALSE, sep = "")
test_lb <- read.table("y_test.txt", header = FALSE, sep = "")
# the "subject" files identify which volunteer performed the activity for each window sample
subject_train <- read.table("subject_train.txt", header = FALSE, sep = "")
subject_test <- read.table("subject_test.txt", header = FALSE, sep = "")
# the "activity_lables" file contains the six types of activities in the experiment
activity_lb <-  read.table("activity_labels.txt", header = FALSE, sep = "", colClasses = "character")
# the "features" file contains the attribute vectors 
feature <- read.table("features.txt", header = FALSE, sep = "")
##################### merge traing and test datasets ########################
train_test <- rbind(train, test)
lb <- rbind(train_lb, test_lb)
subject <- rbind(subject_train, subject_test)
################## extract mean and sd for each measurement #################
extract_index <- sort(c(grep("mean[^F]", feature[, 2]), grep("std", feature[, 2])))
train_test <- train_test[, extract_index]
################## Use descriptive activity names ###########################
activity <- rep("to be assigned", nrow(lb))
for (i in 1:nrow(lb)){activity[i] <- activity_lb[lb[i,1], 2]}
train_test <- cbind(activity, subject, train_test)
################# label the data set with descriptive variable names ########
colnames(train_test) <- c("activity", "volunteer", as.character(feature[extract_index, 2]))
############# create a data set with the average of each variable ###########
avg_train_test <- tbl_df(train_test)
avg_train_test <- avg_train_test %>%
                  group_by(activity, volunteer) %>% 
                  summarize_all(funs(mean))
############# produce the output in a text file #############################
write.table(avg_train_test, file = "avg_upload.txt", row.name = FALSE)
