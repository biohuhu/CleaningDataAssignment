# run_analysis.R for Assignment

# read in data
x_test_df = read.table("./test/X_test.txt", stringsAsFactors= F)
y_test_vec = read.table("./test/y_test.txt", stringsAsFactors = F)  #1-6
subject_test_vec = read.table("./test/subject_test.txt", stringsAsFactors = F) 
# unique:  2  4  9 10 12 13 18 20 24

x_train_df = read.table("./train/X_train.txt", stringsAsFactors= F)
y_train_vec = read.table("./train/y_train.txt", stringsAsFactors = F)  #1-6
subject_train_vec = read.table("./train/subject_train.txt", stringsAsFactors = F) 
# unique: the rest of 21 subjects

activity_labels_df = read.table("activity_labels.txt", stringsAsFactors = F) #1-6 walking etc.
features_df = read.table("features.txt", stringsAsFactors = F) # feature names
#features_info_df = read.table("features_info.txt", stringsAsFactors = F) # feature names

# 1) Merges the training and the test sets to create one data set.
merged_df = rbind(x_train_df, x_test_df)
y_merged = c(y_train_vec$V1, y_test_vec$V1)
s_merged = c(subject_train_vec$V1, subject_test_vec$V1)
rm(list = c("x_train_df", "x_test_df", "y_train_vec", "y_test_vec"))

# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
features_vec = features_df$V2
ids_mean = grep("mean()", features_vec, fixed = T)
ids_std = grep("std()", features_vec, fixed = T)
features_ids = sort(c(ids_mean, ids_std))
features_keep = features_vec[features_ids]
x_keep = merged_df[,features_ids]

# 3) Uses descriptive activity names to name the activities in the data set
activity = activity_labels_df[y_merged, 2]

# 4) Appropriately labels the data set with descriptive variable names. 
features_keep = sub("()", "", features_keep, fixed = T)
colnames(x_keep) = features_keep
x_keep$activity = activity
x_keep$subject = s_merged


# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
x_split = split(x_keep[,1:(ncol(x_keep)-2)], list(x_keep$subject, x_keep$activity))
x_meanbySA = data.frame(t(sapply(x_split, colMeans)))
x_meanbySA = cbind(rownames(x_meanbySA), x_meanbySA)
colnames(x_meanbySA)[1] = "Subject.Activity"
write.table(x_meanbySA, "x_meanbySubjectActivity.txt", sep = "\t", row.name=F)


