# read files in the UCI HAR Dataset directory
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("order", "measurement"), colClasses = c("numeric", "character"))
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("numbers", "activity"), colClasses = c("numeric", "character"))

# read files in the train directory
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "numbers", colClasses = "numeric")
x_train <- read.table("./UCI HAR Dataset/train/x_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

# read files in the text directory
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "numbers", colClasses = "numeric")
x_test <- read.table("./UCI HAR Dataset/test/x_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# tidy the train data
train_activity <- y_train
for (i in 1:6){
  train_activity$numbers[train_activity$numbers == i] <- activity_labels[activity_labels$numbers[activity_labels$numbers == i],][,2] 
}
train_activity$method <- "train"
train_data <- cbind(subject_train, train_activity, x_train)

# tidy the test data
test_activity <- y_test
for (i in 1:6){
  test_activity$numbers[test_activity$numbers == i] <- activity_labels[activity_labels$numbers[activity_labels$numbers == i],][,2] 
}
test_activity$method <- "test"
test_data <- cbind(subject_test, test_activity, x_test)

# merge the train_data and test_data
data <- rbind(train_data, test_data)
names(data)[4:564] <- features[,"measurement"]
names(data)[2] <- paste("activity")

# select mean and std of the data
mean_data <- data[,grep("mean\\()", names(data), value=T)]
std_data <- data[,grep("std()", names(data), value=T)]
mean_and_std_data <- cbind(data[1:3], mean_data, std_data)

# renames the data variable names for tidy data
library(dplyr)
library(tidyr)
mas_features <- data.frame(names(mean_and_std_data)[4:69])
names(mean_and_std_data)[4:69] <- paste("l",4:69,sep="")
gather_data <- gather(mean_and_std_data, features, points, l4:l69)

#mutate the train mean
gather_train_data <- filter(gather_data, method == "train")
group_data <- group_by(gather_train_data, subject, activity, features)
mutate_data <- mutate(group_data, mean = mean(points))
mutate_data_sub <- select(mutate_data,-(points))
unique_train_data <- unique(mutate_data_sub)

#mutate the test mean
gather_test_data <- filter(gather_data, method == "test")
test_group <- group_by(gather_test_data, subject, activity, features)
test_mutate <- mutate(test_group, mean = mean(points))
test_mutate_sub <- select(test_mutate,-(points))
unique_test_data <- unique(test_mutate_sub)

#merge data
mean_train_test <- rbind(unique_train_data, unique_test_data)

#names the features
mas_features$features <- paste("l", 4:69,sep="")
mean_train_test <- merge(mean_train_test, mas_features, by = "features")
mtt <- select(mean_train_test, -(features))
names(mtt)[5] <- paste("features")


#tidy the features value
traixial_data <- separate(data= mtt, col = features, into = c("features","measurement","triaxial"))

#create the csv file
write.table(traixial_data, file = "triaxial_data.txt", row.name=FALSE)
