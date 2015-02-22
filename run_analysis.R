# read files in the UCI HAR Dataset directory
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("order", "measurement"), colClasses = c("numeric", "character"))
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("numbers", "activity"), colClasses = c("numeric", "character"))
feature <- data.frame(grep("mean\\(\\)|std\\(\\)",features[,2]))
names(feature) <- paste("numbers")
feature_need <- features[features$order %in% feature$numbers,]

# read files in the train directory
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "activity", colClasses = "numeric")
x_train <- read.table("./UCI HAR Dataset/train/x_train.txt")[,feature[,1]]
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

# read files in the text directory
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "activity", colClasses = "numeric")
x_test <- read.table("./UCI HAR Dataset/test/x_test.txt")[,feature[,1]]
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# tidy the train data
train_activity <- y_train
for (i in 1:6){
  train_activity$activity[train_activity$activity == i] <- activity_labels[activity_labels$numbers[activity_labels$numbers == i],][,2] 
}
train_activity$method <- "train"
train_data <- cbind(subject_train, train_activity, x_train)

# tidy the test data
test_activity <- y_test
for (i in 1:6){
  test_activity$activity[test_activity$activity == i] <- activity_labels[activity_labels$numbers[activity_labels$numbers == i],][,2] 
}
test_activity$method <- "test"
test_data <- cbind(subject_test, test_activity, x_test)

# merge the train_data and test_data
data <- rbind(train_data, test_data)

#  tidy data
library(dplyr)
library(tidyr)
gather_data <- data %>%
  gather(features, values, V1:V543) %>%
  group_by(subject, activity, features, method) %>%
  mutate(mean = mean(values)) %>%
  select(subject, activity, features, method, mean) %>%
  unique()

feature_need <- mutate(feature_need, features = paste("V",order, sep = ""))
traixial_data <- merge(gather_data, feature_need, by = "features")
traixial_data <- select(traixial_data, subject, activity,	method,	mean, measurement)

#tidy the features value
traixial <- separate(traixial_data, col = measurement, into = c("features","measurement","triaxial"))

#create the csv file
write.table(traixial, file = "triaxial_data.txt", row.name=FALSE)
