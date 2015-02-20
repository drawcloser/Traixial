# import mean and std of data scripts
source(mean_and_std_data.R)
library(dplyr)
library(tidyr)
mas_features <- data.frame(names(mean_and_std_data)[4:82])
names(mean_and_std_data)[4:82] <- paste("l",4:82,sep="")
gather_data <- gather(mean_and_std_data, features, points, l4:l82)

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
mas_features$features <- paste("l", 4:82,sep="")
mean_train_test <- merge(mean_train_test, mas_features, by = "features")
mtt <- select(mean_train_test, -(features))
names(mtt)[5] <- paste("features")

#tidy the features value
traixial_data <- separate(data= mtt, col = features, into = c("features","measurement","triaxial"))

#create the csv file
write.table(traixial_data, file = "triaxial_data.txt", row.name=FALSE)
