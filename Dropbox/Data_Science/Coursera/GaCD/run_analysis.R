run_analysis <- function(){
        
        # Step 1 
        
        ## Reading in Training Data
        setwd("~/Dropbox/Data_Science/Coursera/GaCD/UCI_HAR_Dataset/train")
        
        X_train <- read.table("X_train.txt")
        
        y_train <- read.table("y_train.txt")

        subject_train <- read.table("subject_train.txt")
        
        
        ## Reading in Activity Labels and Features
        setwd("~/Dropbox/Data_Science/Coursera/GaCD/UCI_HAR_Dataset")
        
        activity_labels <- read.table("activity_labels.txt")
        
        features <- read.table("features.txt")
        
        
        ## Reading in Test data
        setwd("~/Dropbox/Data_Science/Coursera/GaCD/UCI_HAR_Dataset/test")
        
        X_test <- read.table("X_test.txt")
        
        y_test <- read.table("y_test.txt")
        
        subject_test <- read.table("subject_test.txt")
        
        
        ## Merging X_train and X_test
        data_set_X <- rbind(X_train, X_test)
        
        
        ## Merging y_train and y_test
        data_set_y <- rbind(y_train, y_test)
        
        
        ## Merging subject_train and subject_test
        data_set_subject <- rbind(subject_train, subject_test)
        
        
        ## rename column names in data_set_subject and data_set_y 
        colnames(data_set_subject)[1] <- "subject"
        colnames(data_set_y)[1] <- "activity"
        
        
        ## Merging data_set_X, data_set_y and data_set_subject
        data_set <- cbind(data_set_subject, data_set_y, data_set_X)
        

        # Step 2
        
        ## Updating column names in the data_set with feature names, 
        ## extracting vairiables names which include mean, std, subject, 
        ## activity and excluding meanFreq
        
        feature.names <- as.character(features[,2]) 
        colnames(data_set)[3:563] <- feature.names
        
        data_set.ex <- data_set[ ,setdiff(grep('mean|std|subject|activity', 
                                              colnames(data_set)), 
                                         grep('meanFreq', colnames(data_set)))]
        
        
        # Step 3
        
        ## Obtaining a list of required activity labels
        labels.list <- as.character(activity_labels[,2])
        
        ## Naming the activities with descriptive activity names 
        data_set.ex$activity[data_set.ex$activity == 1 ] <- labels.list[1]
        data_set.ex$activity[data_set.ex$activity == 2 ] <- labels.list[2]
        data_set.ex$activity[data_set.ex$activity == 3 ] <- labels.list[3]
        data_set.ex$activity[data_set.ex$activity == 4 ] <- labels.list[4]
        data_set.ex$activity[data_set.ex$activity == 5 ] <- labels.list[5]
        data_set.ex$activity[data_set.ex$activity == 6 ] <- labels.list[6]
            
        
        # Step 4 
        
        ## Making the variable names more descriptive by excluding punctuations
        ## and updating the first letters of std and mean 
        colnames(data_set.ex) <- gsub("[[:punct:]]", "", colnames(data_set.ex))
        colnames(data_set.ex) <- gsub("mean", "Mean", colnames(data_set.ex))
        colnames(data_set.ex) <- gsub("std", "Std", colnames(data_set.ex))
        
        
        # Step 5
        
        ## Using the data from step 4 to create a second, independent tidy data 
        ## set with the average of each variable for each activity and each subject
        library(dplyr)
        
        data.ind <- data_set.ex %>%
                group_by(subject, activity) %>%
                summarise_each(c("mean")) %>%
                arrange(subject)
        
        ## converting data.ind into a dataframe class object
        data.ind.df <- as.data.frame(data.ind)
        
        setwd("~/Dropbox/Data_Science/Coursera/GaCD/UCI_HAR_Dataset")
           
        write.table(data.ind.df, "tidyData.txt", row.name=FALSE)   
        
}

run_analysis()

