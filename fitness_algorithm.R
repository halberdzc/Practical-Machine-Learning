

#Practical Machine Learning
    #Course Project by Yuri Plotkin. Sunday, July 26th, 2015.


library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)

set.seed(1234)

#Loading and Cleaning the Data:

    train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    
    #Storing data files in memory
    train_data <- read.csv(url(train_url), na.strings=c("NA","#DIV/0!",""))
    test_data <- read.csv(url(test_url), na.strings=c("NA","#DIV/0!",""))
    
    #Dimension Check (to know the number of variables in the data frame)
    dim_train <- dim(train_data)
    dim_test <- dim(test_data)
    
    #Remove columns that are incomplete(have missing values)
    train_data_new <- train_data[,colSums(is.na(train_data)) == 0]
    test_data_new <- test_data[,colSums(is.na(test_data)) == 0]
    
    #Minimize the size of the data frame by getting rid of variables you do not need 
    #(user_name, raw_timestamp_part_1, raw_timestamp_part_, 2 cvtd_timestamp, new_window, 
    #and num_window; columns 1 to 7)
    train_data_new <- train_data_new[,-c(1:7)]
    test_data_new <- test_data_new[,-c(1:7)]
    
    #Re-check the dimensions of the new training and test data sets
    dim_train_new <- dim(train_data_new)
    dim_test_new <- dim(test_data_new)

    
#Partition the Training Data set:
    #To perform cross-validation, the 19,622 observations will be partitioned
    #into two data sets, a 60/40 split going into training, and testing respectively
    data_train <- createDataPartition(y=train_data_new$classe, p=0.60, list=FALSE)
    Training <- train_data_new[data_train, ]
    Testing <- train_data_new[-data_train, ]
    dim(Training)
    dim(Testing)
    
    
#Prediction Modeling: Will use the variable "classe" to test prediction of the algorithm
    
    #1. Decision Tree Prediction Model
        
            #Decision Tree Model
            dec_tree <- rpart(classe ~ ., data=Training, method="class")
            
            #Predicting
            dec_tree_predict <- predict(dec_tree, Testing, type = "class")
            
            #Plotting
            win.graph()
            fancyRpartPlot(dec_tree, cex=.5,under.cex=1,shadow.offset=0)
            
            #Testing results on the myTesting data set
            confusionMatrix(dec_tree_predict, Testing$classe)
    
    #2. Random Forest Prediction Model
        
            #Decision Tree Model
            random_forest <- rpart(classe ~ ., data=Training, method="class")
            
            #Predicting
            rand_forr_predict <- predict(random_forest, Testing, type = "class")
            
            #Plotting
            win.graph()
            fancyRpartPlot(random_forest, cex=.5,under.cex=1,shadow.offset=0)
            
            #Testing results on the myTesting data set
            confusionMatrix(rand_forr_predict, Testing$classe)
    
#Creating a text file with the Random Forest Prediction
            
    # Applying Random Forest algorithm to the original Testing data set
    final_model <- predict(random_forest,  test_data, type = "class")
    final_model
    
    
    # Generating files with predictions to submit for assignment
    pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
    }
    
    pml_write_files(final_model)