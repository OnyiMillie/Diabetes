#Exploration of diabetes dataset for analysis
#open and read the diabetes file
diabetes <- read.csv("Data/diabetes.csv")
str(diabetes)
summary(diabetes)


#preview the dataset to check the dimmension
dim(diabetes)
colnames(diabetes)
head(diabetes)
str(diabetes)


#run the head function to see the first 20 rows by default
head(diabetes, 20)

#summary of the dataset
summary(diabetes)

#skim the dataset to display missing values, quantile information 
#and  inline histogram for each variable
library(skimr)
skim(diabetes)

#pull a full data profile of the data frame
library(DataExplorer)
DataExplorer::create_report(diabetes)




#loop and get the sum of 0 values count
for (i in colnames(diabetes)){
  print(paste("The number of 0(s) in the colume", i, "is", sum(diabetes[i]==0)))
}

library(corrplot) 
head(diabetes)

# Check the summary of the file
summary(diabetes)

# Check for missing values
colSums(is.na(diabetes))

head(diabetes) 
#correlation matrix 
M<-cor(diabetes) 
head(round(M,2))

head(diabetes) 
#correlation matrix 
M<-cor(diabetes) 
head(round(M,2)) 
  
#visualizing correlogram 
#as circle 
corrplot(M, method="circle") 
# as pie 
corrplot(M, method="pie") 
# as colour 
corrplot(M, method="color") 
# as number 
corrplot(M, method="number")

head(diabetes) 

# correlation matrix 
M<-cor(diabetes) 
head(round(M,2)) 

# types 
# upper triangular matrix 
corrplot(M, type="upper") 

# lower triangular matrix 
corrplot(M, type="lower")

#check the p-value of correlations of the dataset
head(diabetes) 
M<-cor(diabetes) 
head(round(M,2)) 

# mat : is a matrix of data 
# ... : further arguments to pass  
# to the native R cor.test function 
cor.mtest <- function(mat, ...)  
{ 
  mat <- as.matrix(mat) 
  n <- ncol(mat) 
  p.mat<- matrix(NA, n, n) 
  diag(p.mat) <- 0 
  for (i in 1:(n - 1))  
  { 
    for (j in (i + 1):n) 
    { 
      tmp <- cor.test(mat[, i], mat[, j], ...) 
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value 
    } 
  } 
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat 
} 

# matrix of the p-value of the correlation 
p.mat <- cor.mtest(diabetes) 
head(p.mat[, 1:9])

#check the Sig level of correlations of the file
head(diabetes) 
M<-cor(diabetes) 
head(round(M, 2)) 

library(corrplot) 

# mat : is a matrix of data 
# ... : further arguments to pass  
# to the native R cor.test function 
cor.mtest <- function(mat, ...) 
{ 
  mat <- as.matrix(mat) 
  n <- ncol(mat) 
  p.mat<- matrix(NA, n, n) 
  diag(p.mat) <- 0 
  for (i in 1:(n - 1))  
  { 
    for (j in (i + 1):n) 
    { 
      tmp <- cor.test(mat[, i], mat[, j], ...) 
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value 
    } 
  } 
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat 
} 

# matrix of the p-value of the correlation 
p.mat <- cor.mtest(diabetes) 
head(p.mat[, 1:5]) 

# Specialized the insignificant value 
# according to the significant level 


# Leave blank on no significant coefficient 
corrplot(M, type = "upper", order = "hclust",  
         p.mat = p.mat, sig.level = 0.01,  
         insig = "blank")

#Lets split the final dataset to training and test data to build model and prediction
n_obs <- nrow(diabetes)
split <- round(n_obs * 0.7)
train <- diabetes[1:split,]
# Create test
test <- diabetes[(split + 1):nrow(diabetes),]
dim(train)
dim(test)


# Reading the train.csv by removing the  
# last  empty column
diabetes <- read.csv("Data/diabetes.CSV")[,(ncol(data)-1)]


predict_diabetes <- function(pregnancies, glucose, bloodpressure, skinthickness, 
                             insulin, bmi, diabetespedigreefunction, age) {
  input_data <- data.frame(
    Pregnancies = pregnancies,
    Glucose = glucose,
    BloodPressure = bloodpressure,
    SkinThickness = skinthickness,
    Insulin = insulin,
    BMI = bmi,
    DiabetesPedigreeFunction = diabetespedigreefunction,
    Age = age
  )
  input <- as.data.frame(input_data)
  prediction <- predict(log_model, newdata = input, type = "response")
  prediction <- factor(ifelse(prediction > 0.5, 1, 0), 
                       levels = levels(as.factor(prediction)))
  
  return(prediction)
}
# Handle missing values
diabetes <- na.omit(diabetes)

# Encode categorical variables if necessary
diabetes <- as.factor("diabetes")

# Normalize numerical features if necessary
# method = range for normalisation 
diabete = scale(diabete[,1:8], method = "range", range = c(0, 5))
summary(diabete)


#correlation between different attributes of the dataset (Age, BMI and Blood Pressure)
#patients ages
ggplot(data = diabete, aes(x = Age)) + geom_histogram(bins = 30, color = "green", fill = "lightblue") 
+ facet_wrap(Outcome) + theme_dark() + ylab("Number of Patients") + labs(title = "Age(s) of Patients")

#Patients blood pressure
ggplot(data = diabete, aes(x = BloodPressure)) + geom_histogram(bins = 30, color = "green", fill = "lightblue") 
+ facet_wrap(~Outcome) + theme_dark() + ylab("Number of Patients") + labs(title = "Patient Blood Pressure")

#patients BMI
ggplot(data = diabete, aes(x = BMI)) + geom_histogram(bins = 30, color = "green", fill = "lightblue") 
+ facet_wrap(~Outcome) + theme_dark() + ylab("Number of Patients") + labs(title = "BMI of Patients")

#dataset preparation for normalization
diabete_train <- diabete[1:668, ]
diabete_test <- diabete[669:768, ]

diabete_train_labels <- diabete[1:668, 9]
diabete_test_labels <- diabete[669:768, 9]



#model training for predictions
#check for duplicates or missing values.
sapply(diabetes, function(x) table(is.na(x)))
table(duplicated(diabetes))
diabetes <- diabetes[!duplicated(diabetes),]






#inspect and vidualise the dataframe
#Distribution of diabetes outcome
outcome_counts <- table(diabetes)
outcome_diabetes <- data.frame(Outcome = names(outcome_counts), 
                         Count = as.numeric(outcome_counts))

# Create Histograms with Outcome Split
# Select relevant columns
diabetes <-  c("Pregnancies", "Glucose", "BloodPressure", 
                            "BMI", "Age", "Outcome")

library(ggplot2)
# Histograms by pregnancy outcomes
ggplot2:: aes(x = "Pregnancies", y = "count", fill = factor(Outcome)) +
  histogram(position = "identity", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Pregnancies by Outcome") +
  facet_wrap(~Outcome, frequency("free_y")) +
  theme_minimal()


# Boxplot by BMI Outcome
ggplot2:: aes(x = "BMI", y = factor(Outcome), fill = factor(Outcome)) +
  boxplot(position = "identity", bins = 30, alpha = 0.7) +
  labs(title = "BMI Distribution by Outcome") +
  theme_minimal()

#prepare and preprocess data
# Split the data into training and testing sets 
train_idx <- sample(1:nrow(diabete), 0.7 * nrow(diabete)) 
train_data <- diabete[train_idx, ] 
test_data <- diabete[-train_idx, ]


# Train the random forest model 
library(readr)
diabetes <- read_csv("Data/diabetes.csv")
head(diabetes)
str(diabetes)
attach(diabetes)


diabetes$Outcome <- ifelse(diabetes$Outcome == 1,"Not Healthy", "Healthy")
diabetes$Outcome <- as.factor(diabetes$Outcome)


#cross-validation with 10 folds to build the mode
set.seed(456)
library(caret)
train_control <- trainControl(method="cv", number=10)

model <- train(Outcome~., data=diabetes, trControl=train_control, method="rf")
print(model)

#building of random forest for the model

for (split_number in c(1:100)){
  train_ind <- split(diabetes$Pregnancies,Ratio = 0.10)
  test_ind <- !train_ind
  rf <- randomForest(as.factor(Outcome) ~ ., data = diabetes[train_ind,],ntree=100)
  train_accuracy <- sum(diag(rf$confusion))/sum(train_ind)
  cm <- table(predict(rf,diabetes[test_ind,]),diabetes$Outcome[test_ind])
  test_accuracy <- sum(diag(cm))/sum(test_ind)
  
  all_train_accuracies_rf[split_number] <- train_accuracy
  all_test_accuracies_rf[split_number] <- test_accuracy
  
  importance <- rf$importance/sum(rf$importance)
  all_importances_rf[split_number,1] <- importance["Glucose",]
  all_importances_rf[split_number,2] <- importance["BMI",]
  all_importances_rf[split_number,3] <- importance["Age",]
  all_importances_rf[split_number,4] <- importance["Insulin",]
  
  
  install.packages("gbm")
  library(gbm)
  all_gb_accuracies <- matrix(nrow=100)
  all_gb_relative_inf <- matrix(nrow=100,ncol=10)
  for (split_number in c(1:100)){
    train_ind <- split(diabetes$Pregnancies,SplitRatio = 0.10)
    test_ind <- !train_ind
    gb <- gbm(Outcome ~ ., data = diabetes[train_ind,], distribution = "bernoulli")
    vals <- predict.gbm(gb, diabetes[test_ind,],n.trees=100)
    probs <- exp(vals)/(1+exp(vals))
    class1 <- probs>0.5
    cm <- table(class1,diabetes$Outcome[test_ind])
    gb_accuracy <- sum(diag(cm))/sum(test_ind)
    all_gb_accuracies[split_number] <- gb_accuracy
    
    s <- summary.gbm(gb,plotit = FALSE)
    all_gb_relative_inf[split_number,1] <- s$rel.inf[s$var=="Glucose"]
    all_gb_relative_inf[split_number,2] <- s$rel.inf[s$var=="BMI"]
    all_gb_relative_inf[split_number,3] <- s$rel.inf[s$var=="Age"]
    all_gb_relative_inf[split_number,4] <- s$rel.inf[s$var=="Insulin"]
    all_gb_relative_inf[split_number,5] <- s$rel.inf[s$var=="DiabetesPedigreeFunction"]
    all_gb_relative_inf[split_number,6] <- s$rel.inf[s$var=="Pregnancies"]
    all_gb_relative_inf[split_number,7] <- s$rel.inf[s$var=="BloodPressure"]
    all_gb_relative_inf[split_number,8] <- s$rel.inf[s$var=="SkinThickness"]
  }
  all_importances_rf[split_number,5] <- importance["DiabetesPedigreeFunction",]
  all_importances_rf[split_number,6] <- importance["Pregnancies",]
  all_importances_rf[split_number,7] <- importance["BloodPressure",]
  all_importances_rf[split_number,8] <- importance["SkinThickness",]
}