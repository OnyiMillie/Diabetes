#open and read the diabetes file
diabetes <- read.csv("Data/diabetes.csv")

#check the dimmension of the data
dim(diabetes)
colnames(diabetes)
head(diabetes)
str(diabetes)

#loop and get the sum of 0 values count
for (i in colnames(diabetes)){
  print(paste("The number of 0(s) in the colume", i, "is", sum(diabetes[i]==0)))
}

library(corrplot) 
head(diabetes)

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

#check the p-value of correlations of the file
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
corrplot(M, type = "upper", order = "hclust",  
         p.mat = p.mat, sig.level = 0.01) 

# Leave blank on no significant coefficient 
corrplot(M, type = "upper", order = "hclust",  
         p.mat = p.mat, sig.level = 0.01,  
         insig = "blank")