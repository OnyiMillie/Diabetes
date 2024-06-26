#check the dimmension of the data
dim(diabetes)
colnames(diabetes)
head(diabetes)
str(diabetes)

#loop and get the sum of 0 values count
for (i in colnames(diabetes)){
  print(paste("The number of 0(s) in the colume", i, "is", sum(diabetes[i]==0)))
}