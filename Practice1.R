#Create a dataframe
n = c("aa", "bb", "cc", "dd", "ee")
age = c(25,26,27,28,29)
maths = c(29,78,90,56,88)
sc = c(45,67,88,66,55)
total = maths + sc
df1 = data.frame(n,age,maths,sc,total)
View(df1)
pct_maths = round((maths / total) * 100, digits = 2)
df1$maths_percent <- pct_maths #add a new column

student <- df1
student <- student[,c(2:6)] #remove a column
View(student)
student2 <- student[,"age"]

#Rename a column
names(student) = c("A","B","C","D","F") 
names(student)[names(student) == 'F'] <- 'New'
names(student)[2] <- 'Second'

log_age = log(student$age)
student$Log_age <- log_age

inverse_age = 1/student$age
student$inverse_age <- inverse_age

exp_age = exp(student$age)/mean(student$age)
student$exp_age <- exp_age

sqr_age = student$age*student$age
student$sqr_age <- sqr_age

sqrt_age = sqrt(student$age)
student$sqrt_age <- sqrt_age

#change datatype of the column
class(student$n)
student$n = as.character(student$n)
class(student$n)

#Install R GUI
install.packages("Rcmdr")
library("Rcmdr")

dt <- read.csv("C:\\Users\\Subhasish Sahu\\Documents\\R\\data_2017\\train.csv")
View(dt)




























