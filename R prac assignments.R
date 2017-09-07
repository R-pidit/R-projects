#question 1
install.packages("data.table")
library(datasets)

#question 2
women <- data.frame(women)
View(women)

meanHeight <- mean(women$height)
meanWeight <- mean(women$weight)
meanHeight
meanWeight
  
result <- subset(women,height > meanHeight && weight < meanWeight, select =1:weight)
View(result)

#question 3
## Reading data from Comma separated file (csv)
input_csv.df <- read.csv(file="C:\\Users\\Subhasish Sahu\\Documents\\R\\data_2017\\dataset.csv")
View(input_csv.df)

#question 4
install.packages("xml2")
library(xml2)

#Assignment 3
df <- data.frame("ID","Income","Gender")
normal_income = rnorm(100,sd = 75000, mean = 250000)
seq <- sample(1:100, 100, replace = FALSE)
gender <- as.vector(x)
for(q in 1:100){
  if(seq[q] <= 60) {
    gender[q] <- "M"    
  }else{
    gender[q] <- "F"
  }
}
gender
df <- data.frame(1:100, normal_income, gender)

View(df)
