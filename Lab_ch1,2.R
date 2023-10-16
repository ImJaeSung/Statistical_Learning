
##########
## Vector 
##########
x = c(1,5,6)
x2 <- c(1,5,6)
# Are x and x2 the same?

#########
## List
#########
ls()
rm(x, x2)

########
## Matrix
########
x = matrix(data = c(1,2,3,4), nrow=2, ncol=2)
x
xt = matrix(data = c(1,2,3,4), nrow=2, ncol=2, byrow=T)
xt
x2 = matrix(c(1,2,3,4), 2, 2)
x2

### Matrix operation
x+xt
x*x
x%*%x # matrix muplication

######
## generating random numbers
######
x = rnorm(100)
x
length(x)
x2 = rnorm(1000)
x2
length(x2)
mean(x)
var(x)
mean(x2)
var(x2)
head(x)
head(x2)

# what do you observe when you repeat the procedure above
set.seed(7)
x = rnorm(100)
x2 = rnorm(1000)
mean(x) # fix

#####
## Indexing Data
#####
A = matrix(1:16, 4,4)
A
A[1,3]
A[c(1,3),c(2,3)] # first and third rows, second and third columns
A[1,]
A[c(1,3),]
A[-1,] # except first row
A[-c(1,3),] # except first and thrid rows
dim(A)

#####
## Loading Data
####
getwd()
setwd("C:/Users/choi/Downloads/")
Auto <-read.csv("Auto.csv", na.strings = "?", stringsAsFactors = T)
View(Auto)
dim(Auto)
Auto[1:2, ]
Auto <- na.omit(Auto)

# Explore the data
dim(Auto)
summary(Auto)
summary(Auto$mpg)
attach(Auto) # make objects in data frames 
             # accessible without actually typing the name of the dataframe. 
summary(mpg)
names(Auto) # get name

################
### Drawing Plots
################
plot(cylinders , mpg)
plot(Auto$cylinders , Auto$mpg)
cylinders <- as.factor(cylinders) # trans factor, using categorical
plot(cylinders , mpg) 
plot(cylinders , mpg , col = "red")
plot(cylinders , mpg , col = "red", varwidth = T)
# the boxes are drawn with widths proportional to the square-roots 
# of the number of observations in the groups.
hist(mpg)
hist(mpg , col = 2)
hist(mpg , col = 2, breaks = 15) # the number of breaks manually 
                                 # make sure the number is not too high
pairs(Auto) # scatter matrix
pairs(
  ~ mpg + displacement + horsepower + weight + acceleration ,
  data = Auto
)
plot(horsepower , mpg)
identify(horsepower , mpg) 
# 'identify'reads the position of the graphics pointer 
# when the (first) mouse button is pressed

png("auto.png")
plot(horsepower , mpg)
dev.off() # close current device
