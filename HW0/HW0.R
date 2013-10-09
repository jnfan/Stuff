set.seed(1) # set random seed.

# Jing Fan (Student ID:998606600)

# Q1
# Write a program that prints the numbers from 1 to 100. But for multiples of 
# three print "Fizz" instead of the number and for the multiples of 
# five print "Buzz". For numbers which are multiples of both three and five print "FizzBuzz". 
for(i in 1:100)
{
  if(i %% 15 == 0) { print("FizzBuzz") }
  else if(i %% 5 == 0) { print("Buzz") }
  else if(i %% 3 == 0) { print("Fizz") }
  else { print(i) }
}


# Q2 Write a program that generates 10,000 uniform random numbers ....
x <- runif(10000,min=0,max=2*pi)
y <- runif(10000,min=0,max=1)
u <- y*cos(x)
v <- y*sin(x)
r <- sqrt(u^2 + v^2)
d = cbind(u,v)
plot(u,v)
hist(r) # looks like uniform


# Q3 
s <- 'Hello, my name is Bob. I am a statistician. I like statistics very much.'
dir.create("q3Out") # create folder
# write the letters to the files
for(i in 1:nchar(s)) 
{
  tempString <- substr(s,i,i)
  tempFile <- paste(getwd(), "/q3Out/out_",i,".txt",sep="")
  write(tempString, tempFile, sep = "\t")
}

# read the letters from the files
str <- NULL # initialize the string
for(i in 1:72) # could automate the number, that is, judge how many files exist
{
  tempFile <- paste(getwd(), "/q3Out/out_",i,".txt",sep="")
  tempString <- readLines(tempFile)
  str <- paste(str, tempString)
}
print(str) # the font doesn't work so far


# Q6 consider the autoregressive process of order 1, 
# usually called an AR(1) process:
# a
# define function
generate <- function(N){
all <- 0
rou <- 0.5
for(i in 1:N)
{  
  randN <- rnorm(1, mean = 0, sd = 1)
  all <- c(all, rou*all[length(all)] + randN)
} 
all <- all[-1]
return(all)
}
all <- generate(1000)
plot(all,type="l")
# b
m <- NULL
for(i in 1:200) m <- cbind(m,generate(1000))
# c
meanS <- apply(m,2,mean)
plot(meanS)
# d
varS <- apply(m,2,var)
plot(varS)
# c 
# theoretical justification


# Q7 *** using Monte Carlo integration.
# a
x <- rnorm(10000, mean = 0, sd = 1)
y <- exp(-x^2)
mean(y)
# b. truncated normal
x <- rnorm(10000, mean = 0, sd = 1)
ix <- which(x > 1 | x < -2)
x <- x[-ix] # truncate
mean(x)


# Q8 
B <- 1000
yV <- NULL
betaHat <- NULL
for(i in 1:B){
n <- 100
x <- cbind( 1,rnorm(n, mean = 0, sd = 1),rnorm(n, mean = 0, sd = 1)) 
eps <- rnorm(n, mean = 0, sd = 1)
beta <- as.matrix(c(1.2,0.3,-0.9), ncol=1, nrow=3)
y <- x %*% beta + eps
thisBetaHat <- solve(t(x) %*% x) %*% t(x) %*% y
thisBetaHat <-  t(thisBetaHat)
betaHat <- rbind(betaHat, thisBetaHat) 
}
apply(betaHat,2,sd) #-- standard error 
#-- 0.1  0.1  0.1


y <- data.frame(y)
pred <- x[,c(2,3)]
d <- cbind(pred,y)
model <- lm(y~., d) # produce a linear model
summary(model)  #-- see std. Error

