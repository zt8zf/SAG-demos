##October 5, 2017
##Slightly commented code on adjusted plus-minus

##Using code and data from below websites
##How to calculate adjusted plus-minus

#Install and load packages
#Install the packages there that you don't have
install.packages("glmnet")
library(glmnet) #load glmnet package

library(readxl)
library(readr)

#https://web.archive.org/web/20140717131517/http://www.hickory-high.com/how-to-calculate-rapm/

#Code from https://gist.github.com/jacobfrankel/11032065
  
#Other files to read in - ignore this part
#apmdat <- read_excel("~/Desktop/ucsb-year2/ucsb2-spring/misc/rapmapmdatdownload.xlsx")
#apmcsv <- read.csv("~/Desktop/ucsb-year2/ucsb2-spring/misc/apm.csv")
#apmcsv <- read_csv("~/Desktop/ucsb-year2/ucsb2-spring/misc/apmcsv.csv")

#Run this line of code
impcsv <- read_csv("~/Desktop/ucsb-year2/ucsb2-spring/misc/import2.csv")

#Print the first few lines
head(impcsv)

#Convert the object to a data frame
impcsv.df <- data.frame(impcsv)

#Look at its dimensions
dim(impcsv.df)

#Read in this file
names.id <- read_csv("~/Desktop/ucsb-year2/ucsb2-spring/misc/names-ids.csv")

#Make a data frame
names.df <- data.frame(names.id)

#Remove the possessions column
names.df$Poss <- NULL

#create a separate vector for margin
Marg=impcsv$MarginPer100 

#create a separate vector for possessions
Poss=impcsv$Possessions 

#create a separate vector for rebound rate differential
RebMarg=(impcsv$RebRateHome-(100-impcsv$RebRateHome)) 

#remove the possessions column from the impcsv frame
impcsv$Possessions=NULL 

#remove the home rebound rate column from the impcsv frame
impcsv$RebRateHome=NULL

#remove the margin column from the impcsv frame
impcsv$MarginPer100=NULL 

#remove the rebound margin column from the impcsv frame
impcsv$RebMarg=NULL 

x=data.matrix(impcsv) #turn the impcsv frame (which is now just 1s, -1s, and 0s) into a matrix

#Print the first few lines
head(x)

##This next part is some statistics you may not understand yet. Don't worry too much about this part, I just wanted you to have R code you can go through to get you back into it

#find the lambda values. these determine how far towards 0 the coefficients are shrunk
pt <- proc.time()
lambda=cv.glmnet(x,Marg,weights=Poss,nfolds=5) 
proc.time() - pt

#store the lambda value that gives the smallest error in an object called lambda.min
lambda.min=lambda$lambda.min 

#run the ridge regression. x is the matrix of independent variables, Marg is the dependent variable, Poss are the weights. alpha=0 indicates the ridge penalty.
ridge=glmnet(x,Marg,family=c("gaussian"),Poss,alpha=0,lambda=lambda.min) 

#extract the coefficient for each of the independent variables (players) for the lambda with the minimum error
output <- coef(ridge,s=lambda.min) 

#Look at the dimensions
dim(output)

#Some data manipulation
output.df <- cbind(output[,1])

#Make it a data frame
output.df <- data.frame(output.df)
output.df

#Order the data frame in decreasing order. This sorts it by who has the highest adjusted plus-minus, and goes down from there.
dec.order <- order(output.df,decreasing=T)

#Print the first few lines
head(output.df[dec.order,])

#More data manipulation 
output.df1 <- cbind(output.df,rownames(output.df))
output.df1

#Drop first row of output.df since it's the intercept.
output.df1 <- output.df1[-1,]
class(output.df1)

#Merge output.df1 with the names of the players
df.merge <- merge(x=output.df1,y=names.df,by.x="rownames(output.df)",by.y="PlayerID")

#Find the decreasing order
dec.order.merge <- order(df.merge$output.df,decreasing=T)

#Print the first few rows in decreasing order
df.merge[dec.order.merge[1:50],]

##here we are!

#This makes a barplot of the top 150 adjusted plus-minus in descending order. Just a basic visualization.
barplot(df.merge[dec.order.merge[1:150],"output.df"])

