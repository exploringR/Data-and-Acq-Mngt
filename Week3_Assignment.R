## Week 3  Assignment - Shipra Ahuja

## Solution 1 - Write a function that takes a vector as input and returns the number of 
## missing values in the vector.

missingvalsx <- function(x)
{
  count <- length(which(is.na(x)))
  return (count)
}

##Result -
##> missingvalsx(c(1,2,3,NA,5,NA,7,NA,8,NA,NA))
##[1] 5


## Solution 2 - Write a function that takes a data frame as input and returns a named vector with the number of missing values in each column
## of the data frame.

missingvalsdf <- function(df)
{ 
  resultmv <- sapply(df, function(x) sum(is.na(x)))
  return (resultmv)
}  

## Result -
## Self created Dataframe - 

#Name <-  c("David",  NA, "Tom", "Bob", "Matt", NA, NA,"Peter", "Anne", "Paige")
#Age  <- c(30, NA, 60, NA, 40, 25, 56, 45, 73, 62)
#DateofRetirement <-  as.Date(c("2020-08-31", "2015-09-30", NA, "2017-03-01", "2014-12-15", NA, "2015-11-30", NA,  "2018-12-31", "2019-07-30"))
#Salary <- c(40000, 65000, NA, 25000, 70000, 30000, NA, 80000, NA, NA)
#df <- data.frame(Name, Age, DateofRetirement,Salary,stringsAsFactors=FALSE)

##> missingvalsdf(df)
##Name              Age DateofRetirement           Salary 
##3                2                3                4 


##Solution 3 - Write a function that takes a numeric vector as input and uses 
##it to determine the minimum, the maximum, the mean, the median, the first quartile, the third quartile, the standard deviation of the vector, and the number
##of missing values.

findsumm <- function(x)
{
    ##Find Missing Values
    missingvals <- length(which(is.na(x)))
    
    x <- na.omit(x) 
    
    ##Find minimum
    min <- sort(x,decreasing=FALSE)
    min <-  min[1]

    ##Find maximum
    max <- sort(x,decreasing=TRUE)
    max <- max[1]
    
    ##Find Mean
    sum <- 0
    for (i in 1:length(x))
    {
      sum <- sum + x[i]
    }
    mean <- sum/length(x)
    
    ##Find standard deviation    
    sumxi <- 0
    for (i in 1:length(x))
    {
      sumxi <-  sumxi + (x[i]-mean)^2
    }
    stddev <- sqrt((1/(length(x)-1)) * sumxi)
    
    ##Find median
    
    sortedx <- sort(x,decreasing=FALSE)
    medianpos <- (length(sortedx) + 1)/2
    if (length(sortedx)%%2 != 0)
    {
      for (i in 1 : length(sortedx))
      {
        if (i == medianpos)
        {
          medianval <- sortedx[i]
        }
      }
    }else
    {
      if(length(sortedx)%%2 == 0)
      {
        for (i in 1 :length(sortedx))
        {
          if (medianpos - i == 0.5)
          {
            medianval <- (sortedx[i] + sortedx[i+1])/2
          }
        }
      }
    }
    
    ##Find 1st quartile
    xlower <- x[x < medianval]
    
    sortedx <- sort(xlower,decreasing=FALSE)
    medianpos <- (length(sortedx) + 1)/2
    if (length(sortedx)%%2 != 0)
    {
      for (i in 1 : length(sortedx))
      {
        if (i == medianpos)
        {
          firstquart <- sortedx[i]
        }
      }
    }else
    {
      if(length(sortedx)%%2 == 0)
      {
        for (i in 1 :length(sortedx))
        {
          if (medianpos - i == 0.5)
          {
            firstquart <- (sortedx[i] + sortedx[i+1])/2
          }
        }
      }
    }
    
    
    ##Find 3rd quartile
    xhigher <- x[x > medianval]
    
    sortedx <- sort(xhigher,decreasing=FALSE)
    medianpos <- (length(sortedx) + 1)/2
    if (length(sortedx)%%2 != 0)
    {
      for (i in 1 : length(sortedx))
      {
        if (i == medianpos)
        {
          thirdquart <- sortedx[i]
        }
      }
    }else
    {
      if(length(sortedx)%%2 == 0)
      {
        for (i in 1 :length(sortedx))
        {
          if (medianpos - i == 0.5)
          {
            thirdquart <- (sortedx[i] + sortedx[i+1])/2
          }
        }
      }
    }
        
    ##Result
    results <- list(MissingValues=missingvals,Minimum=min,Maximum=max,Mean=mean,Median=medianval,FirstQuartile=firstquart,ThirdQuartile=thirdquart,StandardDeviation=stddev)
    return (results)
}

## Result - 
##> x <- c(NA,5,4,7,8,6,3,2,1,NA)
##> summary(x)
##$MissingValues
##[1] 2

##$Minimum
##[1] 1

##$Maximum
##[1] 8

##$Mean
##[1] 4.5

##$Median
##[1] 4.5

##$FirstQuartile
##[1] 2.5

##$ThirdQuartile
##[1] 6.5

##$StandardDeviation
##[1] 2.44949


##Solution 4 - Write a function that takes a character or factor vector and determines the number of distinct elements in the vector, the most commonly occurring element, the number of times the most commonly 
##occurring element occurs, and the number of missing values.

find <- function(x)
{
  distinct <- length(unique(x))
  mostfreq <- sort(table(x),decreasing=TRUE)
  mostfreqocc <- names(mostfreq[which.max(mostfreq)]) ## Gives most frequent element
  mostfreqcount <- as.matrix(mostfreq)[1]             ## Gives count of most frequent element
  nmissingvals <- length(which(is.na(x)))
  
  result <- list(Distinct=distinct,MostFrequent=mostfreqocc,MostFreqCount=mostfreqcount,MissingValsCount=nmissingvals)
  return (result)
}  

##Result - 
##> find(c("c","b","a","a","d","d","d","e","c",NA,"c",NA))
##$Distinct
##[1] 6

##$MostFrequent
##[1] "c"

##$MostFreqCount
##[1] 3

##$MissingValsCount
##[1] 2


## Solution 5 - Write a function that takes a logical vector and determines the number of true values, the number of false values, the proportion of 
## true values, and the number of missing values.

logicalvector <- function(x)
{
    truecount<- table(x)["TRUE"]
    falsecount <- table(x)["FALSE"]
    proportionoftrue <- truecount/(truecount + falsecount)
    cntmissingvals <- length(which(is.na(x)))
    result <- list(CountTrue=truecount, CountFalse=falsecount,ProportionofTrue=proportionoftrue,MissingValsCount=cntmissingvals)
    return (result)
}

## Result - 
##> logicalvector(c(T,F,T,T,T,T,F,F,F,F,F,T,T,T,F,NA,T,T,NA,F,F,NA))
##$CountTrue
##TRUE 
##10 

##$CountFalse
##FALSE 
##9 

##$Proportion
##TRUE 
##0.5263158 

##$MissingValsCount
##[1] 3


##Solution 6 - Write a function that takes as its input a data frame and returns a summary of its columns using 
## the functions you write for questions 3-5.

colsumm <- function(df)
{
 ## summ1 <- sapply(df, function(x) findsumm(x))
    summ2 <- sapply(df, function(x) find(x))
    summ3 <- sapply(df, function(x) logicalvector(x))
    summ <- list(summ2,summ3)
    return (summ)
}

## Result -
#Name <-  c("David",  NA, "Tom", "Bob", "Bob", NA, NA,"Peter", "Anne", "Paige")
#Age  <- c(30, NA, 60, NA, 45, 25, 56, 45, 73, 62)
##Ageabove40 <- c(F,NA,T,NA,F,F,T,T,T,T)
#df2 <- data.frame(Name, Age, Ageabove40,stringsAsFactors=FALSE)

##> colsumm(df2)
##[[1]]
##Name  Age  Ageabove40
##Distinct         7     8    3         
##MostFrequent     "Bob" "45" "TRUE"    
##MostFreqCount    2     2    5         
##MissingValsCount 3     2    2         

##[[2]]
##Name Age Ageabove40
##CountTrue        NA   NA  5         
##CountFalse       NA   NA  3         
##ProportionofTrue NA   NA  0.625     
##MissingValsCount 3    2   2   

