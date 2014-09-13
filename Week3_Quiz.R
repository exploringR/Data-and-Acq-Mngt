
## Week 3 Quiz -- Shipra Ahuja  

## Solution 1 - Function to accept numeric vector and calculate mean of observations in vector

calcmean<- function(x,func=mean)
{
  if (is.numeric(x))
  {
    do.call(func, args=list(x))
  }else
  {
    print ("Not a numeric input vector")
  }
}

## Results after executing R script -

## Case 1 - Numeric Vector

##> calcmean(c(1,2,3,4,5))
##>[1] 3

## Case 2 - Character vector

##> calcmean(c("a","b","c","d","e"))
##[1] "Not a numeric input vector"

## Case 3 -  Character vector

##> calcmean(c("1","2","3","4","5"))
##[1] "Not a numeric input vector"


## Solution 2 - Function to accept numeric vector with missing values and calculate
##mean of observations in the vector.

calcmean<- function(x,func=mean)
{
  if(is.numeric(x))
  {
    x <- na.omit(x)
    do.call(func, args=list(x))
  }else
  {
    print ("Not a numeric vector")
  }
}

## Results after executing R script -

##> calcmean(c(1,2,3,4,5))
##[1] 3

##> calcmean(c(1,NA,3,NA,NA))
##[1] 2

##> calcmean(c(1,6,3,8,NA))
##[1] 4.5

##> calcmean(c("1","6","3","8",NA))
##[1] "Not a numeric vector"


##Solution 3 - Function to accept two numeric input values and calculate GCD.

gcd <- function(x,y)
{
 if (is.numeric(x) && is.numeric(y))
 {
   factorx <- as.numeric(factorize(x))
   factory <- as.numeric(factorize(y)) 
   common <- intersect(factorx,factory)
   pdt <- 1
   for (i in 1:length(common))
   {
     pdt <- pdt * common[i] 
   }
   return (pdt)
  }else
  {
    print ("Not a numeric input")
  }  
}

## Result after executing R script - 

##> gcd(12,18)
##[1] 6

##> gcd(24,18)
##[1] 6

##> gcd(210,45)
##[1] 15

##> gcd(8,10)
##[1] 2

##> gcd(20,35)
##[1] 5

##> gcd("12",18)
##[1] "Not a numeric input"

## Solution 4 - Function to accept two numeric values and find GCD using 
## Eucidean's algorithm

gcd <- function(a,b)
{
 if (a < b)
 {
  temp1 <- a
  a <- b
  b <- temp1
 }
 while(b != 0)
 {
  temp2 <- b
  b <- a %% b
  a <- temp2
 } 
 return (a)
}
## Result after executing R script -

## Case 1 - a > b

##> gcd(24,18)
##[1] 6

## Case 2 - a < b

##> gcd(12,18)
##[1] 6
 

## Solution 5 - function that takes two numeric inputs x and y and calculates x^2 + 2xy -xy^2.

solveeqn <- function(x,y)
{
  if (is.numeric(x) && is.numeric(y))
  {  
   eqn <- ((x^2)*y) + (2 * x * y) - (x * (y^2))
   return (eqn)
  }else 
  {
    print ("Either x or y or both x & y are not numeric")
  }  
}

##Result after executing R script - 

##> solveeqn(2,3)
##[1] 6

##> solveeqn(-2,3)
##[1] 18

##> solveeqn("2",3)
##[1] "Either x or y or both x & y are not numeric"

##> solveeqn("2","3")
##[1] "Either x or y or both x & y are not numeric"

##> solveeqn(2,"3")
##[1] "Either x or y or both x & y are not numeric"



## Solution 6  - Read the csv files with ALL parms as default.

pricedata <- read.csv("week-3-price-data.csv")
modeldata <- read.csv("week-3-make-model-data.csv")
mergedata1 <- merge(pricedata,modeldata,by.x="ModelNumber",by.y="ModelNumber",all.x=FALSE,all.y=FALSE)

## Result after executing R script - 

## There are 27 observations in the merged dataset.
## Since the default value of ALL parameters is FALSE, it will not display the rows 
## which has missing values.

##SOlution 7 - Read the csv files 

pricedata <- read.csv("week-3-price-data.csv")
modeldata <- read.csv("week-3-make-model-data.csv")
mergedata2 <- merge(pricedata,modeldata,by.x="ModelNumber",by.y="ModelNumber",all.x=TRUE,all.y=TRUE)

## Result after executing R script - 

## There are 28 observations in merged dataset.It takes the additional 1 row from
## from price data for which there are missing values in model dataset.


##Solution 8 - Take your result from question 7 and subset it so that only the 2010 vehicles are included.

subsetvehicles <- subset(mergedata2,Year == 2010)

##Results after executing R script -

##ModelNumber ID  Color Mileage Price   Make    Model Year
##1         1091  1   Blue   36281 12400 Toyota    Camry 2010
##2         1091  6    Red   61130  9900 Toyota    Camry 2010
##3         1091 17 Silver   43017 11700 Toyota    Camry 2010
##4         1091 10   Blue   56095 10400 Toyota    Camry 2010
##5         1091 24   Blue   31204 12900 Toyota    Camry 2010
##6         1091 18   Blue   53126 10700 Toyota    Camry 2010
##14        1254  7    Red   68400  9200 Toyota  Corolla 2010
##15        1254  4  White   63624  9600 Toyota  Corolla 2010
##16        1254 26  Green   34716 12500 Toyota  Corolla 2010
##18        2111 19  Black   42945 11700   Ford    Focus 2010
##19        2111 16  White   36216 12400   Ford    Focus 2010
##21        2310 22 Silver   57672 10200   Ford Explorer 2010
##22        2310 23  Black   53942 10600   Ford Explorer 2010
##23        2310 28  White   37107 12300   Ford Explorer 2010


##Solution 9 - Take your result from question 7 and subset it so that only the red cars
##that cost more than $10,000 are included.

subsetred <- subset(mergedata2,(Color == "Red" & Price > 10000))

## Result after executing R script - 

##ModelNumber ID Color Mileage Price   Make    Model Year
##7         1142  3   Red   45827 11400 Toyota    Camry 2011
##25        2312 15   Red   42685 11700   Ford Explorer 2011
##26        2312 27   Red   30479 13000   Ford Explorer 2011
##27        2312 25   Red   52674 10700   Ford Explorer 2011
                    
##Solution 10 - Take your result from question 9 and subset it so that the ModelNumber 
##and Color columns are removed.

dropcols <- subset(subsetred,select = c(-ModelNumber,-Color))

## Result after executing R script - 

##ID Mileage Price   Make    Model Year
##7   3   45827 11400 Toyota    Camry 2011
##25 15   42685 11700   Ford Explorer 2011
##26 27   30479 13000   Ford Explorer 2011
##27 25   52674 10700   Ford Explorer 2011

##Solution 11 - function that takes as input a character vector and returns a numeric vector with the numbers of characters in each of 
##the elements in the original vector.

input <- function(charstring)
{
  if (is.character(charstring))
  {
    y <- strsplit(charstring," ")
    noofchars <- sapply(y,nchar)
    num <- as.numeric(noofchars)
    return (num)
  }else
  {
    print("Input is not a character vector")
  }
}

## Result after executing R script - 

## Case 1 - I/P is a character vector

##> input(c("fruits","veggies","juices","cereal","dairy"))
##[1] 6 7 6 6 5

##> class(num)
##[1] "numeric"

## Case 2 - Input is not a character vector

##> input(c(1,2,3,4,5))
##[1] "Input is not a character vector"


##Solution 12 - function that takes two character vectors of equal length and
##concatenates them element by element with a space as the separator. 
##Have the function die gracefully if the vectors are not the same length.

concatenate <- function(x,y)
{
  if (is.character(x) && is.character(y))
  {
    if (length(x)==length(y))
    {
      combined <- paste(x,y,sep=" ")
      return (combined)
    }else
    {
     print ("Not eligible for concatenation: x and y are not same length vectors")
     result <- NULL
     return (result)
    }
  }else
  {
    print ("Either x or y or both x and y are not character vectors")
  }
}    
  
## Result after executing R script -

##Case 1 - x numeric and y character vector.

##> concatenate(c(1,2,3,4,5),c("a","b","c","d","e"))
##[1] "Either x or y or both x and y are not character vectors"

## Case 2 - x character and y numeric vector

##> concatenate(c("a","b","c","d","e"),c(1,2,3,4,5))
##[1] "Either x or y or both x and y are not character vectors"

##Case 3- Both x and y are not character vectors

##> concatenate(c(1,2,3,4,5),c(1,2,3,4,5))
##[1] "Either x or y or both x and y are not character vectors"

##Case 4- Length of x and y is equal and both are character vectors

##> concatenate(c("Red","Yellow","Green"),c("Apple","Banana","Grapes"))
##[1] "Red Apple"  "Yellow Banana" "Green Grapes" 

##Case 5 - Length of x and y are not equal

##> concatenate(c("Red","Yellow","Green"),c("Apple","Banana"))
##[1] "Not eligible for concatenation: x and y are not same length vectors"
##NULL

  
##Solution 13 - function that takes a character vector and returns the substring of three characters
##that begins with the first vowel in the string. 
  
matchvowels <- function(x)
{
    vowel <- c("A","a","E","e","I","i","O","o","U","u")
    y <- strsplit(x,NULL)[[1]]
    
    for (i in 1:length(y))
    {
      pos <- match(y[i],vowel) 
      if(!is.na(pos))
          break
    } 
    if ((i+2) <= length(y)) {
 
      if(!is.na(pos))
      {
        result<-substr(x,i,i+2) 
      } else
      {
        result<-NULL
      }
    } else
    {
      result<-NULL
    }
  
    return(result)
}

##Result after executing R script - 

##> matchvowels(c("Hello World"))
##[1] "ell"
  
##Solution 14 - Suppose you have a data frame where one column gives the month 
##(in numeric format), the next gives the day, and the third column gives the 
##year. Use R to create such a data frame (by hand is fine) and then add a 
##fourth column with the date in date format.

month <- c(01,02,03,04,05,06,07,08,09,10,11,12)
day <- c(10:21)
year <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)
df <- data.frame(month,day,year)
concatall <- paste(df$month,"-",df$day,"-",df$year)
date <- mdy(concatall)
date <- as.Date(date)
df2 <- cbind(df,date)

##Result after executing R script - 
##month day year       date
##1      1  10 2001 2001-01-10
##2      2  11 2002 2002-02-11
##3      3  12 2003 2003-03-12
##4      4  13 2004 2004-04-13
##5      5  14 2005 2005-05-14
##6      6  15 2006 2006-06-15
##7      7  16 2007 2007-07-16
##8      8  17 2008 2008-08-17
##9      9  18 2009 2009-09-18
##10    10  19 2010 2010-10-19
##11    11  20 2011 2011-11-20
##12    12  21 2012 2012-12-21
  
## Solution 15 - Illustrate the code necessary to take a string of MM-DD-YYYY format 
##and convert it to a date.

d <- as.Date("09-08-2014",format = "%m-%d-%Y")
d2 <- format(d,"%m-%d-%Y")


##Result after executing R script -

##d <- as.Date("09-08-2014",format = "%m-%d-%Y")
##[1] "2014-09-08"

##d2 <- format(d,"%m-%d-%Y")
## [1] "09-08-2014"

  
## Solution 16 - Illustrate the code necessary to take a date and 
## extract the month of the date.

x <- mdy("09-08-2014")
print (month(x))

##Result after executing R script -

##[1] 9

##Month extracted is 9


## Solution 17  -Create a sequence of all of the dates from January 1, 2005, 
## to December 31, 2014.

dateseq <- seq(as.Date("2005-01-01"), as.Date("2014-12-31"), "days")
print (dateseq)

##Result after executing R script - 

## Total of 3652 dates are printed in sequential order. 
##As there are lot of dates,I have just specified the number of days and
##not printed all the dates here in the script.
