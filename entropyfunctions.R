## Entropy and Information Gain - Shipra Ahuja


dataset <-  read.csv("entropy-test-file.csv")

## Solution 1-  Create a function entropy() that takes a vector 
## as input and returns a single numeric value that is the entropy of the vector.

entropy <- function(d)
{
  if (is.numeric(d)|| is.character(d) || is.factor(d))
  {
    probability <- table(d)/length(d) ##Find probability
    probability <- as.data.frame(probability)[,2]
    entropy <- -sum(probability * log2(probability))
    return (entropy)
  }
}

## Solution 2 - Create a function infogain().

infogain <- function(d, a)
{
  
	cdata <- cbind(d,a)
	total_entropy <- entropy(d)
	nj <- table(a)
	combined_entropy <- 0
	for (i in 1:length(nj))
  	{
		cdata2 <- subset(cdata,a==names(nj[i]),select = d)
		combined_entropy <- combined_entropy + ((as.matrix(nj)[i] * entropy(cdata2))/length(d))
  	}
  	gain <- total_entropy - combined_entropy
  	return (gain)
}

## Solution 3 - Create the decide() function

decide <- function(ds, n)  
{
	gains <- list()	
	gainsdf <- data.frame()
	for (i in 1:(n-1)) 	
	{
		gain <- infogain(ds[[n]], ds[[i]])
		gains[i] <- gain
		gainsdf[1,i] <- gain
	}
	max <- which.max(gains)	
	names(gainsdf) <- names(ds[1:(n-1)])
	result <- list(max,gainsdf)
	names(result) <- c("max","gains")
	print(max)	
	return(result)
}

