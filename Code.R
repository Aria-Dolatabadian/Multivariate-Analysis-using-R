#https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html

library("car")
library(RColorBrewer)
library("MASS") 

#wine.data contains data on concentrations of 13 different chemicals in wines grown in the same region in Italy that are derived from three different cultivars.

wine <- read.table("wine.data",
          sep=",")

wine



#A Matrix Scatterplot

wine[2:6]

scatterplotMatrix(wine[2:6])


#A Scatterplot with the Data Points Labelled by their Group

plot(wine$V4, wine$V5)

#If we want to label the data points by their group (the cultivar of wine here), we can use the “text” function in R to plot some text beside every data point. 
#In this case, the cultivar of wine is stored in the column V1 of the variable “wine”, so we type:

text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")

#A Profile Plot
#Another type of plot that is useful is a “profile plot”, which shows the variation in each of the variables, by plotting the value of each of the variables for each of the samples.

#For example, to make a profile plot of the concentrations of the first five chemicals in the wine samples (stored in columns V2, V3, V4, V5, V6 of variable “wine”), we type:

names <- c("V2","V3","V4","V5","V6")
mylist <- list(wine$V2,wine$V3,wine$V4,wine$V5,wine$V6)
makeProfilePlot(mylist,names)

#It is clear from the profile plot that the mean and standard deviation for V6 is quite a lot higher than that for the other variables.








#Calculating Summary Statistics for Multivariate Data
#Another thing that you are likely to want to do is to calculate summary statistics such as the mean and standard deviation for each of the variables in your multivariate data set.
#This is easy to do, using the “mean()” and “sd()” functions in R. 
#For example, say we want to calculate the mean and standard deviations of each of the 13 chemical concentrations in the wine samples. 
#These are stored in columns 2-14 of the variable “wine”. So we type:

sapply(wine[2:14],mean)

#This tells us that the mean of variable V2 is 13.0006180, the mean of V3 is 2.3363483, and so on.
#Similarly, to get the standard deviations of the 13 chemical concentrations, we type:

sapply(wine[2:14],sd)

#Means and Variances Per Group
#It is often interesting to calculate the means and standard deviations for just the samples from a particular group, for example, for the wine samples from each cultivar. 
#The cultivar is stored in the column “V1” of the variable “wine”.
#To extract out the data for just cultivar 2, we can type:

cultivar2wine <- wine[wine$V1=="2",]

#We can then calculate the mean and standard deviations of the 13 chemicals’ concentrations, for just the cultivar 2 samples:

sapply(cultivar2wine[2:14],mean)
sapply(cultivar2wine[2:14], sd)

#You can calculate the mean and standard deviation of the 13 chemicals’ concentrations for just cultivar 1 samples, or for just cultivar 3 samples, in a similar way.

printMeanAndSdByGroup <- function(variables,groupvariable)
  {
     # find the names of the variables
     variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
     # within each group, find the mean of each variable
     groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
     means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
     names(means) <- variablenames
     print(paste("Means:"))
     print(means)
     # within each group, find the standard deviation of each variable:
     sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
     names(sds) <- variablenames
     print(paste("Standard deviations:"))
     print(sds)
     # within each group, find the number of samples:
     samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
     names(samplesizes) <- variablenames
     print(paste("Sample sizes:"))
     print(samplesizes)
  }

printMeanAndSdByGroup(wine[2:14],wine[1])


#The function “printMeanAndSdByGroup()” also prints out the number of samples in each group. In this case, we see that there are 59 samples of cultivar 1, 71 of cultivar 2, and 48 of cultivar 3.


#Between-groups Variance and Within-groups Variance for a Variable

calcWithinGroupsVariance <- function(variable,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # get the mean and standard deviation for each group:
     numtotal <- 0
     denomtotal <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata <- variable[groupvariable==leveli,]
        levelilength <- length(levelidata)
        # get the standard deviation for group i:
        sdi <- sd(levelidata)
        numi <- (levelilength - 1)*(sdi * sdi)
        denomi <- levelilength
        numtotal <- numtotal + numi
        denomtotal <- denomtotal + denomi
     }
     # calculate the within-groups variance
     Vw <- numtotal / (denomtotal - numlevels)
     return(Vw)
  }

#You will need to copy and paste this function into R before you can use it. For example, to calculate the within-groups variance of the variable V2 (the concentration of the first chemical), we type:


calcWithinGroupsVariance(wine[2],wine[1])

#Thus, the within-groups variance for V2 is 0.2620525.
#We can calculate the between-groups variance for a particular variable (eg. V2) using the function “calcBetweenGroupsVariance()” below:

calcBetweenGroupsVariance <- function(variable,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # calculate the overall grand mean:
     grandmean <- mean(variable)
     # get the mean and standard deviation for each group:
     numtotal <- 0
     denomtotal <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata <- variable[groupvariable==leveli,]
        levelilength <- length(levelidata)
        # get the mean and standard deviation for group i:
        meani <- mean(levelidata)
        sdi <- sd(levelidata)
        numi <- levelilength * ((meani - grandmean)^2)
        denomi <- levelilength
        numtotal <- numtotal + numi
        denomtotal <- denomtotal + denomi
     }
     # calculate the between-groups variance
     Vb <- numtotal / (numlevels - 1)
     Vb <- Vb[[1]]
     return(Vb)
  }

#Once you have copied and pasted this function into R, you can use it to calculate the between-groups variance for a variable such as V2:


calcBetweenGroupsVariance (wine[2],wine[1])   # Returns NA and Warning


Thus, the between-groups variance of V2 is 35.39742.

#We can calculate the “separation” achieved by a variable as its between-groups variance devided by its within-groups variance. 
#Thus, the separation achieved by V2 is calculated as:

35.39742/0.2620525

#If you want to calculate the separations achieved by all of the variables in a multivariate data set, you can use the function “calcSeparations()” below:


calcSeparations <- function(variables,groupvariable)
  {
     # find out how many variables we have
     variables <- as.data.frame(variables)
     numvariables <- length(variables)
     # find the variable names
     variablenames <- colnames(variables)
     # calculate the separation for each variable
     for (i in 1:numvariables)
     {
        variablei <- variables[i]
        variablename <- variablenames[i]
        Vw <- calcWithinGroupsVariance(variablei, groupvariable)
        Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
        sep <- Vb/Vw
        print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
     }
  }

#For example, to calculate the separations for each of the 13 chemical concentrations, we type:

calcSeparations(wine[2:14],wine[1])


#Between-groups Covariance and Within-groups Covariance for Two Variables

calcWithinGroupsCovariance <- function(variable1,variable2,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # get the covariance of variable 1 and variable 2 for each group:
     Covw <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata1 <- variable1[groupvariable==leveli,]
        levelidata2 <- variable2[groupvariable==leveli,]
        mean1 <- mean(levelidata1)
        mean2 <- mean(levelidata2)
        levelilength <- length(levelidata1)
        # get the covariance for this group:
        term1 <- 0
        for (j in 1:levelilength)
        {
           term1 <- term1 + ((levelidata1[j] - mean1)*(levelidata2[j] - mean2))
        }
        Cov_groupi <- term1 # covariance for this group
        Covw <- Covw + Cov_groupi
     }
     totallength <- nrow(variable1)
     Covw <- Covw / (totallength - numlevels)
     return(Covw)
  }


#For example, to calculate the within-groups covariance for variables V8 and V11, we type:


calcWithinGroupsCovariance(wine[8],wine[11],wine[1])

calcBetweenGroupsCovariance <- function(variable1,variable2,groupvariable)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # calculate the grand means
     variable1mean <- mean(variable1)
     variable2mean <- mean(variable2)
     # calculate the between-groups covariance
     Covb <- 0
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata1 <- variable1[groupvariable==leveli,]
        levelidata2 <- variable2[groupvariable==leveli,]
        mean1 <- mean(levelidata1)
        mean2 <- mean(levelidata2)
        levelilength <- length(levelidata1)
        term1 <- (mean1 - variable1mean)*(mean2 - variable2mean)*(levelilength)
        Covb <- Covb + term1
     }
     Covb <- Covb / (numlevels - 1)
     Covb <- Covb[[1]]
     return(Covb)
  }


#For example, to calculate the between-groups covariance for variables V8 and V11, we type:


calcBetweenGroupsCovariance(wine[8],wine[11],wine[1])  # Returns NA and Warning

#Calculating Correlations for Multivariate Data

cor.test(wine$V2, wine$V3)

mosthighlycorrelated <- function(mydataframe,numtoreport)
  {
     # find the correlations
     cormatrix <- cor(mydataframe)
     # set the correlations on the diagonal or lower triangle to zero,
     # so they will not be reported as the highest ones:
     diag(cormatrix) <- 0
     cormatrix[lower.tri(cormatrix)] <- 0
     # flatten the matrix into a dataframe for easy sorting
     fm <- as.data.frame(as.table(cormatrix))
     # assign human-friendly names
     names(fm) <- c("First.Variable", "Second.Variable","Correlation")
     # sort and print the top n correlations
     head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
  }

mosthighlycorrelated(wine[2:14], 10)


#Standardising Variables

standardisedconcentrations <- as.data.frame(scale(wine[2:14]))

sapply(standardisedconcentrations,mean)

sapply(standardisedconcentrations,sd)

#Principal Component Analysis

standardisedconcentrations <- as.data.frame(scale(wine[2:14])) # standardise the variables

wine.pca <- prcomp(standardisedconcentrations)                 # do a PCA

summary(wine.pca)


#This gives us the standard deviation of each component, and the proportion of variance explained by each component. 
#The standard deviation of the components is stored in a named element called “sdev” of the output variable made by “prcomp”:

wine.pca$sdev

#The total variance explained by the components is the sum of the variances of the components:

sum((wine.pca$sdev)^2)

#Deciding How Many Principal Components to Retain

screeplot(wine.pca, type="lines")

(wine.pca$sdev)^2

#Loadings for the Principal Components

wine.pca$rotation[,1]

sum((wine.pca$rotation[,1])^2)

calcpc <- function(variables,loadings)
  {
     # find the number of samples in the data set
     as.data.frame(variables)
     numsamples <- nrow(variables)
     # make a vector to store the component
     pc <- numeric(numsamples)
     # find the number of variables
     numvariables <- length(variables)
     # calculate the value of the component for each sample
     for (i in 1:numsamples)
     {
        valuei <- 0
        for (j in 1:numvariables)
        {
           valueij <- variables[i,j]
           loadingj <- loadings[j]
           valuei <- valuei + (valueij * loadingj)
        }
        pc[i] <- valuei
     }
     return(pc)
  }

#We can then use the function to calculate the values of the first principal component for each sample in our wine data:


calcpc(standardisedconcentrations, wine.pca$rotation[,1])

#In fact, the values of the first principal component are stored in the variable wine.pca$x[,1] that was returned by the “prcomp()” function, so we can compare those values to the ones that we calculated, and they should agree:


wine.pca$x[,1]

wine.pca$rotation[,2]

sum((wine.pca$rotation[,2])^2)

#Scatterplots of the Principal Components

plot(wine.pca$x[,1],wine.pca$x[,2]) # make a scatterplot
text(wine.pca$x[,1],wine.pca$x[,2], wine$V1, cex=0.7, pos=4, col="red") # add labels


printMeanAndSdByGroup(standardisedconcentrations,wine[1])


#Linear Discriminant Analysis

#For example, to carry out a linear discriminant analysis using the 13 chemical concentrations in the wine samples, we type:

wine.lda <- lda(wine$V1 ~ wine$V2 + wine$V3 + wine$V4 + wine$V5 + wine$V6 + wine$V7 +
                            wine$V8 + wine$V9 + wine$V10 + wine$V11 + wine$V12 + wine$V13 +
                            wine$V14)


wine.lda


wine.lda$scaling[,1]

calclda <- function(variables,loadings)
  {
     # find the number of samples in the data set
     as.data.frame(variables)
     numsamples <- nrow(variables)
     # make a vector to store the discriminant function
     ld <- numeric(numsamples)
     # find the number of variables
     numvariables <- length(variables)
     # calculate the value of the discriminant function for each sample
     for (i in 1:numsamples)
     {
        valuei <- 0
        for (j in 1:numvariables)
        {
           valueij <- variables[i,j]
           loadingj <- loadings[j]
           valuei <- valuei + (valueij * loadingj)
        }
        ld[i] <- valuei
     }
     # standardise the discriminant function so that its mean value is 0:
     ld <- as.data.frame(scale(ld, center=TRUE, scale=FALSE))
     ld <- ld[[1]]
     return(ld)
  }


calclda(wine[2:14], wine.lda$scaling[,1])

#In fact, the values of the first linear discriminant function can be calculated using the “predict()” function in R, so we can compare those to the ones that we calculated, and they should agree:



wine.lda.values <- predict(wine.lda, wine[2:14])
wine.lda.values$x[,1] # contains the values for the first discriminant function



groupStandardise <- function(variables, groupvariable)
  {
     # find out how many variables we have
     variables <- as.data.frame(variables)
     numvariables <- length(variables)
     # find the variable names
     variablenames <- colnames(variables)
     # calculate the group-standardised version of each variable
     for (i in 1:numvariables)
     {
        variablei <- variables[i]
        variablei_name <- variablenames[i]
        variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
        variablei_mean <- mean(variablei)
        variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
        data_length <- nrow(variablei)
        if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
        variables_new[`variablei_name`] <- variablei_new
     }
     return(variables_new)
  }


#For example, we can use the “groupStandardise()” function to calculate the group-standardised versions of the chemical concentrations in wine samples:

groupstandardisedconcentrations <- groupStandardise(wine[2:14], wine[1])  #There were 13 warnings (use warnings() to see them)


wine.lda2 <- lda(wine$V1 ~ groupstandardisedconcentrations$V2 + groupstandardisedconcentrations$V3 +
                             groupstandardisedconcentrations$V4 + groupstandardisedconcentrations$V5 +
                             groupstandardisedconcentrations$V6 + groupstandardisedconcentrations$V7 +
                             groupstandardisedconcentrations$V8 + groupstandardisedconcentrations$V9 +
                             groupstandardisedconcentrations$V10 + groupstandardisedconcentrations$V11 +
                             groupstandardisedconcentrations$V12 + groupstandardisedconcentrations$V13 +
                             groupstandardisedconcentrations$V14)

#Error in x - group.means[g, ] : non-conformable arrays



wine.lda.values <- predict(wine.lda, wine[2:14])
wine.lda.values$x[,1] # values for the first discriminant function, using the unstandardised data


wine.lda.values2 <- predict(wine.lda2, groupstandardisedconcentrations)
wine.lda.values2$x[,1] # values for the first discriminant function, using the standardised data #Error


#Separation Achieved by the Discriminant Functions

wine.lda.values <- predict(wine.lda, standardisedconcentrations)
calcSeparations(wine.lda.values$x,wine[1])

wine.lda

(wine.lda$svd)^2

#A Stacked Histogram of the LDA Values

ldahist(data = wine.lda.values$x[,1], g=wine$V1)


ldahist(data = wine.lda.values$x[,2], g=wine$V1)


#Scatterplots of the Discriminant Functions

plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],wine$V1,cex=0.7,pos=4,col="red") # add labels

#Allocation Rules and Misclassification Rate

printMeanAndSdByGroup(wine.lda.values$x,wine[1])

calcAllocationRuleAccuracy <- function(ldavalue, groupvariable, cutoffpoints)
  {
     # find out how many values the group variable can take
     groupvariable2 <- as.factor(groupvariable[[1]])
     levels <- levels(groupvariable2)
     numlevels <- length(levels)
     # calculate the number of true positives and false negatives for each group
     numlevels <- length(levels)
     for (i in 1:numlevels)
     {
        leveli <- levels[i]
        levelidata <- ldavalue[groupvariable==leveli]
        # see how many of the samples from this group are classified in each group
        for (j in 1:numlevels)
        {
           levelj <- levels[j]
           if (j == 1)
           {
              cutoff1 <- cutoffpoints[1]
              cutoff2 <- "NA"
              results <- summary(levelidata <= cutoff1)
           }
           else if (j == numlevels)
           {
              cutoff1 <- cutoffpoints[(numlevels-1)]
              cutoff2 <- "NA"
              results <- summary(levelidata > cutoff1)
           }
           else
           {
              cutoff1 <- cutoffpoints[(j-1)]
              cutoff2 <- cutoffpoints[(j)]
              results <- summary(levelidata > cutoff1 & levelidata <= cutoff2)
           }
           trues <- results["TRUE"]
           trues <- trues[[1]]
           print(paste("Number of samples of group",leveli,"classified as group",levelj," : ",
              trues,"(cutoffs:",cutoff1,",",cutoff2,")"))
        }
     }
  }


#For example, to calculate the accuracy for the wine data based on the allocation rule for the first discriminant function, we type:

calcAllocationRuleAccuracy(wine.lda.values$x[,1], wine[1], c(-1.751107, 2.122505))





