##
##Original box cox transformation
    ##- Perform the original box cox transformation
    ##When lambda is 0 -- returns log(lambda)
    ##When lambda is not zero returns (x^lambda -1)/lambda



##Inverse Box Cox transformation
    ##When given a transformed dataset, returns the original pre box cox dataset
    ##When lambda is 0 again returns exp(x)
    ##Otherwise returns (x+lambda+1)^(1/lambda)




## Lamba estimation for boxcox
    ##This will be a method.
    ## Idea is to estimate the optimal lambda through applying
    ## A shapiro wilk normality test to the transformed data and returning the optimal value



##Box cox visualisation
    ##Compare a plot of the original data with the transformed data


##Box cox diagnostic plots
    ##Compare diagnostic plots of the original data with the transformed data


##Simulation of diagnostic info
    ##This will simulate the effects of transformations over a range of lambda values
    ##Will show (Lambda value, Skewness of transformation, Kurtosis of transformation(Tailedness))

##Summary of the transformation
  ### Skewness, Kurtosis, Shapiro wilks p-value
  ##Potentially compare these values for original data and transformed data
