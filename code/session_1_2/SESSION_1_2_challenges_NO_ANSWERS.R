#################################################################################################### 
# challenge 1
#################################################################################################### 

x<-c(1.214, 1.480, 1.088, 1.444, 0.637, 1.123, 0.136, 1.490, 0.636, -0.294)

set.seed(99) # ensures the same psuedo-random numbers are generated

nx<-10
x<-rnorm(nx,1,1) # random normal data with mean of 1 and sd of 1
x<-round(x,3) # remove decimal places to get exactly the same data as notes
x

# TO DO: make y in the same fashion 
# HINT: do not reset the set.seed from above (otherwise a different starting point)


#################################################################################################### 
# challenge 2
#################################################################################################### 

#1.
mean(x)
var(x)
sqrt(var(x))

#2.


# or try just running the rnorm over and over from the same starting point ...#

#3.
z<-round(rnorm(10,20,1),3)
cbind(x,z)


#################################################################################################### 
# challenge 3
#################################################################################################### 

# generate data (new seed will occur each time the code is run)
x<-round(rnorm(10,1,1),3)
y<-round(rnorm(15,2,1),3)

# put two series of different length into a named list

# apply a function to each element of the list




#################################################################################################### 
# challenge 4
#################################################################################################### 
# using R studio, run the file "spiegelhater_ch7_partners.R"
# compare the charts to "speigelhalter_2019_ch7.pdf" figure 7.3, page 198 
# under the heading "bootstrap means" adjust the nsamp variable and rerun this section to gauge the effect of different sample sizes



#################################################################################################### 
# challenge 5
#################################################################################################### 

# lookup "?sample"
set.seed(99)
x<-round(rnorm(10,1,1),3)

sample(x,length(x),replace=TRUE)

# generate bootstrap samples


# calculate and plot means





#################################################################################################### 
# challenge 6
#################################################################################################### 






#################################################################################################### 
# challenge 7
#################################################################################################### 


# generate simulates samples
nsim<-1000
true_mu<-1
xsim<-lapply(1:nsim,function(i){rnorm(15,true_mu,2)})


#################################################################################################### 
# challenge 8
#################################################################################################### 

# alter the code from challenge 7



#################################################################################################### 
# challenge 9
#################################################################################################### 

# using R studio, run the file "jelly_bean_search.R"
