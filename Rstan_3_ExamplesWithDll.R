# Here is a second example, with the same model, just demonstrating additional vectorization
# I think it is faster to vectorize this as well. Also see notes about matrices and arrays in stan manual

# The only change is going from specifying scalar variables for the parameters real alpha; real beta; to vector[2] beta. In this case beta[1] will be the intercept, beta[2] will be the slope.
# To do it I am building the simplest regression using simulated data.

# setting some options for cleaner output (number of digits printed)
options(digits=3)

# Little function for a nicely printer lm output of coefficients
niceLM <- function(mod) {
	return(cbind(summary(mod)$coef[,1:3], confint(mod)))
}

#Libraries
require(Rcpp) 
require(rstan)
require(lme4)

### Data in
setwd("/Users/ian/R/R scripts/Dll data/") 
dll.data = read.csv("dll.txt", header=TRUE)   #data frame input
##dll.data = read.csv("http://datadryad.org/bitstream/handle/10255/dryad.8377/dll.csv", header=TRUE)

# For purposes of these tests I am going to use a cleaned subset of the data.
dll.data <- na.omit(dll.data)
dll.data$genotype <- relevel(dll.data$genotype, "wt")

# Simple regression of SCT on tarsus
mod1 <- lm(SCT ~ tarsus, data=dll.data)
niceLM(mod1)

# Testing this in Rstan.

# Since RStan takes the code and converts it to cpp, we write out the "code" part of it in '' to get piped to stan for compiling to cpp.

reg_code <- '
  data {
    int<lower=0> N; // number of observations 
    vector[N] tarsus; // predictor variable
    vector<lower=0>[N] SCT; // response variable
    
  }
  
  parameters { 
    vector[2] beta; // instead of alpha and beta scalars (real) just use a vector for beta[1] and beta[2]
    real <lower=0> sigma;
  }
  

  model {
   SCT ~ normal(beta[1] + beta[2] * tarsus, sigma); //normal regression model - note I am using Rstan vectorized notation.
  }
'

# Have to provide observed data as a list
model_dat <- list(N=length(dll.data$SCT),
    SCT=dll.data$SCT,
    tarsus=dll.data$tarsus)

# Now we fit the model in Rstan (which sends it to stan, then gets compiled in cpp)
fit <- stan(model_code = reg_code, data = model_dat,iter = 1000, chains = 1)    

# once the model has compiled in cpp we can re-run it pretty quickly with far more iterations and data.

fit2 <- stan(fit = fit, data = model_dat, iter = 20000, chains = 1)
print(fit2, digits=3)
plot(fit2)

# Keeps spitting out warnings about mis-specification. Obviously I am not incorporating most of the appropriate covariates yet, but this could be due to several issues.
# 1 - No priors on beta[1] and beta[2]. Try adding these
# 2 - intercept is way outside of range of data. Center this.
# 3 response is actually count data.

# First I am going to try just adding priors on beta[1] and beta[2]

reg_code_2 <- '
  data {
    int<lower=0> N; // number of observations 
    vector[N] tarsus; // predictor variable
    vector<lower=0>[N] SCT; // response variable
    
  }
  
  parameters { 
    vector[2] beta; // instead of alpha and beta scalars (real) just use a vector for beta[1] and beta[2]
    real <lower=0, upper=100> sigma; // addition upper constraint on sigma.
  }
  

  model {
   beta[1] ~ normal(0, 100); // prior on the intercept
   beta[2] ~ normal(0, 100); // prior on the slope
   SCT ~ normal(beta[1] + beta[2] * tarsus, sigma); //normal regression model - note I am using Rstan vectorized notation.
  }
'
fit_v2 <- stan(model_code = reg_code_2, data = model_dat,iter = 1000, chains = 1)    

fit2_v2 <- stan(fit = fit_v2, data = model_dat, iter = 10000, chains = 1)
print(fit2_v2, digits=3)
plot(fit2_v2)


### Now I am going to center tarsus length
### See https://github.com/stan-dev/stan/blob/feature/ARM/src/models/ARM/Ch.4/kidiq_interaction_c.stan

mod1c <- lm(SCT ~ scale(tarsus,scale=FALSE), data=dll.data)
niceLM(mod1c)
summary(mod1c)

reg_code_2c <- '
  data {
    int<lower=0> N; // number of observations 
    vector[N] tarsus; // predictor variable
    vector<lower=0>[N] SCT; // response variable
    
  }
  
  transformed data {
  	vector[N] c_tarsus; // centered predictor
  	c_tarsus <- tarsus - mean(tarsus);
  }
  
  parameters { 
    vector[2] beta; // instead of alpha and beta scalars (real) just use a vector for beta[1] and beta[2]
    real <lower=0, upper=100> sigma; // addition upper constraint on sigma.
  }
  

  model {
   beta[1] ~ normal(0, 100); // prior on the intercept
   beta[2] ~ normal(0, 100); // prior on the slope
   SCT ~ normal(beta[1] + beta[2] * c_tarsus, sigma); //normal regression model 
  }
'

fit_v2c <- stan(model_code = reg_code_2c, data = model_dat,iter = 1000, chains = 1)    

fit2_v2c <- stan(fit = fit_v2c, data = model_dat, iter = 10000, chains = 4)
# Faster, and no error messages
print(fit2_v2c)
plot(fit2_v2c)

## Robust regression. 
# We can easily use other distributions in the mix, like a student t-distribution for a robust regression

reg_code_2t <- '
  data {
    int<lower=0> N; // number of observations 
    vector[N] tarsus; // predictor variable
    vector<lower=0>[N] SCT; // response variable
    int<lower=0> nu; //degrees of freedom
     
  }
  
  transformed data {
  	vector[N] c_tarsus; // centered predictor
  	c_tarsus <- tarsus - mean(tarsus);
  }
  
  parameters { 
    vector[2] beta; // instead of alpha and beta scalars (real) just use a vector for beta[1] and beta[2]
    real<lower=0, upper=100> sigma; // addition upper constraint on sigma.
  }
  

  model {
   beta[1] ~ normal(0, 100); // prior on the intercept
   beta[2] ~ normal(0, 100); // prior on the slope
   SCT ~ student_t(nu, beta[1] + beta[2] * c_tarsus, sigma); //normal regression model 
  }
'


model_dat2 <- list(N=length(dll.data$SCT),
    SCT=dll.data$SCT,
    tarsus=dll.data$tarsus,
    nu = (length(dll.data$SCT) -1))

fit_v2t <- stan(model_code = reg_code_2t, data = model_dat2,iter = 1000, chains = 1)    

fit2_v2t <- stan(fit = fit_v2t, data = model_dat2, iter = 20000, chains = 4)
# Faster, and no error messages
print(fit2_v2t)
plot(fit2_v2t)

## Adding an additional covariate to the model (genotype)

mod2 <- lm(SCT ~ scale(tarsus,scale=FALSE) + genotype, data=dll.data)
niceLM(mod2)
summary(mod2)

stan_mod2 <- '
  data {
    int<lower=0> N; // number of observations 
    vector[N] tarsus; // predictor variable
    vector<lower=0>[N] SCT; // response variable
    vector<lower=0>[N] genotype;
  }
  
  transformed data {
  	vector[N] c_tarsus; // centered predictor
  	c_tarsus <- tarsus - mean(tarsus);
  }
  
  parameters { 
    vector[3] beta; // now including genotype
    real <lower=0, upper=100> sigma; // addition upper constraint on sigma.
  }
  

  model {
   for (p in 1:3) {
       beta[p] ~ normal(0, 100); // prior on the intercept
       }
   SCT ~ normal(beta[1] + beta[2] * c_tarsus + beta[3]*genotype, sigma); //normal regression model 
  }
'

model_dat3 <- list(N=length(dll.data$SCT),
    SCT=dll.data$SCT,
    tarsus=dll.data$tarsus,
    genotype=(as.numeric(dll.data$genotype)-1)) # Temporary workaround

stan_fit_mod2 <- stan(model_code = stan_mod2, data = model_dat3,iter = 1000, chains = 1)    

stan_fit2_mod2 <- stan(fit = stan_fit_mod2, data = model_dat3, iter = 10000, chains = 4)

print(stan_fit2_mod2)
plot(stan_fit2_mod2)
