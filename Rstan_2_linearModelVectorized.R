# Here is a second example, with the same model, just demonstrating additional vectorization
# I think it is faster to vectorize this as well. Also see notes about matrices and arrays in stan manual

# The only change is going from specifying scalar variables for the parameters real alpha; real beta; to vector[2] beta. In this case beta[1] will be the intercept, beta[2] will be the slope.
# To do it I am building the simplest regression using simulated data.

require(rstan) 

x <- rnorm(100, 0, 1)
y <- rnorm(100, 4 + x*0.75, 4)

print(lm(y~x))

# Testing this in Rstan.

# Since RStan takes the code and converts it to cpp, we write out the "code" part of it in '' to get piped to stan for compiling to cpp.

reg_code <- '
  data {
    int<lower=0> N; // number of observations 
    vector[N] x; // predictor variable
    vector[N] y; // response variable
    
  }
  parameters { 
    vector[2] beta; // insteatd of alpha and beta scalars (real) just use a vector for beta[1] and beta[2]
    real <lower=0> sigma;
  }
  

  model {
   y ~ normal(beta[1] + beta[2] * x, sigma); //normal regression model - note I am using Rstan vectorized notation.
  }
'

# Have to provide observed data as a list
model_dat <- list(N=length(x),
    y=y,
    x=x)

# Now we fit the model in Rstan (which sends it to stan, then gets compiled in cpp)
fit <- stan(model_code = reg_code, data = model_dat,iter = 1000, chains = 4)    

# once the model has compiled in cpp we can re-run it pretty quickly with far more iterations and data.

fit2 <- stan(fit = fit, data = model_dat, iter = 20000, chains = 4)
print(fit2, digits=3)
plot(fit2)

# compare this with the results from lm()
print(lm(y~x))
summary(lm(y~x))
confint(lm(y~x))