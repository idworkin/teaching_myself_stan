# This is my first trial using Rstan.
# To do it I am building the simplest regression using simulated data.
 
x <- rnorm(100,0,1)
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
    real alpha; 
    real beta;
    real <lower=0> sigma;
  }
  

  model {
   y ~ normal(alpha + beta * x, sigma); //normal regression model - note I am using Rstan vectorized notation.
  }
'

# Have to provide observed data as a list
model_dat <- list(N=length(x),
    y=y,
    x=x)

# Now we fit the model in Rstan (which sends it to stan, then gets compiled in cpp)
fit <- stan(model_code = reg_code, data = model_dat,iter = 1000, chains = 4)    

# once the model has compiled in cpp we can re-run it pretty quickly with far more iterations and data.

fit2 <- stan(fit = fit, data = model_dat, iter = 10000, chains = 4)
print(fit2, digits=3)
plot(fit2)

# compare this with the results from lm()
print(lm(y~x))
summary(lm(y~x))