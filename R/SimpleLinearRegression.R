# generate some fake data
n <- 100
x <- runif(n=n,min=0,max=100)
slope_true <- 3
intercept_true <- 5
y <- slope_true*x + intercept_true
y <- y+rnorm(n=n, sd=20)

# plot the data
plot(x,y, pch=16, cex=.6)

# fit a linear model
model <- lm(y~x)

# plot linear regression
lines(x,model$fitted.values, col='red')

# plot residuals
for (i in 1:length(x)) {
  lines(rep(x[i],2), c(model$fitted.values[i], y[i]), col='blue')
}

# get TSS and RSS
rss <- sum((y-model$fitted.values)^2)
tss <- sum((y-mean(y))^2)

# show that R^2 = r^2
rsq <- 1-rss/tss
sqrt(rsq)
cor(x,y)

summary(model)

# plot histogram of residuals
hist(model$residuals)
# note that the residuals are normally distributed

# repeat several times to visualize variance in fitted model
# first define a function
regression_interval_test <- function(trials=100, n=100) {
  
  # placeholders for model list, confidence intervals, and intervals that don't contain true slope
  models <- list()
  lowers <- c()
  uppers <- c()
  bad_ints <- c()
  
  # loop through trials
  for (i in 1:trials) {
    
    # create data
    x <- runif(n=n,min=0,max=100)
    y <- 3*x + 5
    y <- y+rnorm(n=n, sd=20)
    # plot(x,y, pch=16, cex=.6, main=i)
    
    # fit and plot a linear model
    models[[i]] <- lm(y~x)
    # lines(x,model$fitted.values, col='red')
    
    slope <- models[[i]]$coefficients[2]
    st_err <- summary(models[[i]])$coefficients[2, 2]
    
    lowers <- c(lowers, slope - 1.96*st_err)
    uppers <- c(uppers, slope + 1.96*st_err)
    
    if (lowers[i] > slope_true | uppers[i] < slope_true) {
      bad_ints <- c(bad_ints, i)
    }
  }
  
  # plot results
  matplot(rbind(1:trials,1:trials),t(data.frame(lowers, uppers)),type="l",lty=1,lwd=1,col=4,xlab="Sample #",ylab="Slope")
  abline(h=slope_true,lty=2,col=2,lwd=2)
  if (trials <= 100) {
    text(bad_ints,min(lowers), bad_ints, cex=.5)  
  } else {
    points(bad_ints,rep(min(lowers), length(bad_ints)), pch=16, cex=.25, col=adjustcolor("black", .5))
  }

  sapply(models, mean)
  mean(unlist(lapply(x, mean(z), z$coefficients.x)))
  return(length(bad_ints)/trials)
  
}

# run for 100 samples
set.seed(1980)
regression_interval_test(100)

# run for 1000 samples
set.seed(1990)
regression_interval_test(1000)

