# generate some fake data
n <- 100
x <- runif(n=n,min=0,max=100)
y <- 3*x + 5
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
