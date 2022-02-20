# Event and Independent Trial

event <- factor(c('H', 'T'))
event 

trial <- sample(x = event, size = 10, replace = T)
trial

table(trial)
prop.table(table(trial))

trial <- sample(x = event, size = 100000, replace = T)
prop.table(table(trial))

event <- 1:6
trial <- sample(event, 10, replace = T)
trial
prop.table(table(trial))

trial <- sample(event, 100000, replace = T)
prop.table(table(trial))

# Binomial Distribution

?dbinom
dbinom(x=5, size=10, prob=0.5)

p <- dbinom(0:10, 10, 0.5)
p

plot(0:10, p)
plot(0:10, p, type="l", col="red")

p <- dbinom(0:1000, 1000, 0.5)
plot(0:1000, p, type="l", col="red",
     main = "PDF of Binomial Distribution",
     xlim=c(400, 600), ylim=c(0, 0.03))

sum(p)

# Normal(Gaussian) Distribution

dnorm(x=0, mean=0, sd=1)
x <- seq(-3, 3, length=100)
p <- dnorm(x, mean=0, sd=1)

plot(x, p, type="l", col="red")
plot(x, p, type="l", col="red",
     main = "PDF of Normal Distribution",
     xlim=c(-3, 3), ylim=c(0, 0.5))

pnorm(q=0, 0, 1)
pnorm(q=-1, 0, 1)
pnorm(q=1, 0, 1)
1 - pnorm(q=1, 0, 1)
pnorm(q=1, 0, 1, lower.tail=F)
pnorm(2, 0, 1, lower.tail=F)
pnorm(1.96, 0, 1, lower.tail=F)

qnorm(p=0.5, 0, 1)
qnorm(p=0.15, 0, 1)
qnorm(p=0.15, 0, 1, lower.tail=F)
qnorm(p=0.025, 0, 1)
qnorm(p=0.025, 0, 1, lower.tail=F)
qnorm(p=0.005, 0, 1)
qnorm(p=0.005, 0, 1, lower.tail=F)

random <- rnorm(n = 10000, 0, 1)
hist(random, breaks=100, freq = F)
x <- seq(-4, 4, length=200)
curve(dnorm(x), from = -4, to = 4, 
      add = T, col="red", lwd=2, lty=2)

random <- rnorm(n = 10000, mean=50, sd=20)
hist(random, breaks=100, freq = F,
     xlim=c(0, 100), ylim=c(0, 0.025),
     main="PDF of Normal Distribution")
x <- seq(0, 100, length=200)
curve(dnorm(x, 50, 20), from = 0, to = 100, 
      add = T, col="red", lwd=2, lty=2)

