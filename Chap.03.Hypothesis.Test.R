# Hypothesis Test

# Sampling from population with normality

set.seed(100)
pop <- rnorm(10000, mean=173, sd=10)
mean(pop)
sd(pop)

sam <- sample(pop, 10, replace=F)
sam
mean(sam)
sd(sam)

sam.mean <- c()
for (i in 1:1000) {
    sam.mean[i] <- mean(sample(pop, 10, replace=F))
}
hist(sam.mean, breaks=50)
mean(sam.mean)
sd(sam.mean)
10 / sqrt(10)

shapiro.test(sam.mean)

qqnorm(sam.mean, col="blue")
qqline(sam.mean, col="red", lwd=2)

# Sampling from population without normality

set.seed(100)
pop <- runif(10000, min=150, max=200)
mean(pop)
sd(pop)

sam <- sample(pop, 10, replace=F)
sam
mean(sam)
sd(sam)

sam.mean <- c()
for (i in 1:1000) {
    sam.mean[i] <- mean(sample(pop, 10, replace=F))
}
hist(sam.mean, breaks=50)
mean(sam.mean)
sd(sam.mean)
10 / sqrt(10)

shapiro.test(sam.mean)

qqnorm(sam.mean, col="blue")
qqline(sam.mean, col="red", lwd=2)

# confidence interval and p-value




