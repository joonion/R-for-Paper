# t-분포: t-Distribution

x <- seq(-5, 5, length=100)
t.1 <- dt(x, df=1)
t.10 <- dt(x, df=10)
t.100 <- dt(x, df=100)
plot(x, t.1, lty=1, lwd=3, col="black", type="l", ylim=c(0, 0.5))
lines(x, t.10, lty=2,lwd=3, col="blue")
lines(x, t.100, lty=3, lwd=3, col="red")
legend('topright', lty=c(1, 2, 3), col=c("black", "blue", "red"), 
       legend=c("df = 1", "df = 10", "df = 100"))

# 단일표본 평균검정: One Sample t-Test

library(MASS)
data(cats)
str(cats)
head(cats)

mean(cats$Bwt)
t.test(x=cats$Bwt, mu=2.7)
t.test(x=cats$Bwt, mu=2.6)
t.test(x=cats$Bwt, mu=2.6, alternative="greater")

bwt.t <- t.test(cats$Bwt, mu=2.6)
str(bwt.t)
bwt.t$p.value
bwt.t$conf.int

prop.test(x=18, n=30, p=0.5, alternative = "greater")

# 독립표본 평균검정: Two-Independent Samples t-Test

library(MASS)
str(cats)
table(cats$Sex)
prop.table(table(cats$Sex))
aggregate(Bwt ~ Sex, data = cats, mean)

t.test(formula = Bwt ~ Sex, data=cats)

bwt.t <- t.test(formula = Bwt ~ Sex, data=cats)
bwt.t$p.value
bwt.t$conf.int

# 대응표본 평균검정: Paired Samples t-Test

data(sleep)
str(sleep)
head(sleep)

table(sleep$group)
aggregate(extra ~ group, data = sleep, mean)

t.test(extra ~ group, data = sleep, paired = T)

extra.t <- t.test(extra ~ group, data = sleep, paired = T)
extra.t$p.value
extra.t$conf.int

