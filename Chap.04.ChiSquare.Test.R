# 카이스퀘어 분포: Chi-Squared Distribution

x <- seq(0, 20, length=100)
chi.1 <- dchisq(x, df=1)
chi.5 <- dchisq(x, df=5)
chi.10 <- dchisq(x, df=10)
plot(x, chi.1, lty=1, lwd=3, col="black", type="l", ylim=c(0, 0.5))
lines(x, chi.5, lty=2,lwd=3, col="blue")
lines(x, chi.10, lty=3, lwd=3, col="red")
legend('topright', lty=c(1, 2, 3), col=c("black", "blue", "red"), 
       legend=c("df = 1", "df = 5", "df = 10"))

# 독립성 검정: Independence Test

library(MASS)
data(survey)
str(survey)
head(survey)
head(survey[, c("Sex", "Fold")])
table(survey$Sex, survey$Fold)
mosaicplot(~ Sex + Fold, data = survey, color=T)

chisq.test(survey$Fold, survey$Sex)

t <- table(survey$Sex, survey$Fold)
chisq.test(t)

# 적합성 검정: Goodness-of-Fit Test

freq <- c(60, 55, 35)
chisq.test(freq)

freq <- c(60, 55, 35)
null.p <- c(0.45, 0.30, 0.25)
chisq.test(freq, p = null.p)

null.p <- c(45, 25, 15)/85
chisq.test(freq, p = null.p)

data("HairEyeColor")
str(HairEyeColor)
HairEyeColor

hairs <- margin.table(HairEyeColor, margin = 1)
hairs

null.p = c(0.25, 0.50, 0.10, 0.15)
chisq.test(hairs, p=null.p)

library(MASS)
data(survey)
str(survey)
head(survey)
table(survey$Smoke)
smokers <- table(survey$Smoke)
barplot(smokers)

null.p <- c(0.1, 0.7, 0.1, 0.1)
chisq.test(smokers, null.p)
