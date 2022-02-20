# F-분포: F-Distribution

x <- seq(0, 4, length=100)
F.1 <- df(x, df1=1, df2=30)
F.5 <- df(x, df1=5, df2=25)
F.25 <- df(x, df1=25, df2=5)
plot(x, F.1, lty=1, lwd=3, col="black", type="l", ylim=c(0, 1))
lines(x, F.5, lty=2,lwd=3, col="blue")
lines(x, F.25, lty=3, lwd=3, col="red")
legend('topright', lty=c(1, 2, 3), col=c("black", "blue", "red"), 
       legend=c("df = 1, 30", "df = 5, 25", "df = 25, 5"))

# 일원 분산분석: One-way ANOVA

data("InsectSprays")
str(InsectSprays)
head(InsectSprays)

table(InsectSprays$spray)
aggregate(count ~ spray, data=InsectSprays, mean)
aggregate(count ~ spray, data=InsectSprays, sd)

boxplot(count ~ spray, data = InsectSprays)

aov(count ~ spray, data = InsectSprays)

spray.aov <- aov(count ~ spray, data = InsectSprays)
summary(spray.aov)

model.tables(spray.aov, type="mean")
model.tables(spray.aov, type="effects")

## 사후 분석: Post-hoc Analysis

TukeyHSD(spray.aov)
spray.compare <- TukeyHSD(spray.aov)
str(spray.compare)
spray.compare$spray
spray.compare$spray['D-C', ]

plot(spray.compare, col="blue", las=1)

## 정규성, 등분산성 가정에 대한 검정

shapiro.test(InsectSprays$count)

qqnorm(InsectSprays$count, col="blue")
qqline(InsectSprays$count, col="red")

library(car)
leveneTest(count ~ spray, data=InsectSprays)

library(stats)
bartlett.test(count ~ spray, data=InsectSprays)

## 등분산의 가정을 만족하지 못할 때

oneway.test(count ~ spray, data = InsectSprays)
oneway.test(count ~ spray, data = InsectSprays, var.equal = T)

# 이원 분산분석: Two-way ANOVA

data("ToothGrowth")
str(ToothGrowth)
head(ToothGrowth)
table(ToothGrowth$dose)

df <- ToothGrowth
df$dose <- factor(df$dose, 
                  levels=c(0.5, 1.0, 2.0),
                  labels=c("low", "med", "high"))
str(df)
head(df)
table(df$dose)
table(df$dose, df$supp)

aggregate(len ~ supp + dose, data=df, mean)
aggregate(len ~ supp + dose, data=df, sd)

aov(len ~ supp + dose + supp:dose, data = df)
aov(len ~ supp * dose, data = df)

tooth.aov <- aov(len ~ supp * dose, data = df)
summary(tooth.aov)

model.tables(tooth.aov, type="means")
boxplot(len ~ supp * dose, data=df)

library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "), data = df, 
          connect = list(c(1, 3, 5), c(2, 4, 6)),
          col = c("red", "magenta"))

TukeyHSD(tooth.aov)
TukeyHSD(tooth.aov, which="dose", conf.level = 0.99)
