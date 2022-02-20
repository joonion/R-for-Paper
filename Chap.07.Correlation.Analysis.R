# 상관 계수: Correlation Coefficient

library(MASS)
data(cats)
str(cats)
head(cats)

plot(cats$Hwt ~ cats$Bwt)
plot(cats$Hwt ~ cats$Bwt, 
     main="Body Weight and Heart Weight of Cats",
     xlab="Body Weight(kg)", ylab="HeartWeight(g)",
     col="forestgreen", pch=19)

cor(cats$Bwt, cats$Hwt)
with(cats, cor(Bwt, Hwt))

# 상관 분석: Correlation Analysis

cor.test(cats$Bwt, cats$Hwt)
cor.test(~ Bwt + Hwt, data = cats)

cor.test(~ Bwt + Hwt, data=cats, 
         alternative="greater", conf.level=0.99)

cor.test(~ Bwt + Hwt, data=cats, subset = (Sex=="F"))

data(iris)
str(iris)
head(iris)
df <- iris[, -5]
cor(df)
mat <- cor(df)
str(mat)
mat["Petal.Width", "Petal.Length"]

## 세 개 이상의 변수 간의 상관계수: 유의성 검정

library(psych)
df <- iris[, -5]
corr.test(df)

pairs.panels(df)
pairs.panels(df, bg="red", pch=21, hist.col="gold",
             main="Correlation Plot of IRIS")

library(corrgram)
corrgram(df)
corrgram(df, order=T, 
         lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel=panel.txt,
         main="Corrgram of IRIS")

cols <- colorRampPalette(c("darkgoldenrod4",
                           "burlywood1",
                           "darkkhaki",
                           "darkgreen"))

corrgram(df, order=F, 
         col.regions=cols,
         lower.panel=panel.pie,
         upper.panel=panel.conf,
         text.panel=panel.txt,
         main="Corrgram of IRIS")

# 편상관관계 분석: Partial Correlation Analysis

data(mtcars)
df <- mtcars[, c("mpg", "cyl", "hp", "wt")]
cor(df)

library(ggm)
pcor(c("mpg", "hp", "cyl", "wt"), cov(df))
pcor(c(1, 3, 2, 4), cov(df))

coef <- pcor(c(1, 3, 2, 4), cov(df))
pcor.test(coef, q=2, n=nrow(df))

library(ppcor)
pcor(df)
pcor.test(df$mpg, df$hp, df[, c("cyl", "wt")])

