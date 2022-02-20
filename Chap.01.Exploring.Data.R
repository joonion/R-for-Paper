# Categorical Variable

install.packages("MASS")
library(MASS)
data(survey)
?survey

str(survey)
head(survey)

survey$Smoke
levels(survey$Smoke)

table(survey$Smoke)
tab <- table(survey$Smoke)
prop.table(tab)
prop.tab <- prop.table(tab)
round(prop.tab * 100, 1)

barplot(tab)
barplot(height=tab, 
        main="How much the student smokes",
        xlab="Smoke", ylab="Frequency",
        col="steelblue", ylim=c(0, 200))

# Continuous Variable

library(MASS)
data(survey)

str(survey)
head(survey)

survey$Age
mean(survey$Age)
median(survey$Age)
var(survey$Age)
sd(survey$Age)
quantile(survey$Age)
min(survey$Age)
max(survey$Age)

summary(survey)

survey$Pulse
mean(survey$Pulse)

mean(survey$Pulse, na.rm=T)
median(survey$Pulse, na.rm=T)
var(survey$Pulse, na.rm=T)
sd(survey$Pulse, na.rm=T)
quantile(survey$Pulse, na.rm=T)
min(survey$Pulse, na.rm=T)
max(survey$Pulse, na.rm=T)

summary(survey)

summary(survey$Age)
hist(survey$Age)
hist(x = survey$Age, 
     main="Age of the student in years",
     xlab="Age", ylab="Frequency",
     col="steelblue", 
     xlim=c(10, 80), ylim=c(0, 100),
     breaks=50)

# Descriptive Statistics by Group

library(MASS)
data(survey)

survey$Exer
tab <- table(survey$Exer)
prop.tab <- prop.table(tab)
round(prop.tab * 100, 1)

tapply(X = survey$Pulse, 
       INDEX = survey$Exer,
       FUN = mean,
       na.rm = T)

tapply(survey$Pulse, survey$Sex, mean, na.rm=T)
with(survey, tapply(Pulse, Sex, mean, na.rm=T))
aggregate(x = survey$Pulse, 
          by = list(Exercise=survey$Exer), 
          FUN = mean, 
          na.rm = T)
with(survey, aggregate(Pulse, list(Exer), mean, na.rm=T))

boxplot(survey$Pulse)
boxplot(Pulse ~ Exer, survey)
boxplot(formula = Pulse ~ Exer, data = survey, 
        main = "Box Plot of Pulse grouped by Exer",
        col=c("red", "yellow", "blue"))

