#ONE-WAY ANOVA 

#Import data as CSV
dataframename <- read.csv(file.choose())

#If needed change x to factor type (eg if type is numbers)
anova$type <- as.factor(anova$type)
#Run OW ANOVA - note it is y ~ x variable!
model1 <- aov(value ~ type, data = anova)
#See results
model1
summary.aov(model1)

#Now need to test assumptions - homogeniety of variables and normality of data
#First, obtain standardised residuals from ANOVA titled model1 - 
sresid <- (model1$residuals - mean(model1$residuals))/sd(model1$residuals)
#Then test normaility using all below:
hist(sresid)
qqnorm(sresid)
qqline(sresid)
#You want shapiro-wilk to have a LARGE p-value as it assuming they are NOT 
#normal - want to prove it wrong and have p-value > 0.05
shapiro.test (sresid)
#And check the homogeneity of the standardised residuals using: 
#Again want LARGE p-value as proving it wrong that variances ARE NOT 
#homeogenous
#bartlett.test(sresid ~ dataframename$xvariablename)
bartlett.test(sresid ~ anova$type)

#If need to compare levels (eg Diet A to Diet B to Diet C) use Tukey test.
TukeyHSD(aov(model1))

