#ONE-WAY ANOVA 

#Import data as CSV
dataframename <- read.csv(file.choose())

#If needed change x to factor type (eg if type is numbers)
anova$type <- as.factor(anova$type)

#Run ANOVA - different to One-Way!
#model1 <- aov(y ~ x + Error(r/x), data = dframe1) where:
#y = y variable, x = x variable
#Error(r/x) r = random, usually individual or subject, x = x variable
#eg Error(subject / dose)
model1 <- aov(value ~ dose + Error(subject/dose), data = weed)
model1

#NOT summary.aov!
summary(model1)

#Now need to test assumptions - homogeniety of variables and normality of data
#First, obtain standardised residuals from ANOVA titled model1 - 
residuals<-model1$"r:x"$residuals
residuals
sresid <- (residuals - mean(residuals)) / sd(residuals)

#Check the homogeneity of the standardised residuals using bartlett.
#USe on RAW DATA not normality of standard residuals during repeated measures
#Again want LARGE p-value as proving it wrong that variances ARE NOT 
#homeogenous
#bartlett.test(dframe1$y ~ dframe1$x)
bartlett.test(dframe1$y ~ dframe1x)

#Visually assess if residuals are homogenous
fitt<-model1$"r:x"$fitted.values
plot(sresid ~ fitt)

#If need to compare levels (eg Diet A to Diet B to Diet C) cannot use Tukey test
#Use pairwise T-Test
with(dframe1, pairwise.t.test(y, x,p.adjust.method="holm",
                              paired=T))
