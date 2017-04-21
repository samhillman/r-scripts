#TWO-WAY ANOVA 

#Import data as CSV
dataframename <- read.csv(file.choose())

#If needed change x to factor type (eg if type is numbers)
anova$type <- as.factor(anova$type)

#Run TW ANOVA - note it is y ~ x variable!
# Two-way Anova without an interaction term
model1 <- aov(y ~ x1 + x2, data = dframe1)
# Two-way Anova with an interaction term:
model1 <- aov(y ~ x1 * x2, data = dframe1)
# or where x1:x2 is the interaction term:
model1 <- lm(y~ x1 + x2 + x1:x2, data = dframe1) 

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

#And check the homogeneity of the standardised residuals using Bartlett:
#Again want LARGE p-value. DIFFERENT TEST FOR TWO WAY - 
#For a simple Two-way Anova the Bartlett's test must be performed separately for
#each factor using the same commands as for the One-way Anova:
bartlett.test(sresid ~ dframe1$x1)
bartlett.test(sresid ~ dframe1$x2)

#For a Two-way Anova with an interaction term the homogeneity of variances of 
# the residuals must be verified in relation to the interaction and not to each
#factor individually, using a slightly different command:
bartlett.test(sresid ~ interaction(x1,x2), data=dframe1)

#Graphical visualisation of the residuals against the model fitted values is 
#still a valid way of investigating if the residuals are homogeneous:
plot(sresid ~model1$fitted.values)

