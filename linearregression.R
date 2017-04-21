#LINEAR REGRESSION

#Import data as CSV
dataframename <- read.csv(file.choose())

#Run regression - nearly same as ANOVA!
model1 <- lm (y ~x, data = dframe1)

#REMEMBER - might need to TRANSFORM data first
#WANT HIGH coefficient AND HIGH r^2 value
#Check residuals are normally distributed as well (below) - if not, transform

#Then look at data - see sheet on how to interpret
#If q is 'Is regression significant' look at F-value - should be p < 0.05
model1
summary(model1)

#Now need to test assumptions - homogeniety of variables and normality of data
#First, obtain standardised residuals from the lm, titled model1 - 
sresid <- (model1$residuals - mean(model1$residuals))/sd(model1$residuals)
#Then test normaility using all below:
hist(sresid)
qqnorm(sresid)
qqline(sresid)
#You want shapiro-wilk to have a LARGE p-value as it assuming they are NOT 
#normal - want to prove it wrong and have p-value > 0.05
shapiro.test (sresid)

#To look at if data looks like it is on a straight line, can just plot:
plot (y ~ x, data = dframe1)  
abline(lm(y ~ x, data=dframe1))  

#To test if any outliers are unduly influential, use Cook's Distance Values
cook = cooks.distance(model1)
#Grapically look to see if one might be 
plot(cook,ylab="Cooks distances") 
#Then get the values for each variable. Looking for a value above CRITICAL 
#VALUE - this is measured by N (number of variables) / 4 - so for 9 variables,
#the critical value would be 9/4 = 0.44
cook
