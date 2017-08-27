#####Packages
install.packages("lme4")
install.packages("pbkrtest")
install.packages("MuMIn")
library(lme4)
library(tidyverse)
library(pbkrtest)
library(MuMIn)
library(LMERConvenienceFunctions)

#####Input Data
glm.tempdata <- read.csv("cleaned data\\growthrate.tempdata.csv")
head(glm.tempdata)

#####Modify Data
#add data - actual growth and then growth rate
#clean data - filter out where edge has been hit and the contaminated
glm.tempdata <- mutate(glm.tempdata, actualgrowth.mm = totalgrowth.mm - 
                         plugarea.mm, 
                      growthrate = actualgrowth.mm / timepoint) %>%
                      filter(hit.edge == "no", contaminated == "no") 

glm.tempdata$replicate <- as.factor(glm.tempdata$replicate)
glm.tempdata$temperature <- as.factor(glm.tempdata$temperature)
glimpse(glm.tempdata)

#remove all na values
glm.tempdata.narm <- na.omit(glm.tempdata)
#TODO look at -ve growthrates! for now filter out
glm.tempdata.narm <- filter(glm.tempdata.narm, growthrate > 0)

#####Models
#GLMM Mixed Effect odel
temp.model <- lmer(growthrate ~ temperature + daysinculture + 
                      speciesisolatedfrom + (1|isolate/replicate), 
                    REML = TRUE, data = glm.tempdata.narm, 
                    na.action = na.exclude)

#rescale variables
glm.tempdata.narm <- mutate(glm.tempdata.narm,
                              yearsinculture = daysinculture / 365)

temp.model.scaled <- lmer(growthrate ~ temperature + yearsinculture + 
                              speciesisolatedfrom + (1|isolate/replicate), 
                            REML = TRUE, data = glm.tempdata.narm, 
                            na.action = na.exclude)

summary(temp.model.scaled)

#get pvalues from model
drop1(temp.model.scaled, test = "Chisq")
#large rsquared value means random effect is having an effect
r.squaredGLMM(temp.model.scaled)

#####Model validation
hist(glm.tempdata.narm$growthrate)
qqnorm(glm.tempdata.narm$growthrate)
qqline(glm.tempdata.narm$growthrate, lty = 2)
#checking normal distribution of residuals
sresid <- resid(temp.model.scaled, type = "pearson")
hist(sresid)
#plots residuals vs fitted values
fits <- fitted(temp.model.scaled)
plot(sresid ~ fits)
#check for equal variances / homoscedasticity
plot(temp.model.scaled)

#plot residuals vs x variables
plot(sresid ~ glm.tempdata.narm$temperature)
plot(sresid ~ glm.tempdata.narm$yearsinculture)
plot(sresid ~ glm.tempdata.narm$speciesisolatedfrom)

mcp.fnc(temp.model.scaled)
plotLMER.fnc(temp.model.scaled)

#####GLMER rather than lmer
glmer.temp.model <- glmer(growthrate ~ temperature + daysinculture + 
                            speciesisolatedfrom + (1|isolate/replicate),
                          family = poisson (link = log),
                          nAGQ = 1, data = glm.tempdata.narm)

glmer.temp.model <- glmer.nb(growthrate ~ temperature + (1|isolate),
                          family = poisson (link = sqrt),
                            data = glm.tempdata.narm)

glm.tempdata.narm$growthrate <- as.numeric(glm.tempdata.narm$growthrate)
str(glm.tempdata.narm)
summary(glmer.temp.model)

######Testing other variations of the model
#test to see if instead of (1|isolate/replicate) can use unique ID value instead
test2 <- unite(glm.tempdata.narm, id, isolate, replicate, temperature, sep=".", 
               remove = FALSE)

test2.model <- lmer(growthrate ~ temperature + daysinculture + 
                      speciesisolatedfrom + (1|id), 
                    REML = TRUE, data = test2, na.action = na.exclude)

summary(test2.model)
#here we find that days in culture is now non-significant, and that there is
#negligible different in the rsquared value so interaction is expkaining a lot 
#of our growthrate (y variable)
drop1(test2.model, test = "Chisq")
r.squaredGLMM(test2.model)

#can try to take out the random effect in total to see what happens
test3.model <- glm(growthrate ~ temperature + daysinculture + 
                     speciesisolatedfrom, 
                    family = gaussian, data = glm.tempdata.narm, 
                    na.action = na.exclude)

#so temp and species is significant even without the random effect
summary(test3.model)
drop1(test3.model, test = "F")

#this null model and rsquared represents the variance in y that is explained by
#the random structure only (ie random effect of isolate/replicate)
#as the R2C (conditional rsquared) value is low (0.07) then the remaining 
#variance must be explained by the dependant variables!
null.model <- lmer(growthrate ~ 1 + (1|isolate/replicate), 
                   REML = TRUE, data = glm.tempdata.narm, 
                   na.action = na.exclude)

r.squaredGLMM(null.model)

##GLS model with no random effect for AIC comparison
test4.model <- gls(growthrate ~ 1 + temperature + daysinculture + 
                     speciesisolatedfrom, 
                   method = "REML", data = glm.tempdata, na.action = na.exclude)

###TODO add in na.rm remove for same number of observations for AIC

AIC(temp.model, test2.model, test3.model, test4.model, null.model)

#####Overdispertion function (overdisp_fun)
##check for overdispersion
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(temp.model.scaled)

