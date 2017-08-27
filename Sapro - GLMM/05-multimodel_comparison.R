temp.model <- lmer(growthrate ~ temperature + daysinculture + 
                     speciesisolatedfrom + (1|isolate/replicate), 
                   REML = TRUE, data = glm.tempdata, na.action = na.exclude)

global.model <- glm (growthrate ~
                       temperature + daysinculture + speciesisolatedfrom
                        + temperature:daysinculture + daysinculture:speciesisolatedfrom + temperature:speciesisolatedfrom
                        + temperature:daysinculture:speciesisolatedfrom,
                     family = gaussian (link = identity),
                     na.action = na.exclude,
                     data=glm.tempdata)

plot(global.model)

null.model1 <- glm(growthrate ~ 1 + (1|isolate/replicate), 
                   family = gaussian (link = identity),
                   na.action = na.exclude,
                   data=glm.tempdata)
