
###############################################################
setwd("C:/Masters/Thesis/James Stats")
new.data <- read.csv("data_final_totals.csv" , stringsAsFactors = T)
summary(new.data)
str(new.data)

library(nlme)
library(multcomp)
library(car)

## within each of the catchments - run t test to see whether there is a difference between the sites specifically - ##



####################################################################
# Variance decomposition (Type-II ANOVA model)

# one level decomposition

one.level <- lme(corrected_count_across_sizes.L ~ 1, random = ~1|Drain, data=new.data) 
VarCorr(one.level)
# rough check
7/57
# overall, drains don't explain the variation (12%) 

# two level maybe makes more sense, so lets do that properly

two.level <- lme(corrected_count_across_sizes.L ~ 1, random = ~1|Drain/Site.Code, data=new.data) 
VarCorr(two.level)
var.out <- VarCorr(two.level)
drains <- as.numeric(var.out[2,1])
site <- as.numeric(var.out[4,1])
within.site <- as.numeric(var.out[5,1])
tot <- drains + site + within.site
drains/tot
site/tot
within.site/tot

#  3% variation at the drain level
#  23% variation by site, unrelated to drain
#  73% variation residual: 

# I think this goes to a plastic is ubiquitous story
#  neither site nor drain explains the variation

#################################################################

# ANovA model treating drain as a factor

lme.a <- lme(log(corrected_count_across_sizes.L) ~
               Drain,
             random = ~1|Site.Code, data=new.data )  
summary(lme.a)
Anova(lme.a)

#  allow for unequal group variance at the drain level

lme.b <- lme(log(corrected_count_across_sizes.L) ~
               Drain, weights = varIdent(form = ~1 | Drain),
             random = ~1|Site.Code, data=new.data )  
summary(lme.b)

anova(lme.b,lme.a)

#  Allowing for hetero is not a statistically significant improvement

#  So model b is over fitting and then with over fitting we get a difference
#  but just to see what happens

Anova(lme.b)
Anova(lme.a)

Tukey.b <- glht(lme.b, linfct = mcp(Drain = "Tukey"))
summary(Tukey.b)

Tukey.a <- glht(lme.a, linfct = mcp(Drain = "Tukey"))
summary(Tukey.a)

# slightly different variance structure

lme.c <- lme(log(corrected_count_across_sizes.L) ~
               Drain, weights = varIdent(form = ~1 | Site.Code),
             random = ~1|Site.Code, data=new.data )  
summary(lme.c)
anova(lme.c,lme.a)

Tukey.c <- glht(lme.c, linfct = mcp(Drain = "Tukey"))
summary(Tukey.c)

#  Maybe there is something about drain K, but also this is over fitting

###  Lets not chase p-values! that would be bad science

#  There is no additional variance structure assumption needed

#  No different across drains

####################################################

#  Now explore regression model options for covariates that might make sense

#####REGRESSION - CATCHMENT POPULATION#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
               catchment_population, control=ctrl,
             random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
               catchment_population, weights = varIdent(form = ~1 | Drain), control=ctrl,
             random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       catchment_population, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#  catchment population is statistically significant

#  this can be treated as an empirical question

##################################################################################


#####################################################################

#####REGRESSION - CATCHMENT SIZE#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  catchment_area_ha, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       catchment_area_ha, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       catchment_area_ha, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#  catchment area is statistically significant

##################################################################################

#####REGRESSION - RAINFALL#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  rainfall_week_prior_mm, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       rainfall_week_prior_mm, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       rainfall_week_prior_mm, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#  rainfall is still not stat significant.
#  The variance decomposition possible says at site level variance makes sense
#  this can be treated as an empirical question


##################################################################################

#  exploration of log(Y) is something you can look at.  Note anova() cant be used to
#  compare log(Y)~ models and Y~ models


lme.base.het2.log <- lme(log(corrected_count_across_sizes.L) ~
                           rainfall_week_prior_mm, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                         random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.log)

#  Looking for nonlinear effect

lme.base.het2.quad <- lme(corrected_count_across_sizes.L ~
                            rainfall_week_prior_mm + I(rainfall_week_prior_mm^2), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                          random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.quad)

#  nothing here...

#  lets scale up..

new.data$rain <-new.data$rainfall_week_prior_mm/1000

lme.base.het2.quad2 <- lme(corrected_count_across_sizes.L ~
                             rain + I(rain^2), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                           random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.quad2)


##  nothing 

##  maybe impose cubic

lme.base.het2.cub <- lme(corrected_count_across_sizes.L ~
                           I(rainfall_week_prior_mm^3), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                         random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.cub)


## nothing

summary(lme.base.het2.quad2)

#  a log on x would capture this, approximately

lme.base.het2.xlog <- lme(corrected_count_across_sizes.L ~
                            log(rainfall_week_prior_mm), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                          random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.xlog)

#nothing with rainfall

#####################################################################

#####REGRESSION - RESIDENTIAL PROPORTION#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  residential_proportion, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       residential_proportion, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       residential_proportion, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 1 model is a stat better fit that het 2 model - variation at catchment level
#  residential proportion is statistically significant - negative relationship

##################################################################################

#####REGRESSION - AGRICULTURAL PROPORTION#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  agricultural_proportion, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       agricultural_proportion, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       agricultural_proportion, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#  agricultural proporiton is still not stat significant.
#  The variance decomposition possible says at site level variance makes sense
#  this can be treated as an empirical question


##################################################################################

#  exploration of log(Y) is something you can look at.  Note anova() cant be used to
#  compare log(Y)~ models and Y~ models


lme.base.het2.log <- lme(log(corrected_count_across_sizes.L) ~
                           agricultural_proportion, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                         random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.log)

#  Looking for nonlinear effect

lme.base.het2.quad <- lme(corrected_count_across_sizes.L ~
                            agricultural_proportion + I(agricultural_proportion^2), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                          random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.quad)

#  so maybe something here:  hard to see

#  so lets scale so we can see what is happening

new.data$ag <-new.data$agricultural_proportion/1000

lme.base.het2.quad2 <- lme(corrected_count_across_sizes.L ~
                             ag + I(ag^2), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                           random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.quad2)


##   can solve for the turning point (quadractic and see)

##  maybe impose cubic

lme.base.het2.cub <- lme(corrected_count_across_sizes.L ~
                           I(agricultural_proportion^3), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                         random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.cub)


###   not cubic
# so look closely at the quadratic maybe runs the other way

summary(lme.base.het2.quad2)

###no relationship###
#####################################################################

#####REGRESSION - INDUSTRIAL_PROPORTION#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  industrial_proportion, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       industrial_proportion, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       industrial_proportion, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#  industrial proportion statistically significant.

##################################################################################

#####REGRESSION - NATURAL PROPORTION (sum of water envornment and natural environment#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  natural_proportion, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       natural_proportion, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       natural_proportion, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#  natural area IS statistically significant...#
#######

#####################################################################

#####REGRESSION -  SERVICES PROPORTION#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  services_proportion, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       services_proportion, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       services_proportion, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#statistically significant - negative#

#####################################################################

#####REGRESSION - PUBLIC OPEN#######

ctrl <- lmeControl(opt='optim')  # less flakey search strategy
ctrl <- lmeControl(opt='optim', maxIter=1000)  # default is 50: it will run for a while if you cracnk it iup


lme.base <- lme(corrected_count_across_sizes.L ~
                  public_open, control=ctrl,
                random = ~1|Site.Code, data=new.data )  
summary(lme.base)


lme.base.het1 <- lme(corrected_count_across_sizes.L ~
                       public_open, weights = varIdent(form = ~1 | Drain), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het1)

anova(lme.base,lme.base.het1)
#  Low p-value say het model a better fit

lme.base.het2 <- lme(corrected_count_across_sizes.L ~
                       public_open, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                     random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2)
anova(lme.base,lme.base.het1,lme.base.het2)

#  Het 2 model is a stat better fit that het 1 model: variation at site level
#  public open is stat significant (positive correlation)



##############################################################################
#use this section of code if linear relationship not found and need to explore other options#
##################################################################################

#  exploration of log(Y) is something you can look at.  Note anova() cant be used to
#  compare log(Y)~ models and Y~ models


lme.base.het2.log <- lme(log(corrected_count_across_sizes.L) ~
                           catchment_population, weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                         random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.log)

#  Looking for nonlinear effect

lme.base.het2.quad <- lme(corrected_count_across_sizes.L ~
                            catchment_population + I(catchment_population^2), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                          random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.quad)

#  so maybe something here:  hard to see

#  so lets scale so we can see what is happening

new.data$pop <-new.data$catchment_population/1000

lme.base.het2.quad2 <- lme(corrected_count_across_sizes.L ~
                             pop + I(pop^2), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                           random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.quad2)


##   can solve for the turning point (quadractic and see)

##  maybe impose cubic

lme.base.het2.cub <- lme(corrected_count_across_sizes.L ~
                           I(catchment_population^3), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                         random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.cub)


###   Hmmmmmmmm
# so look closely at the quadratic maybe runs the other way

summary(lme.base.het2.quad2)

#  a log on x would capture this, approximately

lme.base.het2.xlog <- lme(corrected_count_across_sizes.L ~
                            log(catchment_population), weights = varIdent(form = ~1 | Site.Code), control=ctrl,
                          random = ~1|Site.Code, data=new.data )  
summary(lme.base.het2.xlog)

#  so maybe there is a population effect 
##this is showing whether variance is constant - HINT it is not, therefore use heter
summary(lme.base.het2$modelStruct$varStruct)
summary(lme.base.het1$modelStruct$varStruct)

#  lots to explore

#####################################################################
