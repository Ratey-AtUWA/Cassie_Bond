
getwd()
setwd("C:/Masters/Thesis/James Stats")



my.data <- read.csv("new_data.csv")
head(my.data)
dim(my.data)
df.new <- my.data[seq(1, nrow(my.data), 4), ]
head(df.new)
dim(df.new)

hist(df.new$Total.MP.across.sizes)
plot(density(df.new$Total.MP.across.sizes))
#shows the probability of a sample having a given MP value#

boxplot(df.new$Total.MP.across.sizes~as.factor(df.new$Site.Code))
boxplot(df.new$Total.MP.across.sizes~as.factor(df.new$Drain), boxwex=.6)

#  Check the exteme observation for H. could be something to look at

max(df.new$Total.MP.across.sizes)


library(nlme)


#null model - 1 factor (drain)
g.mean.0 <- lme(Total.MP.across.sizes~as.factor(Drain), random = ~1|Site.Code, data=df.new )    

#site specific#
g.mean.1 <- lme(Total.MP.across.sizes~as.factor(Drain), weights=varIdent(form=~1|Drain), random = ~1|Site.Code, data=df.new )    

anova(g.mean.0,g.mean.1) #  select simple model


library(multcomp)
library(car)

summary(g.mean.0)
Anova(g.mean.0)  #  so none of the drains differnt

# 
df.new$drain <- as.factor(df.new$Drain) 
g.mean.a <- lme(Total.MP.across.sizes~drain, random = ~1|Site.Code, data=df.new )    

#showing statistically significant pairings^^#

summary(glht(g.mean.a, linfct=mcp(drain = "Tukey")))

# or the traditional CI measure

confint(glht(g.mean.a, linfct=mcp(drain = "Tukey")))
plot(confint(glht(g.mean.a, linfct=mcp(drain = "Tukey"))))
#if statistically significant, the 0 should fall outside of the 95 CI. Estimates are dots, lwr upper make bounds#

########################################

#pure linear model using drains#
g.mean.test <- lm(Total.MP.across.sizes~as.factor(Drain), data=df.new )    
Anova(g.mean.test)  #  so none of the drains differnt
summary(g.mean.test)


#####################################################
#correction for the false positive rate#
# e.g. for data frame called 'cassie' 

set.seed(12345)
cassie$corrected_counts <- rep(NA,NROW(cassie)) 
for (i in 1:NROW(cassie)){
  cassie$corrected_counts[i] <- sum(rbinom(cassie$Total_MP[i], 1, 1-0.5263))
}
plot(cassie[,c("Total_MP","corrected_counts")]) 

# try this to see what's happening
rbinom(50, 1, 1-0.5263)



my.data <- read.csv("blank.correct.raw.csv")
set.seed(12345)
my.data$corrected_counts <- rep(NA,NROW(my.data)) 
for (i in 1:NROW(my.data)){
  my.data$corrected_counts[i] <- sum(rbinom(my.data$Total.MP.Blank.Correct[i], 1, 1-0.5263))
}
plot(my.data[,c("Total.MP.Blank.Correct","corrected_counts")]) 

# try this to see what's happening
rbinom(50, 1, 1-0.5263)

##check density plot for the corrected counts##

head(my.data)
dim(my.data)
df.new <- my.data[seq(1, nrow(my.data), 4), ]
head(df.new)
dim(df.new)

hist(df.new$corrected_counts)
plot(density(df.new$corrected_counts))

write.csv(my.data, "my.data.csv")

#####################################################
#investigating the corrected data#

corrected.data.across.size <- read.csv("corrected.across.sizes.csv")
head(corrected.data.across.size)
dim(corrected.data.across.size)
df.new <- corrected.data.across.size[seq(1, nrow(corrected.data.across.size), 4), ]
head(df.new)
dim(df.new)

par(mfrow = c(1,1))
hist(df.new$corrected_count_across_size_pL)
plot(density(df.new$corrected_count_across_size_pL))
#shows the probability of a sample having a given MP value#

boxplot(df.new$corrected_count_across_size_pL~as.factor(df.new$Site.Code))
boxplot(df.new$corrected_count_across_size_pL~as.factor(df.new$Drain), boxwex=.6)

#  Check the exteme observation for H. could be something to look at

max(df.new$corrected_count_across_size_pL)


library(nlme)


#null model - 1 factor (drain)
g.mean.0 <- lme(corrected_count_across_size_pL~as.factor(Drain), random = ~1|Site.Code, data=df.new )    

#site specific#
g.mean.1 <- lme(corrected_count_across_size_pL~as.factor(Drain), weights=varIdent(form=~1|Drain), random = ~1|Site.Code, data=df.new )    

anova(g.mean.0,g.mean.1) #  select simple model


library(multcomp)
library(car)

summary(g.mean.0)
Anova(g.mean.0)  #  so none of the drains differnt

# 
df.new$drain <- as.factor(df.new$Drain) 
g.mean.a <- lme(corrected_count_across_size_pL~drain, random = ~1|Site.Code, data=df.new )    

#showing statistically significant pairings^^#

summary(glht(g.mean.a, linfct=mcp(drain = "Tukey")))

# or the traditional CI measure

confint(glht(g.mean.a, linfct=mcp(drain = "Tukey")))
plot(confint(glht(g.mean.a, linfct=mcp(drain = "Tukey"))))
#if statistically significant, the 0 should fall outside of the 95 CI. Estimates are dots, lwr upper make bounds#

########################################

#pure linear model using drains#
g.mean.test <- lm(corrected_count_across_size_pL~as.factor(Drain), data=df.new )    
Anova(g.mean.test)  #  so none of the drains differnt
summary(g.mean.test)


#####################################################

