rm(list=ls())
library(epiR)
library(doBy)
library(reshape)
library(gmodels)
library(rtf)
library(stargazer)
library(xtable)
library(ggplot2)
library(BSDA)
library(car)
library(MASS)
library(lmtest)
library(ResourceSelection)
library(BaylorEdPsych)
library(lme4)
library(pscl)


##Load data
Data655 <- read.csv(file.choose(), header = TRUE)


##Dog cases##
Dogcases632 <- subset(Data655, Bitinganlspecies=="Dog")
table(Dogcases632$District)

##Rabied dos#
Rabieddog465 <- subset(Dogcases632, Bitinganlspecies=="Dog" & Rabied=="1")
table(Rabieddog465$District)

##Behavioural characteristics of the biting dogs
##proportion of rabied dog out of total dogs bites
table(Rabieddog465$District)/table(Dogcases632$District)

##confidence interval for proportions of diagnosis ability
epi.prev(pos=189,tested=208,se=1, sp=1, method = "wilson",conf.level = 0.95)
epi.prev(pos=189,tested=250,se=1, sp=1, method = "wilson",conf.level = 0.95)
epi.prev(pos=87,tested=174,se=1, sp=1, method = "wilson",conf.level = 0.95)

###test for significance 
prop.test(c(189,189,87),c(208,250,174))

Rabieddog465$Bishoftu<-factor(Rabieddog465$Bishoftu)
Rabieddog465$lemunabilbilo<-factor(Rabieddog465$lemunabilbilo)
Rabieddog465$yabelo<-factor(Rabieddog465$yabelo)

Bishoftudata<-Rabieddog465[which(Rabieddog465$District=='Bishoftu'), ]
lemunabilbilodata<-Rabieddog465[which(Rabieddog465$District=='Lemu bilbilo'), ]
Yabelodata<-Rabieddog465[which(Rabieddog465$District=='Yabelo'), ]


##chi square tables
#
##table and chi-square for Sex
table(Bishoftudata$Sex)
chisq.test(table(Bishoftudata$Sex))
table(lemunabilbilodata$Sex)
chisq.test(table(lemunabilbilodata$Sex))
table(Yabelodata$Sex)
chisq.test(table(Yabelodata$Sex))
table(Rabieddog465$Sex, Rabieddog465$District)
chisq.test(table(Rabieddog465$Sex, Rabieddog465$District))

##table and chi-square for religion
table(Rabieddog465$Religion, Rabieddog465$District)
chisq.test(table(Rabieddog465$Religion, Rabieddog465$District))

table(Bishoftudata$Religion)
chisq.test(table(Bishoftudata$Sex))
table(lemunabilbilodata$Religion)
chisq.test(table(lemunabilbilodata$Religion))
table(Yabelodata$Religion)
chisq.test(table(Yabelodata$Religion))

####table and chi-square for adulthood
table(Rabieddog465$Adulthoodornot, Rabieddog465$District)
chisq.test(table(Rabieddog465$Adulthoodornot, Rabieddog465$District))

table(Bishoftudata$Adulthoodornot)
chisq.test(table(Bishoftudata$Adulthoodornot))
table(lemunabilbilodata$Adulthoodornot)
chisq.test(table(lemunabilbilodata$Adulthoodornot))
table(Yabelodata$Adulthoodornot)
chisq.test(table(Yabelodata$Adulthoodornot))

######table and chi-square for education

Rabieddog465edu<-subset(Rabieddog465, !is.na(Educational_level))
Bishoftudataedu <- Rabieddog465edu[which(Rabieddog465edu$District=='Bishoftu'), ]
lemunabilbilodataedu<-Rabieddog465edu[which(Rabieddog465edu$District=='Lemu bilbilo'), ]
Yabelodataedu<-Rabieddog465edu[which(Rabieddog465edu$District=='Yabelo'), ]
table(Rabieddog465edu$Educational_level,Rabieddog465edu$District)
chisq.test(table(Rabieddog465edu$Educational_level,Rabieddog465edu$District))

table(Bishoftudataedu$Educational_level)
chisq.test(table(Bishoftudataedu$Educational_level))
table(lemunabilbilodataedu$Educational_level)
chisq.test(table(lemunabilbilodataedu$Educational_level))
table(Yabelodataedu$Educational_level)
chisq.test(table(Yabelodataedu$Educational_level))

######table and chi-square for Dog ownership
table(Rabieddog465$Ownershipofbitinganimals, Rabieddog465$District)
chisq.test(table(Rabieddog465$Ownershipofbitinganimals, Rabieddog465$District))

table(Bishoftudata$Ownershipofbitinganimals)
chisq.test(table(Bishoftudata$Ownershipofbitinganimals))
table(lemunabilbilodata$Ownershipofbitinganimals)
chisq.test(table(lemunabilbilodata$Ownershipofbitinganimals))
table(Yabelodata$Ownershipofbitinganimals)
chisq.test(table(Yabelodata$Ownershipofbitinganimals))

######table and chi-square for Severity of bite
table(Rabieddog465$Characteristicsofbite, Rabieddog465$District)
chisq.test(table(Rabieddog465$Characteristicsofbite, Rabieddog465$District))

table(Bishoftudata$Characteristicsofbite)
chisq.test(table(Bishoftudata$Characteristicsofbite))
table(lemunabilbilodata$Characteristicsofbite)
chisq.test(table(lemunabilbilodata$Characteristicsofbite))
table(Yabelodata$Characteristicsofbite)
chisq.test(table(Yabelodata$Characteristicsofbite))


######table and chi-square for Body part bitten
table(Rabieddog465$bodypartwasbittencluster,Rabieddog465$District )
chisq.test(table(Rabieddog465$bodypartwasbittencluster,Rabieddog465$District))

table(Bishoftudata$bodypartwasbittencluster)
chisq.test(table(Bishoftudata$bodypartwasbittencluster))
table(lemunabilbilodata$bodypartwasbittencluster)
chisq.test(table(lemunabilbilodata$bodypartwasbittencluster))
table(Yabelodata$bodypartwasbittencluster)
chisq.test(table(Yabelodata$bodypartwasbittencluster))


######table and chi-square for places sought for treatment
table(Rabieddog465$healthfacilitysought,Rabieddog465$District )
chisq.test(table(Rabieddog465$healthfacilitysought,Rabieddog465$District))

table(Bishoftudata$healthfacilitysought)
chisq.test(table(Bishoftudata$healthfacilitysought))
table(lemunabilbilodata$healthfacilitysought)
chisq.test(table(lemunabilbilodata$healthfacilitysought))
table(Yabelodata$healthfacilitysought)
chisq.test(table(Yabelodata$healthfacilitysought))

######table and chi-square for PET completeness
Rabieddog465pepcomp<-subset(Rabieddog465, !is.na(PEPcompleted))
Bishoftudatapepcomp <- Rabieddog465pepcomp[which(Rabieddog465pepcomp$District=='Bishoftu'), ]
lemunabilbilodatapepcomp<-Rabieddog465pepcomp[which(Rabieddog465pepcomp$District=='Lemu bilbilo'), ]
Yabelodatapepcomp<-Rabieddog465pepcomp[which(Rabieddog465pepcomp$District=='Yabelo'), ]
table(Rabieddog465pepcomp$PEPcompleted, Rabieddog465$District)
chisq.test(table(Rabieddog465pepcomp$PEPcompleted, Rabieddog465$District))

table(Bishoftudatapepcomp$PEPcompleted)
chisq.test(table(Bishoftudatapepcomp$PEPcompleted))
table(lemunabilbilodatapepcomp$PEPcompleted)
chisq.test(table(lemunabilbilodatapepcomp$PEPcompleted))
table(Yabelodatapepcomp$PEPcompleted)
chisq.test(table(Yabelodatapepcomp$PEPcompleted))

######table and chi-square for helperornot
#table(Rabieddog465$helperornot)
#chisq.test(table(Rabieddog465$helperornot))

#table(Bishoftudata$helperornot)
#chisq.test(table(Bishoftudata$helperornot))
#table(lemunabilbilodata$helperornot)
#chisq.test(table(lemunabilbilodata$helperornot))
#table(Yabelodata$helperornot)
#chisq.test(table(Yabelodata$helperornot))

######table and chi-square for Income
Rabieddog465$Whatistheincomeofthefamily<-factor(Rabieddog465$Whatistheincomeofthefamily)
table(Rabieddog465$Whatistheincomeofthefamily, Rabieddog465$District)
chisq.test(table(Rabieddog465$Whatistheincomeofthefamily, Rabieddog465$District))


table(Bishoftudata$Whatistheincomeofthefamily)
chisq.test(table(Bishoftudata$Whatistheincomeofthefamily))
table(lemunabilbilodata$Whatistheincomeofthefamily)
chisq.test(table(lemunabilbilodata$Whatistheincomeofthefamily))
table(Yabelodata$Whatistheincomeofthefamily)
chisq.test(table(Yabelodata$Whatistheincomeofthefamily))



##fate of dog
Rabieddog465fate<-subset(Rabieddog465, !is.na(Fateoftheanimal))
Rabieddog465fate$Fateoftheanimal<-factor(Rabieddog465fate$Fateoftheanimal)
Rabieddog465fate$Fateoftheanimal
table(Rabieddog465fate$Fateoftheanimal)

Bishoftudatafate <- Rabieddog465fate[which(Rabieddog465fate$District=='Bishoftu'), ]
lemunabilbilodatafate<-Rabieddog465fate[which(Rabieddog465fate$District=='Lemu bilbilo'), ]
Yabelodatafate<-Rabieddog465fate[which(Rabieddog465fate$District=='Yabelo'), ]
table(Rabieddog465fate$Fateoftheanimal, Rabieddog465fate$District)
chisq.test(table(Rabieddog465fate$Fateoftheanimal, Rabieddog465fate$District))


table(Bishoftudatafate$Fateoftheanimal)
chisq.test(table(Bishoftudatafate$Fateoftheanimal))
table(lemunabilbilodatafate$Fateoftheanimal)
chisq.test(table(lemunabilbilodatafate$Fateoftheanimal))
table(Yabelodatafate$Fateoftheanimal)
chisq.test(table(Yabelodatafate$Fateoftheanimal))

######table and anova for age overall= comparing continous data among three groups/districts
anova.age<- aov(Age~ District, data =Rabieddog465)
summary(anova.age)
TukeyHSD(anova.age)
mean(Bishoftudata$Age)
mean(lemunabilbilodata$Age)
mean(Yabelodata$Age)

######table and anova for Distance overall= comparing continous data among three groups/districts
anova.distance<-aov( GPSdistanceKM~ District, data =Rabieddog465)
summary(anova.distance)
TukeyHSD(anova.distance)

##for mean distances and confidence interval to the nearest health center
mean(Bishoftudata$GPSdistanceKM)
z.test(Bishoftudata$GPSdistanceKM, sigma.x = 6.8, conf.level = 0.95)
mean(lemunabilbilodata$GPSdistanceKM)
z.test(lemunabilbilodata$GPSdistanceKM, sigma.x = 6.8, conf.level = 0.95)
mean(Yabelodata$GPSdistanceKM)
z.test(Yabelodata$GPSdistanceKM, sigma.x = 6.8, conf.level = 0.95)


#####################################################################################################################

###Predictors of decisions on health centre visit after suspected rabid dog bite - without district 
table(Rabieddog465$Visit)
xtabs(~ District + Visit, data = Rabieddog465)

# Specify the varaibles that are to be used as factors (i.e. categorical vars)
Rabieddog465<-subset(Rabieddog465, !is.na(Fateoftheanimal))
Rabieddog465$Fateoftheanimal<-factor(Rabieddog465$Fateoftheanimal)
Rabieddog465<-subset(Rabieddog465, !is.na(Educational_level))
Rabieddog465$Educational_level<-factor(Rabieddog465$Educational_level)


Rabieddog465$District = as.factor(Rabieddog465$District)
Rabieddog465$Sex = as.factor(Rabieddog465$Sex)
Rabieddog465$Religion = as.factor(Rabieddog465$Religion)
Rabieddog465$Adulthoodornot = as.factor(Rabieddog465$Adulthoodornot)
Rabieddog465$Educational_level = as.factor(Rabieddog465$Educational_level)
Rabieddog465$Ownershipofbitinganimals = as.factor(Rabieddog465$Ownershipofbitinganimals)
Rabieddog465$Fateoftheanimal = as.factor(Rabieddog465$Fateoftheanimal)
Rabieddog465$Characteristicsofbite = as.factor(Rabieddog465$Characteristicsofbite)
Rabieddog465$bodypartwasbittencluster = as.factor(Rabieddog465$bodypartwasbittencluster)
Rabieddog465$Whatistheincomeofthefamily=as.factor(Rabieddog465$Whatistheincomeofthefamily)
nrow(Rabieddog465) ##444

##for all district#
##Univariate analysis using loop
lapply(c("Sex","Religion","Adulthoodornot","Educational_level","Ownershipofbitinganimals","Characteristicsofbite",
         "Fateoftheanimal","bodypartwasbittencluster","Age","GPSdistanceKM", "Whatistheincomeofthefamily"),
       
       function(var) {
         
         formula    <- as.formula(paste("Visit ~", var))
         res.logist <- glm(formula, data = Rabieddog465, family = binomial,na.action = na.exclude)
         
         summary(res.logist)
       })



##now multivariable logistic regresion for variables woth P value of greater than 0.2
##first set reference
Rabieddog465$Ownershipofbitinganimals <- relevel(Rabieddog465$Ownershipofbitinganimals, ref = "Own")
Rabieddog465$Characteristicsofbite <- relevel(Rabieddog465$Characteristicsofbite, ref = "Minor")
Rabieddog465$bodypartwasbittencluster <- relevel(Rabieddog465$bodypartwasbittencluster, ref = "Leg")
Rabieddog465$Fateoftheanimal<-relevel(Rabieddog465$Fateoftheanimal, ref = "Killed")
Rabieddog465$District <- relevel(Rabieddog465$District, ref = "Bishoftu")
Rabieddog465edu$Educational_level<-relevel(Rabieddog465edu$Educational_level, ref="Illitrate")
Rabieddog465$Fateoftheanimal<-relevel(Rabieddog465$Fateoftheanimal, ref = "Killed")
mod1 <- glm(Visit ~ Sex + Educational_level + Ownershipofbitinganimals + Characteristicsofbite +Fateoftheanimal +bodypartwasbittencluster + GPSdistanceKM + Whatistheincomeofthefamily,
            family = binomial(logit), data=Rabieddog465, na.action = na.exclude)

summary(mod1)


##Check for multicolliniarity
vif(mod1)

# Try step-wise removal of 'redundant' variables to get the best model
mod2<-step(mod1)

summary(mod2)

anova(mod2, test="Chisq")

#model fit tests

pR2(mod2)

## check for predictive power, Pseudo R-square measures (Negelkerke R square) 
PseudoR2(mod2)

## check for goodness of fit, Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(Rabieddog465$Visit,fitted(mod2))

# Show mod2 and also the 95% CI on the coefficients

sum<-summary(mod2)
sum

(cbind(CoEff = coef(mod2), confint(mod2)))

# We can also exponentiate coefficients to get ORs and their (exponentiated) confidence intervals.
# To put it all in one table, use cbind to bind the coefficients and confidence intervals column-wise.
exp(coef(mod2))
OR <- exp(cbind(OR = coef(mod2), confint(mod2)))
OR



###########################################################################################################################
###Predictors of decisions on health centre visit after suspected rabid dog bite -  with districts
Data655 <- read.csv(file.choose(), header = TRUE)


##Dog cases##
Dogcases632 <- subset(Data655, Bitinganlspecies=="Dog")
table(Dogcases632$District)

##Rabied dos#
Rabieddog465 <- subset(Dogcases632, Bitinganlspecies=="Dog" & Rabied=="1")
table(Rabieddog465$District)


Rabieddog465<-subset(Rabieddog465, !is.na(Fateoftheanimal))
Rabieddog465$Fateoftheanimal<-factor(Rabieddog465$Fateoftheanimal)
Rabieddog465<-subset(Rabieddog465, !is.na(Educational_level))
Rabieddog465$Educational_level<-factor(Rabieddog465$Educational_level)
Rabieddog465$Fateoftheanimal

Rabieddog465$District = as.factor(Rabieddog465$District)
Rabieddog465$Sex = as.factor(Rabieddog465$Sex)
Rabieddog465$Religion = as.factor(Rabieddog465$Religion)
Rabieddog465$Adulthoodornot = as.factor(Rabieddog465$Adulthoodornot)
Rabieddog465$Educational_level = as.factor(Rabieddog465$Educational_level)
Rabieddog465$Ownershipofbitinganimals = as.factor(Rabieddog465$Ownershipofbitinganimals)
Rabieddog465$Fateoftheanimal = as.factor(Rabieddog465$Fateoftheanimal)
Rabieddog465$Characteristicsofbite = as.factor(Rabieddog465$Characteristicsofbite)
Rabieddog465$bodypartwasbittencluster = as.factor(Rabieddog465$bodypartwasbittencluster)
Rabieddog465$Whatistheincomeofthefamily=as.factor(Rabieddog465$Whatistheincomeofthefamily)
nrow(Rabieddog465) ##444


##Univariate analysis using loop
lapply(c("Sex","Religion","Adulthoodornot","Educational_level","Ownershipofbitinganimals","Characteristicsofbite",
         "Fateoftheanimal","bodypartwasbittencluster","Age","GPSdistanceKM", "Whatistheincomeofthefamily", "Bishoftu", "lemunabilbilo","yabelo"),
       
       function(var) {
         
         formula    <- as.formula(paste("Visit ~", var))
         res.logist <- glm(formula, data = Rabieddog465, family = binomial,na.action = na.exclude)
         
         summary(res.logist)
       })


##now multivariable logistic regresion for variables woth P value of greater than 0.2
##first set reference

Rabieddog465$Ownershipofbitinganimals <- relevel(Rabieddog465$Ownershipofbitinganimals, ref = "Own")
Rabieddog465$Characteristicsofbite <- relevel(Rabieddog465$Characteristicsofbite, ref = "Minor")
Rabieddog465$bodypartwasbittencluster <- relevel(Rabieddog465$bodypartwasbittencluster, ref = "Leg")
Rabieddog465$Fateoftheanimal<-relevel(Rabieddog465$Fateoftheanimal, ref = "Killed")
Rabieddog465$District <- relevel(Rabieddog465$District, ref = "Bishoftu")
Rabieddog465edu$Educational_level<-relevel(Rabieddog465edu$Educational_level, ref="Illitrate")
Rabieddog465$Fateoftheanimal<-relevel(Rabieddog465$Fateoftheanimal, ref = "Killed")

modintvis <- glm(Visit ~ Sex + Educational_level + Ownershipofbitinganimals + Characteristicsofbite +Fateoftheanimal +bodypartwasbittencluster + GPSdistanceKM + Whatistheincomeofthefamily +lemunabilbilo +yabelo,
            family = binomial(logit), data=Rabieddog465, na.action = na.exclude)

summary(modintvis)

##Check for multicolliniarity
vif(modintvis)

# Try step-wise removal of 'redundant' variables to get the best model
modinteravis<-step(modintvis)

summary(modinteravis)

anova(modinteravis, test="Chisq")

#model fit tests
pR2(modinteravis)

## check for predictive power, Pseudo R-square measures (Negelkerke R square) 
PseudoR2(modinteravis)

## check for goodness of fit, Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(Rabieddog465$Visit,fitted(modinteravis))

# Show mod2 and also the 95% CI on the coefficients

sum<-summary(modinteravis)
sum

(cbind(CoEff = coef(modinteravis), confint(modinteravis)))

# We can also exponentiate coefficients to get ORs and their (exponentiated) confidence intervals.
# To put it all in one table, use cbind to bind the coefficients and confidence intervals column-wise.
exp(coef(modinteravis))
OR <- exp(cbind(OR = coef(modinteravis), confint(modinteravis)))
OR

####################################################################################################################################
#####for PEP completness- reciving sufficient doses of PEP- without district
##Rabied, healthcenter visit


##Load data

rm(list=ls())

Data655 <- read.csv(file.choose(), header = TRUE)


Rabieddoghealtcenter<- subset(Data655, Bitinganlspecies=="Dog" & Rabied=="1"& healthfacilitysought=="Health center")
table(Rabieddoghealtcenter$District)

nrow(Rabieddoghealtcenter)

Rabieddoghealtcenter<-subset(Rabieddoghealtcenter, !is.na(Fateoftheanimal))
Rabieddoghealtcenter$Fateoftheanimal<-factor(Rabieddoghealtcenter$Fateoftheanimal)
Rabieddoghealtcenter<-subset(Rabieddoghealtcenter, !is.na(Educational_level))
Rabieddoghealtcenter$Educational_level<-factor(Rabieddoghealtcenter$Educational_level)
Rabieddoghealtcenter<-subset(Rabieddoghealtcenter, !is.na(FULLPEP))
Rabieddoghealtcenter$FULLPEP<-factor(Rabieddoghealtcenter$FULLPEP)

nrow(Rabieddoghealtcenter)

# Specify the varaibles that are to be used as factors (i.e. categorical vars)
Rabieddoghealtcenter$District = as.factor(Rabieddoghealtcenter$District)
Rabieddoghealtcenter$Sex = as.factor(Rabieddoghealtcenter$Sex)
Rabieddoghealtcenter$Religion = as.factor(Rabieddoghealtcenter$Religion)
Rabieddoghealtcenter$Adulthoodornot = as.factor(Rabieddoghealtcenter$Adulthoodornot)
Rabieddoghealtcenter$Educational_level = as.factor(Rabieddoghealtcenter$Educational_level)
Rabieddoghealtcenter$Ownershipofbitinganimals = as.factor(Rabieddoghealtcenter$Ownershipofbitinganimals)
Rabieddoghealtcenter$Fateoftheanimal = as.factor(Rabieddoghealtcenter$Fateoftheanimal)
Rabieddoghealtcenter$Characteristicsofbite = as.factor(Rabieddoghealtcenter$Characteristicsofbite)
Rabieddoghealtcenter$bodypartwasbittencluster = as.factor(Rabieddoghealtcenter$bodypartwasbittencluster)
Rabieddoghealtcenter$Whatistheincomeofthefamily=as.factor(Rabieddoghealtcenter$Whatistheincomeofthefamily)

Rabieddog465bishcomp<- subset(Rabieddoghealtcenter, District="Bishoftu")
Rabieddog465lemucomp<- subset(Rabieddoghealtcenter, District="Lemu bilbilo")
Rabieddog465bishcomp<- subset(Rabieddoghealtcenter, District="Yabelo")

lapply(c("Sex","Religion","Adulthoodornot","Educational_level","Ownershipofbitinganimals","Characteristicsofbite","Whatistheincomeofthefamily",
         "Fateoftheanimal","bodypartwasbittencluster","Age","GPSdistanceKM"),
       
       function(var) {
         
         formula    <- as.formula(paste("FULLPEP ~", var))
         res.logist <- glm(formula, data = Rabieddoghealtcenter, family = binomial,na.action = na.exclude)
         
         summary(res.logist)
       })



##now multivariable logistic regresion for variables woth P value of greater than 0.2
##first set reference
Rabieddoghealtcenter$Ownershipofbitinganimals <- relevel(Rabieddoghealtcenter$Ownershipofbitinganimals, ref = "Own")
Rabieddoghealtcenter$Characteristicsofbite <- relevel(Rabieddoghealtcenter$Characteristicsofbite, ref = "Minor")
Rabieddoghealtcenter$District <- relevel(Rabieddoghealtcenter$District, ref = "Bishoftu")
Rabieddoghealtcenter$Educational_level <- relevel(Rabieddoghealtcenter$Educational_level, ref = "Illitrate")
Rabieddoghealtcenter$Whatistheincomeofthefamily <- relevel(Rabieddoghealtcenter$Whatistheincomeofthefamily, ref = "<20")


modfullpep <- glm(FULLPEP ~ Age + Ownershipofbitinganimals + GPSdistanceKM+Educational_level + Characteristicsofbite +Whatistheincomeofthefamily,
                  family = binomial(logit), data=Rabieddoghealtcenter, na.action = na.exclude)

summary(modfullpep)

##Check for multicolliniarity
vif(modfullpep)
sqrt(vif(modfullpep))

# Try step-wise removal of 'redundant' variables to get the best model
modfullpep2 <- step(modfullpep)

##Gooddness of fit-likelihood ratio test if mod2 is better than mod1
## check for predictive power, Pseudo R-square measures (Negelkerke R square) 

pR2(modfullpep2)

anova(modfullpep2, test="Chisq")

PseudoR2(modfullpep2)

## check for goodness of fit, Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(Rabieddoghealtcenter$FULLPEP,fitted(modfullpep2))

# Show mod2 and also the 95% CI on the coefficients

summary(modfullpep2)

(cbind(CoEff = coef(modfullpep2), confint(modfullpep2)))

# We can also exponentiate coefficients to get ORs and their (exponentiated) confidence intervals.
# To put it all in one table, use cbind to bind the coefficients and confidence intervals column-wise.
exp(coef(modfullpep2))
OR <- exp(cbind(OR = coef(modfullpep2), confint(modfullpep2)))
OR




######################################################################################################################
###for PEP completness- reciving sufficient doses of PEP- with districts 
rm(list=ls())
Data655 <- read.csv(file.choose(), header = TRUE)


Rabieddoghealtcenter<- subset(Data655, Bitinganlspecies=="Dog" & Rabied=="1"& healthfacilitysought=="Health center")
table(Rabieddoghealtcenter$District)


Rabieddoghealtcenter<-subset(Rabieddoghealtcenter, !is.na(Fateoftheanimal))
Rabieddoghealtcenter$Fateoftheanimal<-factor(Rabieddoghealtcenter$Fateoftheanimal)
Rabieddoghealtcenter<-subset(Rabieddoghealtcenter, !is.na(Educational_level))
Rabieddoghealtcenter$Educational_level<-factor(Rabieddoghealtcenter$Educational_level)
Rabieddoghealtcenter<-subset(Rabieddoghealtcenter, !is.na(FULLPEP))
Rabieddoghealtcenter$FULLPEP<-factor(Rabieddoghealtcenter$FULLPEP)

nrow(Rabieddoghealtcenter)

# Specify the varaibles that are to be used as factors (i.e. categorical vars)
Rabieddoghealtcenter$District = as.factor(Rabieddoghealtcenter$District)
Rabieddoghealtcenter$Sex = as.factor(Rabieddoghealtcenter$Sex)
Rabieddoghealtcenter$Religion = as.factor(Rabieddoghealtcenter$Religion)
Rabieddoghealtcenter$Adulthoodornot = as.factor(Rabieddoghealtcenter$Adulthoodornot)
Rabieddoghealtcenter$Educational_level = as.factor(Rabieddoghealtcenter$Educational_level)
Rabieddoghealtcenter$Ownershipofbitinganimals = as.factor(Rabieddoghealtcenter$Ownershipofbitinganimals)
Rabieddoghealtcenter$Fateoftheanimal = as.factor(Rabieddoghealtcenter$Fateoftheanimal)
Rabieddoghealtcenter$Characteristicsofbite = as.factor(Rabieddoghealtcenter$Characteristicsofbite)
Rabieddoghealtcenter$bodypartwasbittencluster = as.factor(Rabieddoghealtcenter$bodypartwasbittencluster)
Rabieddoghealtcenter$Whatistheincomeofthefamily=as.factor(Rabieddoghealtcenter$Whatistheincomeofthefamily)

Rabieddog465bishcomp<- subset(Rabieddoghealtcenter, District="Bishoftu")
Rabieddog465lemucomp<- subset(Rabieddoghealtcenter, District="Lemu bilbilo")
Rabieddog465bishcomp<- subset(Rabieddoghealtcenter, District="Yabelo")


lapply(c("Sex","Religion","Adulthoodornot","Educational_level","Ownershipofbitinganimals","Characteristicsofbite","Whatistheincomeofthefamily",
         "Fateoftheanimal","bodypartwasbittencluster","Age","GPSdistanceKM", "Bishoftu", "lemunabilbilo", "yabelo"),
       
       function(var) {
         
         formula    <- as.formula(paste("FULLPEP ~", var))
         res.logist <- glm(formula, data = Rabieddoghealtcenter, family = binomial,na.action = na.exclude)
         
         summary(res.logist)
       })



##now multivariable logistic regresion for variables woth P value of greater than 0.2
##first set reference
Rabieddoghealtcenter$Ownershipofbitinganimals <- relevel(Rabieddoghealtcenter$Ownershipofbitinganimals, ref = "Own")
Rabieddoghealtcenter$Characteristicsofbite <- relevel(Rabieddoghealtcenter$Characteristicsofbite, ref = "Minor")
Rabieddoghealtcenter$Educational_level <- relevel(Rabieddoghealtcenter$Educational_level, ref = "Illitrate")
Rabieddoghealtcenter$Whatistheincomeofthefamily <- relevel(Rabieddoghealtcenter$Whatistheincomeofthefamily, ref = "<20")
modfullpep <- glm(FULLPEP ~ Age + GPSdistanceKM+Educational_level + Ownershipofbitinganimals+ Characteristicsofbite +Whatistheincomeofthefamily +lemunabilbilo+yabelo,
                  family = binomial(logit), data=Rabieddoghealtcenter, na.action = na.exclude)

summary(modfullpep)

##Check for multicolliniarity
vif(modfullpep)
sqrt(vif(modfullpep))

# Try step-wise removal of 'redundant' variables to get the best model
modfullpep2 <- step(modfullpep)

##Gooddness of fit-likelihood ratio test if mod2 is better than mod1
## check for predictive power, Pseudo R-square measures (Negelkerke R square) 
library(pscl)
pR2(modfullpep2)

anova(modfullpep2, test="Chisq")


PseudoR2(modfullpep2)

## check for goodness of fit, Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(Rabieddoghealtcenter$FULLPEP,fitted(modfullpep2))

# Show mod2 and also the 95% CI on the coefficients

summary(modfullpep2)

(cbind(CoEff = coef(modfullpep2), confint(modfullpep2)))

# We can also exponentiate coefficients to get ORs and their (exponentiated) confidence intervals.
# To put it all in one table, use cbind to bind the coefficients and confidence intervals column-wise.
exp(coef(modfullpep2))
OR <- exp(cbind(OR = coef(modfullpep2), confint(modfullpep2)))
OR








  
