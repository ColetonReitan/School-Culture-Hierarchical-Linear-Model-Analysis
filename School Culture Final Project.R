library(descr)
library(Hmisc) 
library(plyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(sjstats)
library(lattice)
library(merTools)
library(effects) 


# Import a csv file
mydataset <- read.csv("C:/Users/colet/Documents/Wesleyan QAC/QAC312 Hierarchical Linear Models/school_culture_final.csv") 

# convert all variables names to lowercase
colnames(mydataset) <- tolower(colnames(mydataset))

# select a subset of variables to exclude observations with missing data
var.keep <- c("school", "sesc", "patt92", "suspser", "atssc", "eesc")
data2 <- mydataset[ , var.keep]
data3 <- na.omit(data2)
describe(mydataset$sesc)

# number of level 2 units
length(unique(data3$school))

# average number of level 1 observations (students) per level 2 unit (school)
numlev1 <- ddply(data3, c("school"), summarise, N=sum(!is.na(sesc)))
describe(numlev1$N)

#plot mean response and error bars by level 2 type
cdata <- ddply(data3, c("school"), summarise,
               N    = sum(!is.na(sesc)),
               mean = mean(sesc, na.rm=TRUE),
               sd   = sd(sesc, na.rm=TRUE),
               se   = sd / sqrt(N))
print(cdata)

# reorder observations by mean effort
cdata$school <- factor(cdata$school, levels = cdata$school[order(cdata$mean)])

# plot 
p <- ggplot(cdata, aes(x=school, y=mean))
p + geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se)) + xlab("School ID") + ylab("Mean Effort Towards School")

# random intercept model (random effects ANOVA)  
hlm1 <- lmer(sesc ~ 1 + (1|school), data3, REML=F)
summary(hlm1)

# ICC
totalvar<-0.063+1.981
icc<-0.063/totalvar
print(icc)
performance::icc(hlm1)

# confidence intervals
hlm1ci <- confint(hlm1, method="profile")
print(hlm1ci)

# design effect
deseff<-1+0.031*(586-1)
print(deseff)

# amount of bias in standard errors based on design effect
deft=sqrt(deseff)
print(deft)

'''
shows that if clustering is ignored then standard errors are 3 times smaller than they
would be if clustering were taken into account
effective sample size
'''

effsize=6446/19.135
print(effsize)

MuMIn::r.squaredGLMM(hlm1)

#plot school level random effects (standard deviations with confidence intervals)
randoms <- REsim(hlm1, n.sims = 100)
plotREsim(REsim(hlm1, n.sims = 100), stat='mean', sd = TRUE)

#-----------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------

#1
# group mean center quantitative Level 1 predictor (atssc)
# calculate school mean effort towards school
mean1 <- ddply(data3, c("school"), summarise, mean_atssc=mean(atssc))

# merge back with data3
data3 <- join(data3, mean1, by='school', type='left', match='all')
data3$atssc_c<-data3$atssc-data3$mean_atssc
describe(data3$atssc)
describe(data3$sesc)

# add student level atssc with random intercept
hlm2 <- lmer(sesc ~ 1 + atssc_c + (1|school), data3, REML=F)
summary(hlm2)

#2
# Likelihood ratio chi-square difference test for adding a level 1 predictor
# anova(simple model; complex model)
anova(hlm1,hlm2)

# confidence intervals
hlm2ci <- confint(hlm2, method="profile", oldNames=F)
print(hlm2ci)

# intraclass correlation coefficient
performance::icc(hlm2)

# plot association between ATSSC and effort towards school 
# first get predicted effort towards school
predeff <- fitted(hlm2)

# combine school id and centered ATSSC score variables with predicted
datapred <- unique(data.frame(cbind(predeff = predeff, atssc_c =
                                      data3$atssc_c, school = data3$school)))

# generate plot
xyplot(predeff ~ atssc_c, data = datapred, groups = school, type = c("p","l"), col = "blue", 
       xlab="Student ATSSC (group mean centered)", ylab="Predicted Effort Towards School",
       title="Slope of Association of Student ATSSC on Effort Towards School by School")

# plot random effects
randoms2 <- REsim(hlm2, n.sims = 500)
plotREsim(randoms2)

#---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------

#Add a level 1 predictor with a random slope
hlm3 <- lmer(sesc ~ 1 + atssc_c + (1+atssc_c|school), data3, REML=F)
summary(hlm3)

#chi-square difference test for adding a random slope
anova(hlm2,hlm3)

# confidence intervals
hlm3ci <- confint(hlm3, method="profile", oldNames=F)
print(hlm3ci)

# plot association between student attatchment to shcool and effort towards school
# Step 1: get predicted effort towards school
predscore3 <- fitted(hlm3)

# combine school id and centered atssc score variables with predicted scores
datapred3 <- unique(data.frame(cbind(predscore3 = predscore3, atssc_c =
                                       data3$atssc_c, school = data3$school)))

hlm3g <- lmer(sesc ~ 1 + atssc_c + eesc + (1+eesc|school), data3, REML=F)

# generate plot
xyplot(predscore3 ~ atssc_c, data = datapred3, groups = school, type = c("p","l"), col = "blue", 
       xlab="Student ATSSC (centered)", ylab="Predicted Effort towards School by School")

# add another Level 1 predictor with a fixed slope
hlm4a <- lmer(sesc ~ 1 + atssc_c + eesc + (1+atssc_c|school), data3, REML=F)
summary(hlm4a)

# remove atssc_c random slope (nonconvergence)
hlm4a <- lmer(sesc ~ 1 + atssc_c + eesc + (1|school), data3, REML=F)
summary(hlm4a)

# Likelihood ratio chi-square difference test for adding another level 1 predictor
# anova(simpler model; complex model)
anova(hlm2,hlm4a)

# adding random slope for educational expectations
hlm4b <- lmer(sesc ~ 1 + atssc_c + eesc + (1+eesc|school), data3, REML=F)
summary(hlm4b)

# Likelihood ratio chi-square difference test for adding a random slope for level 1 predictor
# anova(simpler model; complex model)anova(hlm4a,hlm4b)
anova(hlm4a,hlm4b)

# confidence interval
hlm4ci <- confint(hlm4b, method="profile", oldNames=F)
print(hlm4ci)

# plot association between educational expectation and effort towards school
# Step 1: get predicted effort towards school
predeff4 <- fitted(hlm4b)

# combine school id and centered sesc effort variables with predicted effort
datapred4  <- unique(data.frame(cbind(predeff4 = predeff4, school = data3$school, 
                                      eesc =  data3$eesc)))

# recode expectation for axis labels
datapred4$eesc<-as.factor(datapred4$eesc)

# compute mean predicted math achievement score by minority status for each school  
plotdata4 <- ddply(datapred4, c("school","eesc"), summarise, mean_predicted = mean(predeff4, na.rm=FALSE))

# generate plot
xyplot(mean_predicted ~ eesc, data = plotdata4, groups = school, type = c("p","l"), col = "blue", 
       xlab="School Expectations", ylab="Predicted Effort Towards School")
# plot random effects
reEx <- as.data.frame(REsim(hlm4b))
head(reEx)
p1 <- plotREsim(reEx)
p1

#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# ADDING LV2 PREDICTORS

# grand mean center quantitative predictors
describe(data3$patt92)
data3$patt92_c<-data3$patt92-mean(data3$patt92)
describe(data3$patt92_c)

describe(data3$eesc)
data3$eesc_c <- data3$eesc-mean(data3$eesc)
describe(data3$eesc_c)


hlm4 <- lmer(sesc ~ 1 + atssc_c + patt92 + (1+atssc_c|school), data3, REML=F)
summary(hlm4)

# compare models with and without Level 2 school type variable
anova(hlm3,hlm4)

# add random slope for school type 
hlm5 <- lmer(sesc ~ 1 + atssc_c + patt92 + (1+atssc_c+patt92|school), data3, REML=F)
summary(hlm5)
anova(hlm4,hlm5)

# compute confidence intervals for hlm4 model (random slope for ... was not significant)
hlm4ci <- confint(hlm4, method="profile", oldNames=F)
print(hlm4ci)

# adding a cross-level interaction between school type and student ses
hlm6 <- lmer(sesc ~ 1 + atssc_c + eesc_c + patt92_c + patt92_c*atssc_c + (1+atssc_c|school), data3, REML=F)
summary(hlm6)

anova(hlm1,hlm6g)

# FINAL MODEL IS HLM6

#compute confidence intervals for final model
hlm6ci <- confint(hlm6, method="profile", oldNames=F)
print(hlm6ci)

# intraclass correlation coefficient
performance::icc(hlm4)

# R-square for final model
MuMIn::r.squaredGLMM(hlm6)

# plot interaction
plot(effect("atssc_c:patt92_c",hlm6))

# alternative interaction plot using ggplot
int1<-effect("atssc_c:patt92_c",hlm6)
summary(int1)

# save effects as a data frame
int1data <- as.data.frame(int1)
int1data

# plot
ggplot(int1data, aes(atssc_c, fit, color=patt92_c)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4) + theme_bw(base_size=12)



# final model random effects plot
randoms6 <- REsim(hlm6, n.sims = 500)
plotREsim(randoms6)
plotREsim(REsim(hlm1, n.sims = 500), stat='mean', sd = TRUE)

hlm6f <- lmer(sesc ~ 1 + atssc_c + patt92_c + patt92_c*atssc_c + (1+atssc_c|school), data3, REML=F)
summary(hlm6f)

hlm6g <- lmer(sesc ~ 1 + atssc_c + eesc_c + patt92_c + patt92_c*atssc_c + (1+eesc_c|school), data3, REML=F)
summary(hlm6g)

anova(hlm6, hlm6g)


hlm6gci <- confint(hlm6g, method="profile", oldNames=F)
print(hlm6gci)


MuMIn::r.squaredGLMM(hlm6g)