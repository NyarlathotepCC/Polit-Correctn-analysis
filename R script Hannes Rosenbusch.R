#####week 1

#exploration and first description of the variables

###1
setwd("C:/Users/Hannes/Desktop/Courses ReMa/Applied Statistics")
load("main.Rdata")

###2
main
summary(main)
names(main)

###3
attach(main)
#the following variables are categorical:
#gender, condition, WorkerId, educ
#all others are numeric
#creating a factor variable for condition and gender
main$conditionf<-as.factor(condition)
main$genderf<-as.factor(gender)

###4 (description of variables)
#WorkerId - ID Variable
#condition - experimental condition that participant was in; not all 5 conditions will be used for this course
#Avg_NegSelf_PC - Construct Scale Score; Potential moderator in proposed model
#Avg_PosAtt_PC - Construct Scale Score; Alternative Moderator in proposed model
#Avg_MSLIM_RAT - Construct SCale Score; Dependent Variable
#PCSelf1_49 - single Item from "Avg_NegSelf_PC" Scale
#PCSelf2_34 - single Item from "Avg_NegSelf_PC" Scale
#PCSelf_28 - single Item from "Avg_NegSelf_PC" Scale
#PCSelf_39 - single Item from "Avg_NegSelf_PC" Scale
#PCNecssry_35 - single Item from "Avg_PosAtt_PC" Scale
#PCGeneral_38 - single Item from "Avg_PosAtt_PC" Scale
#PCGeneral_71 - single Item from "Avg_PosAtt_PC" Scale
#PCEffectiv_10 - single Item from "Avg_PosAtt_PC" Scale
#preAttentionCheck_67 - Attention CHeck Item
#Muslimrati_9N - single Item from "Avg_MSLIM_RAT" Scale
#Muslimrati_10N - single Item from "Avg_MSLIM_RAT" Scale
#Muslimrati_11P - single Item from "Avg_MSLIM_RAT" Scale
#Muslimrati_12N - single Item from "Avg_MSLIM_RAT" Scale
#Muslimrati_13N - single Item from "Avg_MSLIM_RAT" Scale
#Muslimrati_14P - single Item from "Avg_MSLIM_RAT" Scale
#Muslimrati_15N - single Item from "Avg_MSLIM_RAT" Scale
#gender - gender of participant; control; exploratory
#age - age of participant; control; exploratory
#educ - highest degree; control; exploratory

###5 (Missing Values)
summary(main)
#no missing values but one for the age item

###6 (check reliability of 3 scales)
library (psych)
alpha(main[,c(6,7,8,9)])
#the "Avg_NegSelf_PC" Scale has a raw alpha of 0.79
alpha(main[,c(10,11,12,13)])
#the "Avg_PosAtt_PC" Scale has a raw alpha of 0.92 
#The "Avg_MSLIM_RAT" Scale has items with both positive and negative formulations
#I will recode the all variables into positive variables, in order to make a scale score afterwards

main$Muslimrati_9rec<--1*Muslimrati_9N
main$Muslimrati_10rec<--1*Muslimrati_10N
main$Muslimrati_12rec<--1*Muslimrati_12N
main$Muslimrati_13rec<--1*Muslimrati_13N
main$Muslimrati_15rec<--1*Muslimrati_15N
main$Muslimrati_11rec<-Muslimrati_11P
main$Muslimrati_14rec<-Muslimrati_14P
main[,27]
alpha(main[,c(28,29,30,31,32,33,34)])
#the "Avg_MSLIM_RAT" Scale has a raw alpha of 0.92

###7 (renaming?)
#I prefer to keep the current names

###8 (descriptive of categorical variables: gender, educ, condition)
#gender
gender
summary(gender)
hist(gender)
table (gender)
#educ
educ
summary(educ)
hist(educ)
table(educ)
#condition
condition
summary(condition)
hist(condition)

###################end of week one

#exclude unattentive participant
#there was an attention check item that one participant failed.
#this participant is excluded from further analysis

table(preAttentionCheck_67)
main_attentive<-subset(main[preAttentionCheck_67!=1,])
str(main_attentive)
detach(main)
attach(main_attentive)

#describe sample (subset main_attentive)
table(gender)
table(educ)
summary(age)


#here I created subsets for the experimental conditions
#this was mainly to examine the subsamples on a descriptive level

main_attentive_condition1 <- subset(main_attentive,condition==1)
View(main_attentive_condition1)
main_attentive_condition2 <- subset(main_attentive,condition==2)
View(main_attentive_condition2)
main_attentive_condition4 <- subset(main_attentive,condition==4)
View(main_attentive_condition4)

library(lattice)

###check normality and skew for Avg_NegSelf_PC sclae (+ experimental groups)

shapiro.test(Avg_NegSelf_PC)      #nope
ks.test(Avg_NegSelf_PC, pnorm)    #nope
hist(Avg_NegSelf_PC)              #not too bad
qqmath(~Avg_NegSelf_PC | condition) #better 
shapiro.test(main_attentive_condition1$Avg_NegSelf_PC) #nope
shapiro.test(main_attentive_condition2$Avg_NegSelf_PC) #good
shapiro.test(main_attentive_condition4$Avg_NegSelf_PC) #nope
ks.test(main_attentive_condition1$Avg_NegSelf_PC, pnorm) #nope
ks.test(main_attentive_condition2$Avg_NegSelf_PC, pnorm) #nope
ks.test(main_attentive_condition4$Avg_NegSelf_PC, pnorm) #nope

#okay normality is not supported by inferential tests, but charts look okay
#skew:
tapply(Avg_NegSelf_PC, condition, skew)#looks good



#check normality for PosAtt scale (+ experimental groups)
shapiro.test(Avg_PosAtt_PC)        #nope
hist(Avg_PosAtt_PC)                #not great
qqmath(~Avg_PosAtt_PC | condition) #hmmyeah
shapiro.test(main_attentive_condition1$Avg_PosAtt_PC) #nope
shapiro.test(main_attentive_condition2$Avg_PosAtt_PC) #nope
shapiro.test(main_attentive_condition4$Avg_PosAtt_PC) #nope
ks.test(Avg_PosAtt_PC, pnorm)
ks.test(main_attentive_condition1$Avg_PosAtt_PC, pnorm) #nope
ks.test(main_attentive_condition2$Avg_PosAtt_PC, pnorm) #nope
ks.test(main_attentive_condition4$Avg_PosAtt_PC, pnorm) #nope

#okay normality is not supported by inferential tests, but charts look not too bad
#skew fragen:
tapply(Avg_PosAtt_PC, condition, skew)#looks good


#check normality for muslimrating sclae (+ experimental subgroups)
shapiro.test(Avg_MSLIM_RAT)      #nope
hist(Avg_MSLIM_RAT)              #kind of normal
qqmath(~Avg_MSLIM_RAT | condition)#not too bad
shapiro.test(main_attentive_condition1$Avg_MSLIM_RAT)#good
shapiro.test(main_attentive_condition2$Avg_MSLIM_RAT)#nope
shapiro.test(main_attentive_condition4$Avg_MSLIM_RAT)#nope
ks.test(Avg_MSLIM_RAT, pnorm)
ks.test(main_attentive_condition1$Avg_MSLIM_RAT, pnorm) #nope
ks.test(main_attentive_condition2$Avg_MSLIM_RAT, pnorm) #nope
ks.test(main_attentive_condition4$Avg_MSLIM_RAT, pnorm) #nope

#okay normality is not supported by inferential tests, but charts look not too bad
#skew fragen
tapply(Avg_MSLIM_RAT, condition, skew)#looks good

#####################start of week two
setwd("C:/Users/Hannes/Desktop/Courses ReMa/Applied Statistics")
source("wilcox_30.R.txt")

#outlier based on MAD procedure from wilcox paper
out(main_attentive_condition1$Avg_MSLIM_RAT)#no outliers 
out(main_attentive_condition2$Avg_MSLIM_RAT)#no outliers 
out(main_attentive_condition4$Avg_MSLIM_RAT)#no outliers 
out(main_attentive_condition1$Avg_PosAtt_PC)#no outliers
out(main_attentive_condition2$Avg_PosAtt_PC)#no outliers
out(main_attentive_condition4$Avg_PosAtt_PC)#no outliers
out(main_attentive_condition1$Avg_NegSelf_PC)#no outliers
out(main_attentive_condition2$Avg_NegSelf_PC)#no outliers
out(main_attentive_condition4$Avg_NegSelf_PC)#one outlier!!!!!!!!

main_attentive_condition4[2,]

#check for missing values in scales
is.na(Avg_MSLIM_RAT)
is.na(Avg_NegSelf_PC)
is.na(Avg_PosAtt_PC)
is.na(main_attentive)
complete.cases(main_attentive)
main_attentive[49,]

#only the age value for participant 49 is missing

#descriptive of Avg_NegSelf_PC
mean(Avg_NegSelf_PC)
tapply(Avg_NegSelf_PC, condition, mean)
median(Avg_NegSelf_PC)
tapply(Avg_NegSelf_PC, condition, median)
sd(Avg_NegSelf_PC)
tapply(Avg_NegSelf_PC, condition, sd)
boxplot(Avg_NegSelf_PC)
boxplot(Avg_NegSelf_PC~condition)
hist(Avg_NegSelf_PC)
par(mfcol=c(3,1))
hist(main_attentive_condition1$Avg_NegSelf_PC);hist(main_attentive_condition2$Avg_NegSelf_PC);hist(main_attentive_condition4$Avg_NegSelf_PC)
skew(Avg_NegSelf_PC)
tapply(Avg_NegSelf_PC, condition, skew)

###for report only
par(mfcol=c(1,2))
boxplot(Avg_NegSelf_PC); hist(Avg_NegSelf_PC)


#robust measures of Avg_NegSelf_PC
mean(Avg_NegSelf_PC)
mean(Avg_NegSelf_PC, trim=20)
winmean(Avg_NegSelf_PC)
tapply(Avg_NegSelf_PC, condition, mean)
tapply(Avg_NegSelf_PC, condition, mean, trim=20)
tapply(Avg_NegSelf_PC, condition, winmean)
library(chemometrics)
var(Avg_NegSelf_PC)
sd_trim(Avg_NegSelf_PC,trim=0.2)^2
winvar(Avg_NegSelf_PC)

#descriptive of Avg_PosAtt_PC
mean(Avg_PosAtt_PC)
tapply(Avg_PosAtt_PC, condition, mean)
median(Avg_PosAtt_PC)
tapply(Avg_PosAtt_PC, condition, median)
sd(Avg_PosAtt_PC)
tapply(Avg_PosAtt_PC, condition, sd)
boxplot(Avg_PosAtt_PC)
boxplot(Avg_PosAtt_PC~condition)
hist(Avg_PosAtt_PC)
par(mfcol=c(3,1))
hist(main_attentive_condition1$Avg_PosAtt_PC);hist(main_attentive_condition2$Avg_PosAtt_PC);hist(main_attentive_condition4$Avg_PosAtt_PC)
skew(Avg_PosAtt_PC)
tapply(Avg_PosAtt_PC, condition, skew)

###for report only
par(mfcol=c(1,2))
boxplot(Avg_PosAtt_PC); hist(Avg_PosAtt_PC)

#robust measures of Avg_PosAtt_PC
mean(Avg_PosAtt_PC)
mean(Avg_PosAtt_PC, trim=20)
winmean(Avg_PosAtt_PC)
tapply(Avg_PosAtt_PC, condition, mean)
tapply(Avg_PosAtt_PC, condition, mean, trim=20)
tapply(Avg_PosAtt_PC, condition, winmean)
var(Avg_PosAtt_PC)
sd_trim(Avg_PosAtt_PC,trim=0.2)^2
winvar(Avg_PosAtt_PC)

#descriptive of Avg_MSLIM_RAT
mean(Avg_MSLIM_RAT)
tapply(Avg_MSLIM_RAT, condition, mean)
median(Avg_MSLIM_RAT)
tapply(Avg_MSLIM_RAT, condition, median)
sd(Avg_MSLIM_RAT)
tapply(Avg_MSLIM_RAT, condition, sd)
boxplot(Avg_MSLIM_RAT)
boxplot(Avg_MSLIM_RAT~condition)
hist(Avg_MSLIM_RAT)
par(mfcol=c(3,1))
hist(main_attentive_condition1$Avg_MSLIM_RAT);hist(main_attentive_condition2$Avg_MSLIM_RAT);hist(main_attentive_condition4$Avg_MSLIM_RAT)
skew(Avg_MSLIM_RAT)
tapply(Avg_MSLIM_RAT, condition, skew)

###for report only
par(mfcol=c(1,2))
boxplot(Avg_MSLIM_RAT); hist(Avg_MSLIM_RAT)

#robust measures of Avg_MSLIM_RAT
mean(Avg_MSLIM_RAT)
mean(Avg_MSLIM_RAT, trim=20)
winmean(Avg_MSLIM_RAT)
tapply(Avg_MSLIM_RAT, condition, mean)
tapply(Avg_MSLIM_RAT, condition, mean, trim=20)
tapply(Avg_MSLIM_RAT, condition, winmean)
var(Avg_MSLIM_RAT)
sd_trim(Avg_MSLIM_RAT,trim=0.2)^2
winvar(Avg_MSLIM_RAT)

#correlations between the three scales
cor(cbind(Avg_NegSelf_PC, Avg_PosAtt_PC, Avg_MSLIM_RAT))


######### end of week 2 week two and descriptives

##create dummy variables for conditions

cond2_d<-ifelse(condition==2,1,0)
cond4_d<-ifelse(condition==4,1,0)

##center moderator scales

NegSelf_c<-Avg_NegSelf_PC-mean(Avg_NegSelf_PC)
PosAtt_c<-Avg_PosAtt_PC-mean(Avg_PosAtt_PC)

##include variables in main_attentive
main_attentive<-data.frame(cond2_d, cond4_d, NegSelf_c, PosAtt_c, main_attentive)
View(main_attentive)




###first multiple regression models

#Model 1
model1<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+NegSelf_c+cond2_d*NegSelf_c+cond4_d*NegSelf_c)
summary(model1)
confint(model1,level=0.95)

#Model 2
model2<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+PosAtt_c+cond2_d*PosAtt_c+cond4_d*PosAtt_c)
summary(model2)
confint (model2)

#main effects only model 1
model1M<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+NegSelf_c)
summary(model1M)
confint(model1M)

#main effects only model 2
model2M<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+PosAtt_c)
summary(model2M)
confint(model2M)

#integrated main effects model
modelIntM<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+NegSelf_c+PosAtt_c)
summary(modelIntM)

#fully integrated model
modelIntFull<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+NegSelf_c+PosAtt_c+cond2_d*NegSelf_c+cond4_d*NegSelf_c+cond2_d*PosAtt_c+cond4_d*PosAtt_c)
summary(modelIntFull)


#check for influential points in two models
cooks_NegSelf<-cooks.distance(model1)
cooks_NegSelf

cooks_PosAtt<-cooks.distance(model2)
cooks_PosAtt

resid_NegSelf<-residuals(model1)
resid_PosAtt<-residuals(model2)

main_attentive[146,]

###save cooks distance and residual scores in dataset
main_attentive<-data.frame(main_attentive, cooks_NegSelf, cooks_PosAtt, resid_NegSelf, resid_PosAtt)

plot(cooks_PosAtt,ylab="Cook's Distance")
plot(cooks_NegSelf,ylab="Cook's Distance")

which(cooks_PosAtt>0.05)
which(cooks_NegSelf>0.05)
#(cook's disance analysis is reported in the essay together with the assumptions testing)

###leverages and dffit values
summary(influence.measures(model1))
summary(influence.measures(model2))  


####test assumptions of multiple linear regression

## 0 independence of observations
## 1 linearity of the associations with dv and each iv
## 2 normality of residuals in dv 
## 3 homoscedaticity; error variance equal across levels of iv
## 5 multicollinearity

#I will rely mostly on graphical inspection and therefore use the following command
plot(model1)
plot(model2)

hist(resid_NegSelf)
hist(resid_PosAtt)

#create interaction variables to check for multicollinearity
NegAttXPrime2<-NegSelf_c*cond2_d
NegAttXPrime3<-NegSelf_c*cond4_d

PosAttXPrime2<-PosAtt_c*cond2_d
PosAttXPrime3<-PosAtt_c*cond4_d

#save interaction variables to main_attentive
main_attentive<-data.frame(main_attentive, NegAttXPrime2, NegAttXPrime3, PosAttXPrime2, PosAttXPrime3)

##check multicollinearity
#model1
cor(cbind(cond2_d, cond4_d, NegSelf_c, NegAttXPrime2, NegAttXPrime3))
#model2
cor(cbind(cond2_d, cond4_d, PosAtt_c, PosAttXPrime2, PosAttXPrime3))

#vif
library(car)
vif(model1)
vif(model2)

##clean up and create subset without outlier 146 for alternative regression

rm(cond2_d, cond4_d, cooks_NegSelf, cooks_PosAtt, NegAttXPrime2, NegAttXPrime3, NegSelf_c, PosAtt_c, PosAttXPrime2, PosAttXPrime3, resid_NegSelf, resid_PosAtt)

attach(main_attentive)

main_attentive_no146 <- main_attentive[which(cooks_NegSelf<0.1),]
dim(main_attentive_no146)


##compare models with and without participant 146

detach(main_attentive)
detach(main_attentive)
attach(main_attentive_no146)

#main effects model 1

model1aM<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+NegSelf_c)
summary(model1aM)

#full model 1

model1a<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+NegSelf_c+cond2_d*NegSelf_c+cond4_d*NegSelf_c)

summary(model1a)
summary(model1) 

#main effects model 2

model2aM<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+PosAtt_c)
summary(model2aM)

#full model 2

model2a<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+PosAtt_c+cond2_d*PosAtt_c+cond4_d*PosAtt_c)

summary(model2a)
summary(model2)

detach(main_attentive_no146)
attach(main_attentive)
###there seems to be a clear influence of the outlier in favour of the hypotheses

search()
ls(main_attentive)

##examine residuals of two models
hist(resid_NegSelf)
hist(resid_PosAtt)
kurtosi(resid_NegSelf)
kurtosi(resid_PosAtt)
skew(resid_NegSelf)
skew(resid_PosAtt)



#######week 3
#a lot of the steps in week 3 labguide are not applicable or not central for my research project

#1 there is only one missing value in my sample; therefore this step is skipped
#2 --
#3 here I will compare the experimental groups in regard to the split of demographic groups
table(gender, condition)
##gender looks good
boxplot(age~condition)
##age looks good
table(educ, condition)
##education looks reasonably good

#influence of demographic variables on DV
agemodel<-lm(Avg_MSLIM_RAT~age)
cor(na.omit(cbind(age,Avg_MSLIM_RAT)))
summary(agemodel)
##age has strong influence

t.test(Avg_MSLIM_RAT~gender)
##gender has no influence

##educ
educmodel<-aov(Avg_MSLIM_RAT~educ)
summary(educmodel)

boxplot(Avg_MSLIM_RAT~educ)

summary(educ)
table(educ)

main_attentive_edu <- main_attentive[which(educ!=5),]
main_attentive_educ <- main_attentive_edu[which(educ!=11),]
educ1model1<-aov(main_attentive_educ$Avg_MSLIM_RAT~main_attentive_educ$educ)
summary(educmodel1)

######### from here on I do not divide different part of the script into weekly sections
######### since the labguide has some overlap between weeks, not all tasks make sense for me
######### and most of the steps that need to be done stem from personal reasoning and not 
######### from the labguide

####check SE for kurtosis for residuals
SE.kurt<-function(x) {
n<- length(x[which(!is.na(x))])
SES<-sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3)))
SEK<-2*SES*sqrt((n^2-1)/((n-3)*(n+5)))
return(SEK)
} 


SE.kurt(resid_PosAtt)
SE.kurt(resid_NegSelf)

###main graph with 3 lines
###interaction effects in  both models 

library(ggplot2)
cleanup=theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_blank(), axis.line=element_line(color="black"))

#Model 1
model1plot<-ggplot(main_attentive_no146,aes(NegSelf_c, Avg_MSLIM_RAT, color=conditionf))
model1plot+geom_point()+geom_smooth(method="lm",se=FALSE,aes(fill=conditionf))+xlab("Negative PC Attitude")+ylab("Muslim Rating")+cleanup+scale_fill_manual(name="Condition",labels=c("Control","Neutral PC Prime", "Aggressive PC Prime"), values=c("black","blue","red"))+scale_color_manual(name="Condition",labels=c("Control","Neutral PC Prime", "Aggressive PC Prime"), values=c("black","blue","red"))

#Model 2
model1plot<-ggplot(main_attentive_no146,aes(PosAtt_c, Avg_MSLIM_RAT, color=conditionf))
model1plot+geom_point()+geom_smooth(method="lm",se=FALSE,aes(fill=conditionf))+xlab("Positive PC Attitude")+ylab("Muslim Rating")+cleanup+scale_fill_manual(name="Condition",labels=c("Control","Neutral PC Prime", "Aggressive PC Prime"), values=c("black","blue","red"))+scale_color_manual(name="Condition",labels=c("Control","Neutral PC Prime", "Aggressive PC Prime"), values=c("black","blue","red"))

detach(main_attentive)
detach(main_attentive)
attach(main_attentive_no146)

#inspect age effects in estimated models by integrating the variable age

model1age<-lm(Avg_MSLIM_RAT~age+cond2_d+cond4_d+NegSelf_c+cond2_d*NegSelf_c+cond4_d*NegSelf_c,na.action=na.omit)
summary(model1age)

model2age<-lm(Avg_MSLIM_RAT~cond2_d+cond4_d+PosAtt_c+cond2_d*PosAtt_c+cond4_d*PosAtt_c)
summary(model2age)
detach(main_attentive_no146)

###check if drop outs are MCAR MAR or MNAR
###the prestudy dataset is attached and means between dropouts and participants are compared
###on the variables PosAtt and NegSelf

search()

library(foreign)
Prestudy=read.spss(file="PC3_allvariables merged without workderid duplicates.sav", to.data.frame=TRUE)
search()
names(Prestudy)
attach(Prestudy)
prop.table(table(Pre_NegSelf, Ind_Post),2)
mean(Pre_NegSelf[Ind_Post==0])
mean(Pre_PosAtt[Ind_Post==0])
boxplot(Pre_NegSelf~Ind_Post)
boxplot(Pre_PosAtt~Ind_Post)
dim(Prestudy)

###it looks as if there is no strong difference between dropouts and participants
###hence, there is no reason at this point to assume that the missing data are mnar or mar
###I decide to not impute data for the dropouts

