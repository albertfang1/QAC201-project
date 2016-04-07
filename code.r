load("/Volumes/qac201/Studies and Codebooks/AddHealth/Data/Wave 4/addhealth_public4.rdata")

myVars <- c("h4wp23", "h4wp24", "h4wp37", "h4wp38", "h4se1", "h4se2", "bio_sex")
#add variables from personal codebook

addhealth <- addhealth_public4[myVars]

rm(addhealth_public4)
#remove old dataset

install.packages("descr")
install.packages("ggplot2")
#installing outside functions which can maybe do the job better
library(descr)
library(ggplot2)

addhealth$h4wp23[addhealth$h4wp23== 6 ] <- NA
addhealth$h4wp23[addhealth$h4wp23== 7 ] <- NA
addhealth$h4wp23[addhealth$h4wp23== 8 ] <- NA

addhealth$h4wp24[addhealth$h4wp24== 7 ] <- NA
addhealth$h4wp24[addhealth$h4wp24== 8 ] <- NA

addhealth$h4wp37[addhealth$h4wp37== 6 ] <- NA
addhealth$h4wp37[addhealth$h4wp37== 7 ] <- NA
addhealth$h4wp37[addhealth$h4wp37== 8 ] <- NA

addhealth$h4wp38[addhealth$h4wp38== 6 ] <- NA
addhealth$h4wp38[addhealth$h4wp38== 7 ] <- NA
addhealth$h4wp38[addhealth$h4wp38== 8 ] <- NA

addhealth$h4se1[addhealth$h4se1== 6 ] <- NA
addhealth$h4se1[addhealth$h4se1== 7 ] <- NA
addhealth$h4se1[addhealth$h4se1== 8 ] <- NA

addhealth$h4se2[addhealth$h4se2== 6 ] <- NA
addhealth$h4se2[addhealth$h4se2== 7 ] <- NA
addhealth$h4se2[addhealth$h4se2== 8 ] <- NA
#aggregate responses

addhealth$bio_sex[addhealth$bio_sex== 6 ] <- NA
#assign answers as missing data and display frequency tables

addhealth$h4se2[addhealth$h4se2==1|addhealth$h4se2==2|addhealth$h4se2==3|addhealth$h4se2==4]<-1

table(addhealth$h4wp23)
table(addhealth$h4wp24)
table(addhealth$h4wp37)
table(addhealth$h4wp38)
table(addhealth$h4se1)
table(addhealth$h4se2)
table(addhealth$suicide)

addhealth$suicide<-rep(NA, nrow(addhealth))
addhealth$suicide[addhealth$h4se1==1|addhealth$h4se2==1]<-1
addhealth$suicide[addhealth$h4se1==0|addhealth$h4se2==0]<-0
#collapse responses

prop.table(table(addhealth$h4se1,addhealth$h4wp23),2)
prop.table(table(addhealth$h4se1,addhealth$h4wp37),2)
barplot(prop.table(table(addhealth$h4se1,addhealth$h4wp23),2)[2,], xlab="Satisfaction with Mother Figures", ylab="Suicide Attempts", main="Satisfaction with Mother Figures with Suicide Attempts", col=c("darkblue"))
barplot(prop.table(table(addhealth$h4se1,addhealth$h4wp37),2)[2,], xlab="satisfaction with Father Figures", ylab="Suicide Attempts", main="Satisfaction with Mother Figures with Suicide Attempts", col=c("red"))
#barplot

freq(as.ordered(addhealth$h4wp23))
freq(as.ordered(addhealth$h4wp37))
#univariate

myChi <- chisq.test(addhealth$suicide, addhealth$h4wp23)
myChi
myChi$observed
prop.table(myChi$observed,2)
prop.table(myChi$observed,1)

myChi <- chisq.test(addhealth$suicide, addhealth$h4wp37)
myChi
myChi$observed
prop.table(myChi$observed,2)
prop.table(myChi$observed,1)
#categorical explanatory and response variables
#Chi Squared Test 

by(addhealth, addhealth$bio_sex, function(x) list(fisher.test(x$suicide, x$h4wp23), fisher.test(x$suicide, x$h4wp23)$observed))
by(addhealth, addhealth$bio_sex, function(x) list(fisher.test(x$suicide, x$h4wp24), fisher.test(x$suicide, x$h4wp24)$observed))
#moderation

addhealth$h4wp23 <- as.factor(addhealth$h4wp23)
#factor categroical variable as numerical

my.logreg <- glm(suicide ~ h4wp23, data=addhealth, family="binomial")
summary(my.logreg)
exp(my.logreg$coefficients)
exp(confint(my.logreg))
#logistic regression before adding bio_sex

my.logreg <- glm(suicide ~ h4wp23 + bio_sex, data=addhealth, family="binomial")
summary(my.logreg)
exp(my.logreg$coefficients)
exp(confint(my.logreg))
#communication with mother figure
my.logreg <- glm(suicide ~ h4wp37 + bio_sex, data=addhealth, family="binomial")
summary(my.logreg)
exp(my.logreg$coefficients)
exp(confint(my.logreg))
#communication with father figure
#logistic regression after adding bio_sex

my.lm <- lm(h4wp23 ~ suicide + bio_sex, data=addhealth)
summary(my.lm)
#confounding

my.lm <- lm(h4wp37 ~ suicide + bio_sex, data=addhealth)
summary(my.lm)
#confounding 

tab2 <- ftable(addhealth$suicide, addhealth$h4wp23, addhealth$bio_sex)
tab2
tab2_colProp <- prop.table(table2,2)
tab2_colProp

