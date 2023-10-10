dataset = read.csv("dhs_24.csv")
attach(dataset)

## exercise 1

#b

reg1 = lm(log(1+exinc) ~ age+ I(age^2)+ nmembers+ nkids+ college+ working)
summary(reg1)
library(car)
lht(reg1, c("nkids=0", "nmembers=0"), test = "Chisq")

#c 

lht(reg1, c("nkids=nmembers"), test = "Chisq")

## exercise 2

#a

library(lmtest)
resettest(reg1, power = 2:3, test = "fitted")

#b

reg2 = lm(log(1+exinc) ~ age+ I(age^2)+ nmembers+ nkids+ college+ working+ age:gender+ gender:I(age^2)+ gender:nmembers+ gender:nkids+ gender:college+ gender:working+ gender) 
summary(reg2)
lht(reg2, c("gender =0", "age:gender =0", "I(age^2):gender =0", "nmembers:gender =0", "nkids:gender =0","college:gender =0", "working:gender=0"), test = "Chisq")

#c

y.hat=predict(reg1)
res = residuals(reg1)
reg.res = lm(I(res^2) ~ y.hat+ I(y.hat^2))
summary(reg.res)
summary(reg.res)$r.square*nrow(dataset)

## exercise 3

#a

library(truncreg)
reg.trunc = truncreg(log(1+exinc) ~ age+ I(age^2)+ nmembers+ nkids+ college+ working, ll = 0)
summary(reg.trunc)

#b

detach(dataset)
dataset$dummy = dataset$exinc > 0
attach(dataset)

library(VGAM)
library(sampleSelection)
reg3 = heckit(dummy ~ age+ I(age^2)+ nmembers+ nkids+ college+ working, log(1+exinc) ~ age+ I(age^2)+ nmembers+ nkids+ college+ working)
summary(reg3)

#c

detach(dataset)
dataset$highex = dataset$exinc > dataset$inc 
attach(dataset)
reg.probit = glm(highex ~ age+ I(age^2)+ nmembers+ nkids+ college+ working, family = binomial(link = "probit"))
summary(reg.probit)
library(margins)
m.prob = margins(reg.probit)
summary(m.prob)

#d

library(DescTools)
PseudoR2(reg.probit)
CountR2=function(m) mean(m$y==round(m$fitted.values))
CountR2(reg.probit)

## exercise 4

#a

detach(dataset)
library(plm)
dataset$y2012 = dataset$year == 2012
dataset$y2013 = dataset$year == 2013 
dataset$y2014 = dataset$year == 2014
dataset$y2015 = dataset$year == 2015
dataset$y2016 = dataset$year == 2016
dataset$y2017 = dataset$year == 2017
dataset$y2018 = dataset$year == 2018
dataset$y2019 = dataset$year == 2019
dataset$y2020 = dataset$year == 2020

dataset$y2012 [dataset$y2012=="true"] = 1
dataset$y2013 [dataset$y2013=="true"] = 1
dataset$y2014 [dataset$y2014=="true"] = 1
dataset$y2015 [dataset$y2015=="true"] = 1
dataset$y2016 [dataset$y2016=="true"] = 1
dataset$y2017 [dataset$y2017=="true"] = 1
dataset$y2018 [dataset$y2018=="true"] = 1
dataset$y2019 [dataset$y2019=="true"] = 1
dataset$y2020 [dataset$y2020=="true"] = 1

dataset$y2012 [dataset$y2012=="false"] = 0
dataset$y2013 [dataset$y2013=="false"] = 0
dataset$y2014 [dataset$y2014=="false"] = 0
dataset$y2015 [dataset$y2015=="false"] = 0
dataset$y2016 [dataset$y2016=="false"] = 0
dataset$y2017 [dataset$y2017=="false"] = 0
dataset$y2018 [dataset$y2018=="false"] = 0
dataset$y2019 [dataset$y2019=="false"] = 0
dataset$y2020 [dataset$y2020=="false"] = 0
attach(dataset)
reg.pooled = plm(log(1+exinc) ~ age+ I(age^2)+ nmembers+ nkids+ college+ working+ y2012+ y2013+ y2014+ y2015+ y2016+ y2017+ y2018+ y2019+ y2020,data = dataset, index = c("hhid","year"), model = "pooling")
summary(reg.pooled)
coeftest(reg.pooled, vcov=vcovHC(reg.pooled, type = "sss", cluster= "group"))

#b

reg.random = plm(log(1+exinc) ~ age+ I(age^2)+ nmembers+ nkids+ college+ working+ y2012+ y2013+ y2014+ y2015+ y2016+ y2017+ y2018+ y2019+ y2020,data = dataset, index = c("hhid","year"), model = "random")
summary(reg.random)
reg.fixed = plm(log(1+exinc) ~ age+ I(age^2)+ nmembers+ nkids+ college+ working+ y2012+ y2013+ y2014+ y2015+ y2016+ y2017+ y2018+ y2019+ y2020,data = dataset, index = c("hhid","year"), model = "within")
summary(reg.fixed)
summary(reg.random)$r.square
summary(reg.fixed)$r.square

#c

## FE vs PE  Chow Test

pFtest(reg.fixed, reg.pooled)

## Breusch-Pagan RE vs PE

plmtest(reg.random, type = c("bp"))

## Hausman Test FE vs RE

phtest(reg.fixed, reg.random)




