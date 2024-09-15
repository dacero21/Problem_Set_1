require("pacman")

p_load("tidyverse","rio","stargazer")

auto <- import("https://www.stata-press.com/data/r17/auto.dta")

auto<- auto %>% mutate(mpg=as.numeric(mpg),
                       weight=as.numeric(weight),
                       foreign=as.numeric(foreign),
                       mpg=ifelse(foreign==1,mpg+8,mpg))

reg1<-lm(mpg ~ foreign + weight, data =auto)
stargazer(reg1,type="text",digits=7)

auto <- auto %>% 
  mutate(weightResidF=lm(weight~foreign,auto)$residuals) #Residuals of weight~foreign 

auto <- auto %>% 
  mutate(mpgResidF=lm(mpg~foreign,auto)$residuals) #Residuals of mpg~foreign 

reg2<-lm(mpgResidF~weightResidF,auto)
stargazer(reg1,reg2,type="text",digits=7) # with stargazer we can visualize the coefficients next to each other
