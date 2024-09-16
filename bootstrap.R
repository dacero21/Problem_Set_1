

gas<-read.csv("https://raw.githubusercontent.com/ignaciomsarmiento/datasets/main/gas.csv",header=T)
head(gas)

require("pacman")
p_load("tidyverse","stargazer")
library(boot)


mod1<- lm(consumption~price+income,gas)
stargazer(mod1,type="text", omit.stat=c("ser","f","adj.rsq"))

str(mod1)

mod1$coefficients

round(mod1$coefficients[2],3)

B<-1000 # Number of Repetions()

eta_mod1<-rep(NA,B)#this is an empty vector where we are going to save our elasticity estimates

eta_fn<-function(data,index){
  
  coef(lm(consumption~price+income, data = data, subset = index))[2] #returns the second coefficient of the linear regression
}

eta_fn(gas,1:nrow(gas))

set.seed(123)
#call the boot function
x = boot(gas, eta_fn, R = 10000)



