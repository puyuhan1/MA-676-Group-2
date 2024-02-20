set.seed(123)
n = 1000
age = rnorm(n,0,1)
bmi = 5*age + rnorm(n,0,1)
str = rbinom(n=n,prob = plogis(age + bmi),size=1)
td  = rnorm(n, 10*age + bmi + 5*str)

dat = data.frame(td,age,str,bmi)

mod_td  = lm('td~age+str+bmi',dat)
mod_bmi = lm('bmi~age',dat)
mod_str = glm('str~bmi+age',dat,family='binomial')


# TE=E[Y(1)-Y(0)]

dat_te_1 = dat
dat_te_0 = dat
dat_te_1$bmi = quantile(dat$bmi,0.975)
dat_te_0$bmi = quantile(dat$bmi,0.025)

dat_te_me = dat[c('bmi','age')]
dat_te_me$bmi = quantile(dat$bmi,0.975)
dat_te_1$str = rbinom(n=n,prob = predict(mod_str,dat_te_me,type='response'),size=1)
dat_te_me$bmi = quantile(dat$bmi,0.025)
dat_te_0$str = rbinom(n=n,prob = predict(mod_str,dat_te_me,type='response'),size=1)

TE = mean(predict(mod_td,dat_te_1)-predict(mod_td,dat_te_0))


# CDE(m)=E[Y(1,m)-Y(0,m)]

dat_cde_1 = dat
dat_cde_0 = dat
dat_cde_1$bmi = quantile(dat$bmi,0.975)
dat_cde_0$bmi = quantile(dat$bmi,0.025)
dat_cde_1$str = 1
dat_cde_0$str = 1
CDE = mean(predict(mod_td,dat_cde_1)-predict(mod_td,dat_cde_0))


# NDE=E[Y(1,M(0))-Y(0,M(0))]

dat_nde_1 = dat
dat_nde_0 = dat
dat_nde_1$bmi = quantile(dat$bmi,0.975)
dat_nde_0$bmi = quantile(dat$bmi,0.025)
dat_nde_me = dat[c('bmi','age')]
dat_nde_me$bmi = quantile(dat$bmi,0.025)
dat_nde_1$str = rbinom(n=n,prob = predict(mod_str,dat_nde_me,type='response'),size=1)
dat_nde_0$str = rbinom(n=n,prob = predict(mod_str,dat_nde_me,type='response'),size=1)
NDE = mean(predict(mod_td,dat_nde_1)-predict(mod_td,dat_nde_0))


# NIE=E[Y(0,M(1))-Y(0,M(0))]
dat_nie_1 = dat
dat_nie_0 = dat
dat_nie_1$bmi = quantile(dat$bmi,0.025)
dat_nie_0$bmi = quantile(dat$bmi,0.025)

dat_nie_me = dat[c('bmi','age')]
dat_nie_me$bmi = quantile(dat$bmi,0.975)
dat_nie_1$str = rbinom(n=n,prob = predict(mod_str,dat_nie_me,type='response'),size=1)

dat_nie_me$bmi = quantile(dat$bmi,0.025)
dat_nie_0$str = rbinom(n=n,prob = predict(mod_str,dat_nie_me,type='response'),size=1)

NIE = mean(predict(mod_td,dat_nie_1)-predict(mod_td,dat_nie_0))

cat("TE: ",TE, "\nCDE: ",CDE,"\nNDE: ",NDE, "\nNIE: ", NIE)


library(mediation)
med_model<-glm(str~bmi,family="binomial",data=dat)
out_model<-lm(td~bmi+age+str,data=dat)
result<-mediate(med_model,out_model,treat="bmi",mediator="str", boot=TRUE, sims=500,control.value = quantile(dat$bmi,0.025), treat.value = quantile(dat$bmi,0.975))
summary(result)