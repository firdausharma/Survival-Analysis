library("survival")
library("survminer")
library("KMsurv")
library("dplyr")
library("MASS")
library("eha")
library("fitdistrplus")
library("nortest")

data = read.delim("clipboard")
data

#uji deskriptif
library("psych")
describe(data)

#uji model
res.cox <- coxph(Surv(time, status) ~ x1+x2+IPK+x4+x5+x6, data = data)
res.cox
summary(res.cox)


#Uji Asumsi
test.ph = cox.zph(res.cox)
test.ph

#Uji Survival Kaplan Meier
fit0 <- survfit(formula = Surv(time, status) ~ 1, data = data)
summary(fit0)
ggsurvplot(fit0, pval = FALSE, pval.method = FALSE,
           title = 'Kurva Survival Kaplan Meier', data = data)

ggsurvplot(fit0, data = data)
ggsurvplot(fit0, data = data, fun = "event")
ggsurvplot(fit0, data = data, fun = "cumhaz")

#Berdasarkan Jenis Kelamin
fit1 <- survfit(formula = Surv(time, status) ~ x1, data = data)
summary(fit1)
ggsurvplot(fit1, pval = FALSE, pval.method = FALSE,
           title = 'Kurva Survival Kaplan Meier Berdasarkan Jenis Kelamin', data = data)

#Berdasarkan Status
fit2 <- survfit(formula = Surv(time, status) ~ x2, data = data)
summary(fit2)
ggsurvplot(fit2, pval = FALSE, pval.method = FALSE,
           title = 'Kurva Survival Kaplan Meier Berdasarkan Status', data = data)

#Berdasarkan waktu lulus
fit3 <- survfit(formula = Surv(time, status) ~ x3, data = data)
summary(fit3)
ggsurvplot(fit3, pval = FALSE, pval.method = FALSE,
           title = 'Kurva Survival Kaplan Meier Berdasarkan Waktu Lulus', data = data)

#Berdasarkan IPK
fit4 <- survfit(formula = Surv(time, status) ~ IPK, data = data)
summary(fit4)
ggsurvplot(fit4, pval = FALSE, pval.method = FALSE,
           title = 'Kurva Survival Kaplan Meier Berdasarkan IPK', data = data)

#Berdasarkan Aktif Organisasi
fit5 <- survfit(formula = Surv(time, status) ~ x5, data = data)
summary(fit5)
ggsurvplot(fit5, pval = FALSE, pval.method = FALSE,
           title = 'Kurva Survival Kaplan Meier Berdasarkan Aktif Organisasi', data = data)

#Berdasarkan Ikut Kursus
fit6 <- survfit(formula = Surv(time, status) ~ x6, data = data)
summary(fit6)
ggsurvplot(fit6, pval = FALSE, pval.method = FALSE,
           title = 'Kurva Survival Kaplan Meier Berdasarkan Ikut Kursus', data = data)

#Stratification Cox
scph <- coxph(formula = Surv(time, status) ~ x1+x2+IPK+x4+x6+strata(x5),data = data)
summary(scph)



