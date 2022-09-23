library(FactoMineR)
library(Rcpp)
library(car)
library(MASS)
library(rgl)
library(olsrr)
library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation
library(corrplot)
library(lattice)
library(plot.matrix)
library(ggplot2)
library(insight)
library(lattice)
library(lme4)

load("HF.Rdata")
heartf=HF.2006_2012
remove(HF.2006_2012)

singoli_osp <- read.table('singoli_osp.txt', header=T) #senza factor
singoli_osp2 <- read.table('singoli_osp2.txt', header=T) #senza factor
milano <- read.table('milano.txt', header=T) 
mil_red <- read.table('mil_red.txt', header=T) 
only_cat <- read.table('only_cat.txt', header=T) #senza factor
day_deaths <- read.table('day_deaths.txt', header=T) #senza factor
Dead_como <- read.table('Dead_como.txt', header=T) #senza factor

heartf_osp=heartf[heartf$tipo_prest==41,]  # dataset con solo le ospedalizzazioni
heartf_pres=heartf[heartf$tipo_prest==30,c(1:11)]  # dataset con solo le prestazioni

milano=singoli_osp[which(singoli_osp$ASL_RESIDENZA=='308'),]
mil=singoli_osp[which(singoli_osp$ASL_RESIDENZA=='308'),13:32]

load("Cluster_Milano.RData") #contiene cluster.mc, gia in milano

###### MCA ######
for (i in 1:20){
  only_cat[,i] <- factor(only_cat[,i])
}

x11()
res.mca=MCA(only_cat, ncp = 5, ind.sup = NULL, quanti.sup = NULL,quali.sup =NULL,
            excl=NULL, graph = TRUE,level.ventil = 0, axes = c(1,2), row.w = NULL,
            method="Indicator", na.method="NA", tab.disj=NULL)
summary(res.mca)
###### Analisi effetto comorbodità sulla mortalità ######
# dovremmo provare con .05
morti=singoli_osp[singoli_osp$dead==1,13:36]
vivi=singoli_osp[singoli_osp$dead==0,13:36]

n1 <- dim(morti)[1]
n2 <- dim(vivi)[1]
p <- dim(vivi)[2]

x.mean1 <- sapply(morti, mean)
x.mean2 <- sapply(vivi, mean)

p.hat <- (x.mean1*n1+x.mean2*n2)/(n1+n2)
x.var <- (p.hat*(1-p.hat))

alpha=0.01
# Test: H0.i: mu.i1 == mu.i2  vs H1.i: mu.i1 != mu.i2

z.i <- (x.mean1-x.mean2)/sqrt(x.var*(1/n1+1/n2))
p.i <- ifelse(z.i<0, 2*pnorm(z.i),2*(1-pnorm(z.i)))

which(p.i<alpha)

# Bonferoni test
k=p

which(p.i*k<alpha)  

# or
p.Bf <- p.adjust(p.i, method='bonferroni')

which(p.Bf<alpha)  

# Benjamini-Hockberg (control the false discovery rate)  
p.BH <- p.adjust(p.i, method='BH')

which(p.BH<alpha)


x11(width=21, height=7)
par(mfrow=c(1,3))
plot(p.i, main='Univariate')
abline(h=alpha, lwd=2, col='red')

plot(p.Bf, main='Corrected - Bonferroni')
abline(h=alpha, lwd=2, col='red')

plot(p.BH, main='Corrected - BH')
abline(h=alpha, lwd=2, col='red')


#notiamo che la presenza di quasi tutte le malattie e di tutti i dispositivi medici hanno influenza significativa 
# sui pazienti deceduti e quelli no
###### Analisi Esplorativa ######
#Istogram of ages with overline boxplot
x11()
dev.off()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(1,8))
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(singoli_osp$eta_Min , horizontal=TRUE, main="Patient age at first ospedalization", xaxt="n" , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(singoli_osp$eta_Min, breaks = 40, main="" , xlab="Age") #  border=F , 

# quantile(singoli_osp$eta_Min) # 18   71   79   85  110 years
# mean(singoli_osp$eta_Min)  # 77.16838

#Istogram of ages with overline boxplot and colours by group
dev.off()
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(1,8))
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(singoli_osp$eta_Min , horizontal=TRUE, main="Patient age at first ospedalization", xaxt="n" , frame=F)
my_hist = hist(singoli_osp$eta_Min, col="white", breaks = 40, main="" , xlab="Age", plot=F) #  border=F , 
my_color= ifelse (my_hist$breaks < 60, rgb(0,1,0,0.5) , ifelse (my_hist$breaks >= 80, "purple", rgb(0.2,0.2,0.2,0.2) ))
par(mar=c(4, 3.1, 1.1, 2.1))
plot(my_hist, col=my_color , main="" , xlab="Age" )

# Boxplot sesso vs eta
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(mfrow = c(1,1))
dev.off()
boxplot(singoli_osp$eta_Min ~ singoli_osp$SESSO, col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), xlab = "Gender", ylab="First ospedalization age")

#Boxplot sesso vs num_comor
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(mfrow = c(1,1))
boxplot(singoli_osp$num_comor ~ singoli_osp$SESSO, col = c("pink", "deepskyblue1"))



#Histograms ages by sesso
dev.off()
males = singoli_osp[singoli_osp$SESSO == "M",]$eta_Min
n_male = length(males)    # 92249
females = singoli_osp[singoli_osp$SESSO == "F",]$eta_Min
n_female = length(females)  # 95244
hist(females,  breaks = 40, col=rgb(1,0,0,0.5), xlab="Age", 
     ylab="Frequency", main="Distribution of ages by sex" )

# Second with add=T to plot on top
hist(males,  breaks = 40, col=rgb(0,0,1,0.5), add=T)

# Add legend
legend("topright", legend=c("Females","Males"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )



#Multiple boxplots ospedalization by age

a = singoli_osp[singoli_osp$eta_Min <= 40,]$ospitalizations
b = singoli_osp[singoli_osp$eta_Min > 40 & singoli_osp$eta_Min <= 60,]$ospitalizations
c = singoli_osp[singoli_osp$eta_Min > 60 & singoli_osp$eta_Min <= 70,]$ospitalizations
d = singoli_osp[singoli_osp$eta_Min > 70 & singoli_osp$eta_Min <= 80,]$ospitalizations
e = singoli_osp[singoli_osp$eta_Min > 80 & singoli_osp$eta_Min <= 90,]$ospitalizations
f = singoli_osp[singoli_osp$eta_Min > 90,]$ospitalizations

ve = list(a,b,c, d, e, f)

x.mean=rbind(c(0,0,0,0,0,0), c(0,0,0,0,0,0))

for (i in 1:6) {
  x.mean[1,i]=sapply(ve[i],mean)
  x.mean[2,i]=sapply(ve[i],sd)
}

T2 = cbind(c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0))

for (i in 1:6) {
  T2[i,1] = x.mean[1,i] - x.mean[2,i]
  T2[i,2] = x.mean[1,i] 
  T2[i,3] = x.mean[1,i] + x.mean[2,i]
}
aa = mean(a)
bb = mean(b)
cc = mean(c)
dd = mean(d)
ee = mean(e)
ff = mean(f)

aa_cov = var(a)
bb_cov = var(b)
cc_cov = var(c)
dd_cov = var(d)
ee_cov = var(e)
ff_cov = var(f)

dev.off()
x11()
matplot(1:6,1:6,pch='',ylim=c(0,7), xlab='Ages',ylab='Ospedalization',
        main='Numer of ospedalizations by age range') 
for(i in 1:6){
  points(1:6, T2[,2], pch=16, col=1:6)
  segments(i,T2[i,1],i,T2[i,3],lwd=3,col=i) 
}

##### Clustering####
singoli_osp3=singoli_osp2[which(singoli_osp2$COD_REG %in% id_Same_osp),]
# prendo quelli che non cambiano mai ospedale
milano=singoli_osp3[which(singoli_osp3$ASL_RESIDENZA=='308'),]
mil=milano[,13:32]

mil.m <- dist(as.matrix(mil), method='minkowski')
mil.mc <- hclust(mil.m, method='ward.D2')
plot(mil.mc, main='euclidian-ward.D2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(mil.mc, k=6)
cluster.mc <- cutree(mil.mc, k=6)
table(cluster.mc)
cl_table=table(cluster.mc)

coph.mc <- cophenetic(mil.mc) # compute the cophenetic matrices
mc <- cor(mil.m, coph.mc) # cophenetic coefficients, se vicino a |1| è meglio

##### Regressione logistica con patient level frature ####
milano <- read.table('milano.txt', header=T)
milano_logistica=milano[,1:43]
milano_logistica$anno1 = 0
milano_logistica$anno3 = 0
milano_logistica$anno5 = 0

# see if patient died in 1,3,5 years
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$anno1 = ifelse(as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),5])-as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),2])>364,0,1)
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$anno3 = ifelse(as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),5])-as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),2])>1094,0,1)
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$anno5 = ifelse(as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),5])-as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),2])>1824,0,1)

# keep all patient died or alive after 5 years of study
milano_logistica$tmp = 0
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$tmp = 1
milano_logistica[as.Date(milano_logistica[,5])-as.Date(milano_logistica[,2])>364,]$tmp = 1
df_aspettativa = milano_logistica[milano_logistica$tmp == 1,]
df_aspettativa$tmp = NULL

#Anno 1
id_test = c(3,11,33:36,43:44)
colnames(df_aspettativa)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = df_aspettativa[,id_test]
df_test$cluster=as.factor(df_test$cluster)
fit1 <- glm(anno1 ~ ., data= df_test, family='binomial')
summary(fit1)

## Analisi modello ## 
library(ROCR)
p <- predict(fit1, df_test, type='response')
pr <- prediction(p, df_test$anno1)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
x11()
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.7369479  -->0.7650424

for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno1 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.5
  print(APER) # 0.2938179
}

trsh = 0.5
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno1, class.assigned=predictions)
#         class.assigned
# class.true     0     1
#          0 9065 1346
#          1 3440 2438
errorsq = (df_test$anno1 != predictions)
APER   = sum(errorsq)/length(predictions)
print(APER) # 0.3195408  __>0.2938179

Z0.new <- data.frame(cluster = as.factor(2), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0) # questo è il nuovo dato
Z1.new <- data.frame(cluster = as.factor(3), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0) # questo è il nuovo dato
p0 <- predict(fit1, Z0.new, type='response')
p0
p1 <- predict(fit1, Z1.new, type='response')
p1

#Anno 3
id_test = c(3,11,43,45)
colnames(milano_logistica)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = milano_logistica[,id_test]
df_test$cluster=as.factor(df_test$cluster)
fit3 <- glm(anno3 ~ ., data= df_test, family='binomial')
summary(fit3)

## Analisi modello ## 
library(ROCR)
p <- predict(fit3, df_test, type='response')
pr <- prediction(p, df_test$anno3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.7760562    -->0.7454778

for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno3 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.3-->0.5
  print(APER) # 0.2518628-->0.3157345
}

trsh = 0.5
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno3, class.assigned=predictions)
#         class.assigned
# class.true     0     1
#          0 13882 20090
#          1  7222 67246

errorsq = (df_test$anno3 != predictions)
APER   = sum(errorsq)/length(predictions)
print(APER) # 0.2518628-->0.3157345


#Anno 5
id_test = c(3,11,43,46)
colnames(milano_logistica)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = milano_logistica[,id_test]
df_test$cluster=as.factor(df_test$cluster)
fit5 <- glm(anno5 ~ ., data= df_test, family='binomial')
summary(fit5)

## Analisi modello ## 
library(ROCR)
p <- predict(fit5, df_test, type='response')
pr <- prediction(p, df_test$anno5)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.8121018


for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno5 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.2
  print(APER) # 0.1896717
}

trsh = 0.2
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno5, class.assigned=predictions)
#         class.assigned
# class.true     0     1
#          0  6440 17448
#          1  3120 81432


id_test = c(3,11,12,33:36,43:44)
colnames(df_aspettativa)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = df_aspettativa[,id_test]
df_test$cluster=as.factor(df_test$cluster)
log_m_model <- glmer(anno1 ~ eta_Min + SESSO +ICD+SHOCK+CABG+PTCA+ cluster+ (1|strutt_id) , data = df_test, nAGQ=0,
                     family = "binomial")

summary(log_m_model)
# vif(log_m_model)
sigma2_eps <- as.numeric(get_variance_residual(log_m_model))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(log_m_model))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
x11()
dotplot(ranef(log_m_model))

sort(ranef(log_m_model)$strutt_id[,1])
ranef(log_m_model)$strutt_id[which(ranef(log_m_model)$strutt_id > 0.5),1]
which(ranef(log_m_model)>0.5)

p <- predict(log_m_model, df_test, type='response')
pr <- prediction(p, df_test$anno1)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.8121018


for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno1 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.5
  print(APER) # 0.2669374
}

trsh = 0.5
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno1, class.assigned=predictions)

Z0.new.best <- data.frame(cluster = as.factor(2), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030DJS00') # questo è il nuovo dato
Z0.new.worst <- data.frame(cluster = as.factor(2), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030EKU00') # questo è il nuovo dato
Z1.new.best <- data.frame(cluster = as.factor(3), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030DJS00') # questo è il nuovo dato
Z1.new.worst <- data.frame(cluster = as.factor(3), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030EKU00') # questo è il nuovo dato
p0 <- predict(log_m_model, Z0.new.best, type='response')  #0.005076995  
p0
p0 <- predict(log_m_model, Z0.new.worst, type='response')  #0.01836582 
p0
p1 <- predict(log_m_model, Z1.new.best, type='response')  #0.05910724 
p1
p1 <- predict(log_m_model, Z1.new.worst, type='response') #0.1872076 
p1

###### Number of Re-hospitalizations ######
############ 1
hist(milano$ric1)

hist(milano$ric1, breaks = 100, xlim = c(0, quantile(reduced_time$V2, 0.99)))
curve(dpois(x, lambda = 1), from = 0, col = "red", add = TRUE)

qqplot(qpois((1:10000/10000-0.5/10000),1), milano$ric1, col='red', xlab='Theoretical quantile', ylab='Sample Quantile', asp=1)
abline(0, 1, col='blue')

poi_mod1 = glm( ric1 ~ (cluster + num_comor)^2 + eta_Min + SESSO, data=milano[which(milano$time_study>364),], family = 'poisson')
summary(poi_mod1)

colori =rainbow(length(unique(milano$strutt_id[1:1000])))
boxplot(poi_mod1$residuals[1:1000] ~ milano$strutt_id[1:1000], col=colori,
        xlab='Hospitals', ylab='Residuals', main ='Distribution of residuals across hospitals') 

############ 1
lmm1 = glmer(ric1 ~ (cluster + num_comor)^2 + eta_Min + SESSO + (1|strutt_id), 
             data = milano[which(milano$time_study>364),] , family = 'poisson')
summary(lmm1)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_b <- as.numeric(get_variance_random(lmm1))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE  

x11()
dotplot(ranef(lmm1))  

# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm1)  

x11()
qqnorm(resid(lmm1))
qqline(resid(lmm1), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm1)$strutt_id), main='Normal Q-Q Plot - Random Effects for Hospitals')
qqline(unlist(ranef(lmm1)$strutt_id), col='red', lwd=2)   

# 3)
tab_model(lmm1)

ranef(lmm1)$strutt_id
ranef(lmm1)$strutt_id[which(ranef(lmm1)$strutt_id > 104),1]
# 030DJQ00

###################### 3
hist(milano$ric3)

qqplot(qpois((1:1000/1000-0.5/1000),1.6), milano$ric3, col='red', xlab='Theoretical quantile', ylab='Sample Quantile', asp=1)
abline(0, 1, col='blue')

poi_mod3 = glm( ric3 ~ (cluster + num_comor)^2 + eta_Min + SESSO, data=milano[which(milano$time_study>1094),], family = 'poisson')
summary(poi_mod3)

x11()
par(mfrow=c(2,2))
plot(poi_mod3) 

colori =rainbow(length(unique(milano$strutt_id[1:1000])))
boxplot(poi_mod3$residuals[1:1000] ~ milano$strutt_id[1:1000], col=colori,
        xlab='Hospitals', ylab='Residuals', main ='Distribution of residuals across hospitals') 

############ 3
lmm3 = glmer(ric3 ~ (cluster + num_comor)^2 + eta_Min + SESSO + (1|strutt_id), 
             data = milano[which(milano$time_study>1094),] , family = 'poisson')
summary(lmm3)

sigma2_eps <- as.numeric(get_variance_residual(lmm3))
sigma2_b <- as.numeric(get_variance_random(lmm3))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE  

x11()
dotplot(ranef(lmm3))  

# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm3)  

x11()
qqnorm(resid(lmm3))
qqline(resid(lmm3), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm3)$strutt_id), main='Normal Q-Q Plot - Random Effects for Hospitals')
qqline(unlist(ranef(lmm3)$strutt_id), col='red', lwd=2)    

# 3)
tab_model(lmm3)

ranef(lmm3)$strutt_id
ranef(lmm3)$strutt_id[which(ranef(lmm3)$strutt_id > 1.46),1]
# 042IMR00

###################### 5
hist(milano$ric5)

qqplot(qpois((1:1000/1000-0.5/1000),1), milano$ric5, col='red', xlab='Theoretical quantile', ylab='Sample Quantile', asp=1)
abline(0, 1, col='blue')

poi_mod5 = glm( ric5 ~ (cluster + num_comor)^2 + eta_Min + SESSO, data=milano[which(milano$time_study>1824),], family = 'poisson')
summary(poi_mod5)

x11()
par(mfrow=c(2,2))
plot(poi_mod5) 

colori =rainbow(length(unique(milano$strutt_id[1:1000])))
boxplot(poi_mod5$residuals[1:1000] ~ milano$strutt_id[1:1000], col=colori,
        xlab='Hospitals', ylab='Residuals', main ='Distribution of residuals across Hospitals') 

############ 5
lmm5 = glmer(ric5 ~ (cluster + num_comor)^2 + eta_Min + SESSO + (1|strutt_id), 
             data = milano[which(milano$time_study>1824),] , family = 'poisson')
summary(lmm5)

sigma2_eps <- as.numeric(get_variance_residual(lmm5))
sigma2_b <- as.numeric(get_variance_random(lmm5))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE  

x11()
dotplot(ranef(lmm5))  

# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm5)  

x11()
qqnorm(resid(lmm5))
qqline(resid(lmm5), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm5)$strutt_id), main='Normal Q-Q Plot - Random Effects for Hospitals')
qqline(unlist(ranef(lmm5)$strutt_id), col='red', lwd=2)    

# 3)
tab_model(lmm5)

ranef(lmm5)$strutt_id
ranef(lmm5)$strutt_id[which(ranef(lmm5)$strutt_id > 1.6),1]
# 030INS00

###### Time between hospitalizations ######
time_btw_osp = read.table('time_btw_osp2.txt', header = T)
time_btw_osp$num_com_pre = rowSums(time_btw_osp[,12:31])
time_btw_osp$delta_num_com = rowSums(time_btw_osp[,36:55])
time_btw_osp$CountTot = rowSums(time_btw_osp[,9:11])
#Creo reduced_time, dataset time_btw_osp in cui tengo:
# COD_REG : codice paziente 
# V2: giorni passati dall'ospedalizzazione precedente (risposta che studio!)
# SESSO
# qt_prest_Sum :         numero di giorni in ospedale nella ospedalizzazione precedente
# eta_Min :              età del paziente all'osp precedente (non è la minima perchè cambia!!!!)
# strutt_id :            ospedale dove è stato ricoverato la volta precedente
# CountA,CountB,CountC : numero di medicine prese (per tipo A,B,C) ?
# CountTot :             somma di CountA,CountB,CountC
# num_com_pre :          numero di comorbidità all'ospedalizzazione precedente
# delta_num_com :        variazione nel numero di comorbidità dalla precedente alla corrente ospedalizzazione
# class_prest :          motivo per cui hanno ospedalizzato!

reduced_time = time_btw_osp[,c(1,2,3,6,7,8,9:11,60,61,62,5)]

#Risposta con box-cox
reduced_time$V2_bc = reduced_time$V2+1
lambda_opt = powerTransform(reduced_time$V2_bc)
lambda_opt$lambda
lambda_opt_approx = 0
reduced_time$V2_bc = bcPower(reduced_time$V2_bc, lambda_opt_approx) 
hist(reduced_time$V2_bc)

#Analisi esplorativa

hist(reduced_time$countA)
length(which(reduced_time$countA > 0))
#Effetto medicine:
boxplot(V2~ countA, data = reduced_time)
boxplot(V2_bc~ CountTot, data = reduced_time)
boxplot(V2_bc~ countB, data = reduced_time)
boxplot(V2_bc~ countC, data = reduced_time)
#Si riduce la dispersione e trend positivo! Gli ultimi boxplot non sono 
# molto attendibili dato che le osservazioni sono molto poche

#Effetto età:
boxplot(V2~ eta_Min, data = reduced_time)
boxplot(V2_bc~ eta_Min, data = reduced_time)
#Non sembra esserci molta differenza...

#Effetto comorb:
boxplot(V2 ~ num_com_pre, data = reduced_time)
boxplot(V2_bc ~ num_com_pre, data = reduced_time)
#Più ho numero alto di comor, più avrò log(V2+1) piccolo quindi V2 diminuisce

#Effetto tempo in ospedale:
boxplot(V2 ~ qt_prest_Sum, data = reduced_time)
boxplot(V2_bc ~ qt_prest_Sum, data = reduced_time)
#Non è chiaro ...

#Effetto delta numero comorbidità:
boxplot(V2 ~ delta_num_com, data = reduced_time)
boxplot(V2_bc ~ delta_num_com, data = reduced_time)
#Stessa cosa dei count delle medicine..? Forse ha senso toglierla?

#Effetto sesso:
boxplot(V2 ~ SESSO, data = reduced_time)
boxplot(V2_bc ~ SESSO, data = reduced_time)
# Non sembra esserci differenza

#Faccio lm con tutto tranne cod_reg,strutt_id e CountTot (tanto è somma degli altri count)
lin_mod_iniziale = lm(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre+delta_num_com, data=reduced_time) #uso risposta box-cox 
summary(lin_mod_iniziale)
lin_mod = lm(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre, data=reduced_time) #uso risposta box-cox 
summary(lin_mod) 

x11()
par(mfrow = c(2,2))
plot(lin_mod)
plot(lin_mod$residuals) #ok
vif(lin_mod)

#Controllo se i residui hanno diverse variabilità per diversi ospedali o COD_REG
x11()
boxplot(lin_mod$residuals ~ reduced_time$strutt_id,xlab='Strutt_id', ylab='Residuals')
sample_indx = sample(size = 1000, x=1:dim(reduced_time)[1])
boxplot(lin_mod$residuals[sample_indx] ~ reduced_time$strutt_id[sample_indx],xlab='Strutt_id', ylab='Residuals')
boxplot(lin_mod$residuals[sample_indx] ~ reduced_time$COD_REG[sample_indx],xlab='Strutt_id', ylab='Residuals')

#Passo a lmm con rand_int rispetto gli ospedali 
lmm1 = lmer(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre + (1|strutt_id), data = reduced_time) #modello come lin_mod ma con rand_eff su strutt_id
library(sjPlot)
library(sjstats)
tab_model(lmm1)  
summary(lmm1)
vif(lmm1)
x11()
plot(lmm1)
x11()
dotplot(ranef(lmm1))
x11()
qqnorm(unlist(ranef(lmm1)$strutt_id), main='Normal Q-Q Plot - Random Effects for strutt_id')
qqline(unlist(ranef(lmm1)$strutt_id), col='red', lwd=2)
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

#Provo rispetto COD_REG
lmm1 = lmer(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre+ (1|COD_REG), data = reduced_time)
tab_model(lmm1)
summary(lmm1)
x11()
plot(lmm1)
x11()
dotplot(ranef(lmm1))
x11()
qqnorm(unlist(ranef(lmm1)$COD_REG), main='Normal Q-Q Plot - Random Effects for COD_REG')
qqline(unlist(ranef(lmm1)$COD_REG), col='red', lwd=2)
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

#Provo con tutti e due i rand effects
lmm1 = lmer(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre + (1|strutt_id) + (1|COD_REG), data = reduced_time)
tab_model(lmm1)
summary(lmm1)
x11()
plot(lmm1)
dotplot(ranef(lmm1)) #ci mette un po ma li fa entrambi uno dopo l'altro
x11()
qqnorm(unlist(ranef(lmm1)$COD_REG), main='Normal Q-Q Plot - Random Effects for COD_REG')
qqline(unlist(ranef(lmm1)$COD_REG), col='red', lwd=2)
x11()
qqnorm(unlist(ranef(lmm1)$strutt_id), main='Normal Q-Q Plot - Random Effects for strutt_id')
qqline(unlist(ranef(lmm1)$strutt_id), col='red', lwd=2)
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 





###### Functional Analysis ######
library(fda)
ospxmon <- read.table('ospxmon_func.txt', header=T)
x11()
matplot(t(ospxmon),type='l')

data_W <- t(ospxmon)
abscissa <- 1:84

# nbasis <- 6:100
# gcv <- numeric(length(nbasis))
# for (i in 1:length(nbasis)){
#   basis <- create.bspline.basis(rangeval=c(0,365), nbasis[i],m)
#   gcv[i] <- smooth.basis(abscissa, data_W, basis)$gcv
# }
# x11()
# par(mfrow=c(1,1))
# plot(nbasis,gcv)
# nbase=nbasis[which.min(gcv)]   #14
# abline(v = nbasis[which.min(gcv)], col = 2)

#basis <- create.bspline.basis(rangeval=c(0,365), nbase,m)

m=3
nbase=14
basis.1 <- create.bspline.basis(rangeval=c(0,84),nbasis=nbase,m)
data_W.fd.1 <- Data2fd(y = data_W,argvals = abscissa,basisobj = basis.1)
x11()
plot.fd(data_W.fd.1)

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

pca_W.1$values[1:5]
# 1868.863549  279.044590   27.987432    3.556465    1.482923

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=131 are non-null
x11()
plot(pca_W.1$values[1:14],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:14]/sum(pca_W.1$values),xlab='j',ylab='CPV')

cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)
# first three FPCs
x11()
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')


x11()
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(v=c(12,24,36,48,60,72),lty=2)


x11()
par(mfrow=c(1,2))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)

x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2], pch = 16, xlab = 'PC1', ylab = 'PC2')
