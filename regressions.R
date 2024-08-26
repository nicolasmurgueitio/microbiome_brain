setwd()
library("ggplot2")
library("stdmod")
library(ggiraphExtra)
library(dplyr)

# Amygdala-Genus Regression


fit<-(lm(amygdala~Alistipes+Klebsiella+Corynebacterium+Citrobacter+Shigella+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]

fit<-(lm(amygdala~Alistipes+Klebsiella+Corynebacterium+Citrobacter+Shigella+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

ggplot(outdata, aes(y = amygdala, x = Corynebacterium)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, method.args = list(robust = TRUE)) +
  labs(x = "Corynebacterium Abundance (log transformed)", y = "Amygdala Structure")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) +  
  theme(axis.line.x = element_line(color = "black"),  
        axis.line.y = element_line(color = "black")) 




# Insula-Genus Regression


fit<-(lm(insula~Veillonella+Enterococcus+Bifidobacterium+Vibrio+Corynebacterium+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))

dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]

fit<-(lm(insula~Veillonella+Enterococcus+Bifidobacterium+Vibrio+Corynebacterium+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

ggplot(outdata, aes(y = insula, x = Veillonella)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, method.args = list(robust = TRUE)) +
  labs(x = "Veillonella Abundance (log transformed)", y = "Insula Structure")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) +  
  theme(axis.line.x = element_line(color = "black"),  
        axis.line.y = element_line(color = "black")) 

ggplot(outdata, aes(y = insula, x = Vibrio)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, method.args = list(robust = TRUE)) +
  labs(x = "Vibrio Abundance (log transformed)", y = "Insula Structure")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) +  
  theme(axis.line.x = element_line(color = "black"),  
        axis.line.y = element_line(color = "black")) 





# Thalamus-Genus Regression



fit<-(lm(thalamus~Enterobacter+Cutibacterium+Vibrio+Bifidobacterium+Raoultella+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(thalamus~Enterobacter+Cutibacterium+Vibrio+Bifidobacterium+Raoultella+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)


ggplot(outdata, aes(y = thalamus, x = Enterobacter)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, method.args = list(robust = TRUE)) +
  labs(x = "Enterobacter Abundance (log transformed)", y = "Thalamus Structure")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) +  
  theme(axis.line.x = element_line(color = "black"),  
        axis.line.y = element_line(color = "black")) 




# Alpha + Brain


fit<-(lm(thalamus~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(thalamus~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

fit<-(lm(amygdala~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(amygdala~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

fit<-(lm(insula~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(insula~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

ggplot(outdata, aes(y = insula, x = Shannon)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, method.args = list(robust = TRUE)) +
  labs(x = "Shannon Entropy", y = "Insula Structure")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) +  
  theme(axis.line.x = element_line(color = "black"),  
        axis.line.y = element_line(color = "black")) 

fit<-(lm(acc~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(acc~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

fit<-(lm(hippocampus~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(hippocampus~Shannon+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)



# Beta + Brain


fit<-(lm(thalamus~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(thalamus~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

fit<-(lm(amygdala~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(amygdala~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

fit<-(lm(insula~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(insula~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)


ggplot(outdata, aes(y = insula, x = beta2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, method.args = list(robust = TRUE)) +
  labs(x = "Weighted Unifrac PC2", y = "Insula Structure")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) +  
  theme(axis.line.x = element_line(color = "black"),  
        axis.line.y = element_line(color = "black")) 

fit<-(lm(acc~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(acc~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)

ggplot(outdata, aes(y = acc, x = beta1)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, method.args = list(robust = TRUE)) +
  labs(x = "Weighted Unifrac PC1", y = "Anterior Cingulate Structure")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) +  
  theme(axis.line.x = element_line(color = "black"),  
        axis.line.y = element_line(color = "black")) 

fit<-(lm(hippocampus~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=data))
dof=df.residual(fit)
cooksThresh=4/dof
cdlist=cooks.distance(fit)[abs(cooks.distance(fit)) > cooksThresh]
ind.remove = which(row.names(data) %in% names(cdlist))
outdata=data[-ind.remove,]
fit<-(lm(hippocampus~beta1+beta2+GADays_T2+GADays_T2+tc_sex_T2+ICV+fp_brestfed_T2+PH_cesarean_T2+ITN_adj_T0,data=outdata))
summary(fit)
VIF(fit)




