
library(ggplot2)
library(summarytools)
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(r2glmm)
library(performance) 
library(interactions)
library("readxl")

data=read_excel(path='F:/My Documents/MPH/Biostatistics D/Final project/bio_d_test_use_this_data.xlsx', 
                sheet="data", na=c("-99", "NA"))
#deleting non-relevant variables
data <- data[-c(8, 11:12,21:24, 32:46, 54:73, 75:89)]

#data set exploration
#names(data)
#summary(data)
#unique(data$id)
#there are 304 subjects, 2613 observations

#dfSummary(data[,2:10], graph.col= FALSE, varnumbers=FALSE, valid.col = FALSE)
#dfSummary(data[,10:17], graph.col= FALSE, varnumbers=FALSE, valid.col = FALSE)
#dfSummary(data[,21:29], graph.col= FALSE, varnumbers=FALSE, valid.col = FALSE)
#ggplot(data, aes(x=bg_number_events))+geom_histogram()+facet_grid(.~group)
#ggplot(data, aes(x=bg_number_events))+geom_histogram()
#ggplot(data, aes(x=avg_bp_sys))+geom_histogram()
#ggplot(data, aes(x=avg_bp_dis))+geom_histogram(binwidth=2)
#ggplot(data, aes(x=avg_bp_pulse))+geom_histogram(binwidth = 2)

#data preparation
#outliers graphs
#ggplot(data, aes(x=bg_avg))+geom_histogram(binwidth=5)
#ggplot(data, aes(x=bp_number_events))+geom_histogram(binwidth=5)
#ggplot(data, aes(x=avg_bp_pulse))+geom_histogram(binwidth = 2)
#ggplot(data, aes(x=avg_bp_sys))+geom_histogram(binwidth=2)
#ggplot(data, aes(x=bmi))+geom_histogram(binwidth=2)


#renaming
names(data)[names(data)=='time_centered_bp']<-'time_c'
data$group[data$group=="no BP"]<-"Control"
names(data)[names(data)=='median_household_income_city_est']<-'income'

ggplot(data, aes(x=bg_avg))+geom_histogram(binwidth=5)+
  xlab("Average Blood glucose level (mg/dL)")+geom_vline(xintercept = 295)
ggplot(data, aes(x=avg_bp_pulse))+geom_histogram(binwidth = 2)+
  xlab("Average pulse rate (bpm)")+geom_vline(xintercept = 43)
ggplot(data, aes(x=avg_bp_sys))+geom_histogram(binwidth=2)+
  xlab("Average systolic blood pressure (mmHg)")+geom_vline(xintercept = 170)
ggplot(data, aes(x=bmi))+geom_histogram(binwidth=2)+
  xlab("Body Mass Index (BMI)")+geom_vline(xintercept = 55)
ggplot(subset(data, data$bp_number_events>0), aes(x=bp_number_events))+geom_histogram(binwidth=5)+
  xlab("Number of BP measurements per month")+geom_vline(xintercept = 120)
#17 observations eliminated

#checking data quality
#eliminating outliers
data$bp_number_events[data$bp_number_events>300]<-NA
data$bg_avg[data$bg_avg>295]<-NA
data$bg_avg[data$bg_avg<75]<-NA
data$avg_bp_sys[data$avg_bp_sys>170]<-NA
data$avg_bp_pulse[data$avg_bp_pulse<43]<-NA
data$bmi[data$bmi>55]<-NA
data<-data%>% filter(!is.na(data$bg_avg))

#re-labeling, re-coding and formatting
data$gender[data$gender=="female"]<-"Female"
data$gender[data$gender=="male"]<-"Male"
data$gender<-as.factor(data$gender)

data$diabetes_type[data$diabetes_type=="other"]= "Other"
data$diabetes_type[data$diabetes_type=="gestational"]<-"Other"
data$diabetes_type[data$diabetes_type=="diseases-exocrine-pancreas"]= "Other"
data$diabetes_type[data$diabetes_type=="none"]= "Pre-diabetes"
data$diabetes_type[data$diabetes_type=="pre-diabetes"]= "Pre-diabetes"
data$diabetes_type[data$diabetes_type=="not_sure"]= "Other"
data$diabetes_type[data$diabetes_type=="type1"]= "Type 1"
data$diabetes_type[data$diabetes_type=="type2"]= "Type 2"
data$diabetes_type<-factor(data$diabetes_type, levels=c("Pre-diabetes", "Other", "Type 2", "Type 1"))

data$insulin_treatment[data$insulin_treatment=="0"]= "No"
data$insulin_treatment[data$insulin_treatment=="no"]= "No"
data$insulin_treatment[data$insulin_treatment=="pump"]="Yes"
data$insulin_treatment[data$insulin_treatment=="pen"]= "Yes"
data$insulin_treatment<-factor(data$insulin_treatment, levels=c("No", "Yes"))

data$smoke[data$smoke=="no"]= "No"
data$smoke[data$smoke=="ocassionaly"]= "No"
data$smoke[data$smoke=="never"]="Never" 
data$smoke[data$smoke=="yes"]="Yes" 
data$smoke<-factor(data$smoke, levels=c("Never", "No", "Yes"))

#ethnicity: white, latino, black, asian, other (40% missing, mostly white)
data$ethnicity[data$ethnicity=="white"]= "White"
data$ethnicity[data$ethnicity=="other, white"]= "White"
data$ethnicity[data$ethnicity=="white, hawaiian"]= "White"
data$ethnicity[data$ethnicity=="white, indian"]= "White"
data$ethnicity[data$ethnicity=="white, other"]= "White"
data$ethnicity[data$ethnicity=="other, white"]= "White"
data$ethnicity[data$ethnicity=="black"]= "Black"
data$ethnicity[data$ethnicity=="black, indian"]= "Black"
data$ethnicity[data$ethnicity=="latino"]= "Latino"
data$ethnicity[data$ethnicity=="latino, hawaiian"]= "Latino"
data$ethnicity[data$ethnicity=="latino, white, indian"]= "Latino"
data$ethnicity[data$ethnicity=="asian, hawaiian"]= "Asian"
data$ethnicity[data$ethnicity=="asian"]= "Asian"
data$ethnicity[data$ethnicity=="asian, white"]= "Asian"
data$ethnicity[data$ethnicity=="asian, hawaiian"]= "Asian"
data$ethnicity[data$ethnicity=="other"]= "Other"
data$ethnicity[data$ethnicity=="hawaiian, middleEastern"]= "Other"
data$ethnicity[data$ethnicity=="indian"]= "Other"
data$ethnicity[data$ethnicity=="indian, hawaiian"]= "Other"
data$ethnicity[data$ethnicity=="middleEastern"]= "Other"
data$ethnicity<-factor(data$ethnicity, levels=c("White", "Black", "Latino", "Asian", "Other"))

#adding variables
#data$comorbidities<-ifelse(!is.na(data$none_comorb) & data$none_comorb==0, 1, 0)
#data$comorbidities<-data$hypertension+data$high_blood_lypids+data$kidney_disease+
#  data$cardiovascular+data$sleep_disorder+data$cancer
#data$comorbidities[data$comorbidities<2]<-0
#data$comorbidities[data$comorbidities>1]<-1
#data$comorbidities<-factor(data$comorbidities, levels=c(0,1), labels=c("No", "Yes"))

#formatting categorical variables
#data$diabetes_type<-as.factor(data$diabetes_type)
#data$gender<-factor(data$gender, levels=c(1,2), labels=c("Female", "Male"))
data$hypertension<-factor(data$hypertension, levels=c(0,1), labels=c("No", "Yes"))
data$depression<-factor(data$depression, levels=c(0,1), labels=c("No", "Yes"))
data$group<-as.factor(data$group)
data$high_blood_lypids<-factor(data$high_blood_lypids, levels=c(0,1), labels=c("No", "Yes"))
data$kidney_disease<-factor(data$kidney_disease, levels=c(0,1), labels=c("No", "Yes"))
data$cardiovascular<-factor(data$cardiovascular, levels=c(0,1), labels=c("No", "Yes"))
data$sleep_disorder<-factor(data$sleep_disorder, levels=c(0,1), labels=c("No", "Yes"))
data$cancer<-factor(data$cancer, levels=c(0,1), labels=c("No", "Yes"))
data$family_genetics<-factor(data$family_genetics, levels=c("no","yes"), labels=c("No", "Yes"))
data$time_from_diagnosis<-2021-data$diagnosed_since
data$none_comorb<-factor(data$none_comorb, levels=c(0,1), labels=c("No", "Yes"))

#adding baseline variables for each subject
#selecting the first row by id group, and obtaining baseline bg
data$group_c<-ifelse(data$group=="BP", 1, -1)
data$group_c<-as.factor(data$group_c)
df<-data[!duplicated(data$id),]%>%select(id, bg_avg)
#aggregating and dding bg_avg_events for each subject
bg_events<-tapply(data$bg_number_events, data$id, mean, na.rm=TRUE)
df<-cbind(df, bg_events)
data<-merge(data, df, by=c("id"))
#making sure the data is sorted
data<-data[with(data, order(id, time_c)),]
data<-data %>% rename(bg_avg=bg_avg.x, bg_baseline= bg_avg.y)
data$bg_delta<-data$bg_avg - data$bg_baseline

#2) Summary statistics of centered time data
df1<-data.frame(val = unclass(summary(data$time_c[data$group=="BP"])))%>%round(digits=2)
df2<-data.frame(val = unclass(summary(data$time_c[data$group=="Control"])))%>%round(digits=2)
df3<-data.frame(val = unclass(summary(data$time_c)))%>%round(digits=2)
df<-cbind(df1, df2, df3)
knitr::kable(df, col.names=c("BP", "Control", "Total"), caption="Centered time by group" )

#histogram
ggplot(data, aes(x=time_c))+geom_histogram(binwidth = 1)+facet_grid(.~group)+xlab("Centered time (months)")

#creating a df with a single instance for each subject, to examine clinical and demographic parameters
library(table1)
t1<-data[!duplicated(data$id),]
t1$total_time<-tapply(data$time_c, data$id, max)-tapply(data$time_c, data$id, min)
label(t1$total_time)<-"Participation time"

#defining a function to compute p-value
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

#labeling
label(t1$age)<-"Age"
label(t1$gender)<-"Gender"
label(t1$diabetes_type)<-"Diabetes type"
label(t1$bmi)<-"Body Mass Index (BMI)"
label(t1$insulin_treatment)<-"Insulin treatment"
label(t1$hypertension)<-"Hypertension"
label(t1$depression)<-"Depression"
label(t1$smoke)<-"Smoking"
label(t1$alcohol_consumption)<-"Alcohol consumption"
label(t1$ethnicity)<-"Ethnicity"
label(t1$none_comorb)<-"Comorbidities"
label(t1$time_from_diagnosis)<-"Time from diagnosis(years)"
label(t1$activity_level)<-"Activity level"
label(t1$stress_level)<-"Stress level"
label(t1$income)<-"Median household income"
label(t1$bg_baseline)<- "Baseline Blood glucose level (mg/dL)"
label(t1$kidney_disease)<- "Kidney disease"
  label(t1$high_blood_lypids)<-"Hyperlipidemia"
  label(t1$cardiovascular)<-"Cardiovascular disease"
  label(t1$sleep_disorder)<-"Sleeping disorder"
  label(t1$cancer)<-"Cancer"
  label(t1$family_genetics)<-"Family history"
  
  #TABLE1: socio-demographic and clinical  parameters
table1(~ gender + age + ethnicity +diabetes_type +insulin_treatment +bmi +none_comorb+
         hypertension+ kidney_disease+ high_blood_lypids+ cardiovascular+ sleep_disorder+
         depression + cancer+ smoke +alcohol_consumption + activity_level + 
         stress_level+ income+time_from_diagnosis+ bg_baseline+ total_time 
       | group, data=t1, overall=F, extra.col=list(`P-value`=pvalue))

#statistically significant difference in variables: age, comorbidities
#non-relevant vars due to missing values: ethnicity,
#the control group had consistently higher missing data (approx. 60% vs. 30% in the BP group)
#for many demographic and life style parameters and pre-existing conditions, so 
#hypertension differences should be considered with caution.

#analysis of participation time
#summary(t1$total_time)
#dfSummary(t1$total_time, graph.col= FALSE, varnumbers=FALSE, valid.col = FALSE)
#ggplot(t1, aes(x=total_time))+geom_histogram(binwidth = 1)+facet_grid(.~group)+xlab("Subject participation time (months)")

####4. present the distribution of bg over time using centered time variable by group. 
#mark on the graph the beginning of BP monitoring (time=0).
#scatter plots 
#ggplot(subset(data, (data$avg_bg>0&group=="BP")), aes(x=time_c, y=avg_bg))+geom_jitter()
#ggplot(subset(data, (data$avg_bg>0&group=="Control")), aes(x=time_c, y=avg_bg))+geom_jitter()
ggplot(data, aes(x=time_c, y=bg_avg))+geom_jitter()+facet_grid(.~group)+
  geom_vline(xintercept = 0)+
  ggtitle("Distribution of blood glucose over time, by group")

ggplot(data, aes(x=time_c, y=bg_delta))+geom_jitter()+facet_grid(.~group)+
  geom_vline(xintercept = 0)+
  ggtitle("Distribution of blood glucose changes over time, by group")
#the distribution of changes seems random in both control and BP monitoring groups

ggplot(data, aes(x=time_c, y=bg_avg, color=group))+
geom_smooth(method = 'loess',span =2, na.rm=TRUE)+
  geom_vline(xintercept = 0)+
  ggtitle("Blood glucose over time in the BP vs. no BP measurement groups")+
  xlab("Time (months), vertical line denotes the starting point for BP measurement")+
  ylab("Monthly average of blood glucose level")

ggplot(data, aes(x=time_c, y=bg_avg, color=group))+
  geom_smooth(method="lm", a.rm=TRUE)+
  geom_vline(xintercept = 0)+
  ggtitle("Distribution of blood glucose over time by group")+
  xlab("Time (months), vertical line denotes BP measurement starting point")+
  ylab("Monthly average of blood glucose level")

ggplot(subset(data, !is.na(data$bg_delta)), aes(x=time_c, y=bg_delta, color=group))+
  geom_smooth(na.rm=TRUE)+
  geom_vline(xintercept = 0)+
  ggtitle("Distribution of blood glucose changes over time by group")+
  xlab("Time (months), vertical line denotes BP measurement starting point")+
  ylab("Change from baseline in monthly average of blood glucose level")

#loess smooth
ggplot(subset(data, data$bg_avg>0), aes(x=time_c, y=bg_avg, color=group))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+
  ggtitle("Blood glucose over time in the BP vs. no BP measurement groups")+
  xlab("Time (months), 0 is the starting point for BP measurement")+
  ylab("Monthly average of blood glucose level")

ggplot(data, aes(x=time_c, y=bg_avg, color=group))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)+
  ggtitle("Blood glucose over time in the BP vs. no BP measurement groups")+
  xlab("Time (months), 0 is the starting point for BP measurement")+
  ylab("Changes in monthly average of blood glucose level")

m2 <- lmer(bg_delta~ time_c*group +bg_std+bg_number_events+age+gender+
           +(1+time_c|id) ,data=afterdf, REML = T)
summary(m2)

#line trends for each participant, by group
ggplot(data, aes(x = time_c, y = bg_delta, group =id ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  geom_vline(xintercept = 0)+
  facet_grid(.~group)
#in the control group, it seems that the changes are equally distributed: most of the participants 
#remain unchanged, and about an equal amount had an increase or a decrease in blood glucose levels, 
#compared to baseline. in the BP monitoring group, except for a few outliers, the trend is
#toward a stabilization or a decrease in blood glucose level. 
#In the BP monitoring group there is also a noticeable difference in blood glucose changes
#before and after monitoring: most of the changes occur during the intervention period.

ggplot(subset(data, !is.na(data$bg_delta)), aes(x=time_c, y=bg_delta, color=group))+
  geom_smooth(method = 'lm', na.rm=TRUE)+ geom_vline(xintercept = 0)+
  ggtitle("Blood glucose changes over time in the BP monitoring vs. control groups")+
  xlab("Time (months), 0 is the starting point for BP measurement")+
  ylab("Changes in monthly average of blood glucose level")
#both groups show a decline in blood glucose levels over time.
#the decline in the control group might be due to the medical surveillance effect.
#the decline in the BP monitoring group is more pronounced, so we can assume there is an effect 
#that is beyond medical surveillance effect.
#after the beginning of BP monitoring (time=0, denoted by vertical line) the difference 
#in blood glucose decline is evident, and the cI for the control and BP monitoring groups do not overlap
#it should be noted that the decline in blood glucose level is small, around 8 units after 6 months,
#so the effect size of this intervention might be small. clinically, this is 
#-----------------------------------------------555555555--------------------------555555555

#5. fit a model for testing group differences in time-related BG fluctuations
#Mixed model:
#level 1: time= month. vars: time_c, bg_avg, BP monitoring: avg_bp_sys/dis/pulse
#level 2: id= subject. vars: group, demographic and clinical parameters, baseline bp measures: baseline_bp_sys/dis/pulse 
#confounders: age, hypertension?
#testing correlations between possible continuous predictors
library(corrplot)
M <- cor(data[,c(5,9:10, 20:22, 27:29,31, 73:74)], use="pairwise.complete.obs", method="pearson")
corrplot.mixed(M, upper="ellipse",order = "hclust", addrect = 2)
# a correlation of 0.63 was found between avg_bg and baseline BG
#a negative correlation of 51% was found between bg_delta and baseline bg 
#(high baseline is correlated to a negative delta, a decrease in bg).
#

ggplot(afterdf, aes(x=bg_delta, y=bg_number_events))+geom_point()
#subjects with more than 100 events per month have a very stable bg (delta is close to zero
# and tends to be negative). large fluctuations are associated with less than 100 or 50
#events per month
ggplot(afterdf, aes(x=bg_delta, y=age))+geom_point()

ggplot(afterdf, aes(x=bg_delta, y=bg_std))+geom_point()
#std<60 is correlated to a small or negative delta

ggplot(afterdf, aes(x=bg_number_events, y=bg_std))+geom_point()
#events<100 most have std<50
#events>100 is strongly correlated to std<75

#testing correlation to categorical variables
#diabetes type
ggplot(subset(data, (!is.na(data$bg_delta)& !is.na(diabetes_type))), aes(x=time_c, y=bg_delta, color=group))+
  geom_smooth(method = 'lm', na.rm=TRUE)+ geom_vline(xintercept = 0)+
  ggtitle("Blood glucose changes over time in the BP monitoring vs. control groups")+
  xlab("Time (months), 0 is the starting point for BP measurement")+
  ylab("Changes in monthly average of blood glucose level")+facet_grid(.~diabetes_type)
#type1 diabetes has a high increased BG in the monitoring group
#type 2- both decrease, monitoring slightly more
#other- control increase, monitoring decrease, control and monitoring have different intercepts 
#pre-diabetes no significant difference, both stay the same

#insulin
ggplot(subset(data, (!is.na(data$bg_delta)& !is.na(insulin_treatment))), aes(x=time_c, y=bg_delta, color=group))+
  geom_smooth(method = 'lm', na.rm=TRUE)+ geom_vline(xintercept = 0)+
  ggtitle("Blood glucose changes over time in the BP monitoring vs. control groups")+
  xlab("Time (months), 0 is the starting point for BP measurement")+
  ylab("Changes in monthly average of blood glucose level")+facet_grid(.~insulin_treatment)
#complete overlap in the before phase, better difference in the after phase. 
#both decrease in the no-insulin group, monitoring more than control
#control increases and monitoring decreases in the yes-insulin


ggplot(subset(data, (!is.na(data$bg_delta)& !is.na(hypertension))), aes(x=time_c, y=bg_delta, color=group))+
  geom_smooth(method = 'lm', na.rm=TRUE)+ geom_vline(xintercept = 0)+
  ggtitle("Blood glucose changes over time in the BP monitoring vs. control groups")+
  xlab("Time (months), 0 is the starting point for BP measurement")+
  ylab("Changes in monthly average of blood glucose level")+facet_grid(.~hypertension)
#no significant difference for the yes , greater decrease in th monitoring group in the no hypertension


ggplot(subset(data, (!is.na(data$bg_delta)& !is.na(gender))), aes(x=time_c, y=bg_delta, color=group))+
  geom_smooth(method = 'lm', na.rm=TRUE)+ geom_vline(xintercept = 0)+
  ggtitle("Blood glucose changes over time in the BP monitoring vs. control groups")+
  xlab("Time (months), 0 is the starting point for BP measurement")+
  ylab("Changes in monthly average of blood glucose level")+facet_grid(.~gender)
#female: no significant difference between control and monitoring, before and after
#male: no difference before, significant difference after

#comparing BP vs. control after monitoring
library(lme4)
library(lmerTest)
library(r2glmm)
library(performance) 
library(interactions)

afterdf<-data%>% filter(time_c>-1)
#null model-id
m0 <- lmer(bg_delta~ (1 |id) ,data=afterdf, REML = T)
summary(m0)
icc(m0)
#ICC=0.702
#70% of variance is explained by between-subjects factors (level 2, id level)
#this means a lot of the variance is explained by between-subjects, and a mixed model is needed.
#i should enter subject-level factors to the model
#null model-group
m0 <- lmer(bg_delta~ (1 |group) ,data=afterdf, REML = T)
summary(m0)
icc(m0)
model_performance(m0)
#icc=0.013
#testing a 3-level model
m0<-lmer(bg_delta~ (1 |group/id) ,data=afterdf, REML = T)
summary(m0)
icc(m0)
#ICC=0.703, almost the same as the 2-level model

m1 <- lmer(bg_delta~ time_c*group_c +age+(1+time_c |id) ,data=afterdf, REML = T)
tab_model(m1,show.re.var= TRUE, show.ci = FALSE, show.r2 = TRUE,
          pred.labels =c("(Intercept)", "Time(centered)", "Group", "Age", "Time:Group"),
          dv.labels= "Group differences in BG changes, after BP monitoring")

m2 <- lmer(bg_delta~ time_c*group_c +age+gender+bg_std+insulin_treatment
           +(1+time_c|id) ,data=afterdf, REML = T)
tab_model(m2, show.re.var= TRUE, show.ci = FALSE, show.r2 = TRUE,
          pred.labels =c("(Intercept)", "Time(centered)", "Group", "Age", "Gender", "BG SD", "Insulin treatment (Yes/No)", "Time:Group"),
          dv.labels= "Group differences in BG changes, after BP monitoring")

#bg changes over time (fixed effect), and the time dynamic is different for each subject (random effect)
m1 <- lmer(bg_delta~ time_c*group_c +(1+time_c |id) ,data=afterdf, REML = T)
summary(m1)
icc(m1)
#significant interaction!

#adjusting for confounder age
m2 <- lmer(bg_delta~ time_c*group +age+(1|id) ,data=afterdf, REML = T)
summary(m2)
icc(m1)
model_performance(m1)
#interaction is significant, 
#r2=0.772
#AIC 15251
r2beta(m1, method = 'nsj', partial = T)
model_performance(m1)

#adding predictors
m1 <- lmer(bg_delta~ time_c*group +#age+#insulin_treatment+gender+ 
             (1|id) ,data=afterdf, REML = T)
summary(m1)
model_performance(m1)
#r2=0.775
#AIC=15170

m2 <- lmer(bg_delta~ time_c*group +age+bg_std+insulin_treatment+#bg_std+bmi+diabetes_type
             +(1|id) ,data=afterdf, REML = T)
summary(m2)
model_performance(m2)
#R2=0.849, AIC=14912
#the interaction is significant, so we can conclude that there is a difference in slopes of the 
#control and intervention groups. all other predictors are non-significant
m2 <- lmer(bg_delta~ time_c*group +age+gender+insulin_treatment+
                 +(1+time_c|id) ,data=afterdf, REML = T)
summary(m2)
model_performance(m2)

m2<-lmer(bg_delta~ time_c*group +insulin_treatment+gender
+(1+time_c|id) ,data=afterdf, REML = T)
#all predictors are significant, but slopes are not significant

m2<-lmer(bg_delta~ time_c*group +gender+age+insulin_treatment+
                    +(1+time_c|id) ,data=afterdf, REML = T)

#----------------------------------------------------
m2 <- lmer(bg_delta~ time_c*group +bg_std+bg_number_events+age+
           +(1+time_c|id) ,data=afterdf, REML = T)
summary(m2)
model_performance(m2)
#plotting and analyzing the interaction
interact_plot(m2, pred=time_c, modx=group, interval = TRUE, x.label ="Time (months)", 
              y.label = "Blood glucose change from baseline (mg/dL)",
              main.title ="Group differences in BG changes over time", 
              legend.main = "Group",
              title="Group diffrerences in BG by time", data=afterdf)

ss<-sim_slopes(m2, pred=time_c, modx=group)
#slopes are not statistically significant
#BP group has a negative slope (-0.75, SE=0.42, p=0.07), a slight decrease in bg
#control group has a positive slope (0.76, se=0.48, p value=0.11), a slight increase 
#tabulating the results:
library(huxtable)
as_huxtable(ss)

#model assumptions
#1) linear relationship between predictor and outcome- in the interaction plot
#2)subjects are independent
#3)homogeneity of residuals,
plot(resid(m2) )
#4)Homoscedasticity: the residuals' variance is equal for any x
# residuals are normally distributed
#the model's residuals vs. the observed values of the outcome variable
#an evenly scattered plot indicates a linear relationship between predictor and outcome vars
plot(m2)
#computing effect size??

#subgroup analysis by gender
#male
df2<-afterdf%>% filter(gender=="Male")
m1 <- lmer(bg_delta~ time_c*group +age+insulin_treatment+#gender+
             (1+time_c|id) ,data=df2, REML = T)
summary(m1)
model_performance(m1)
interact_plot(m1, pred=time_c, modx=group, interval = TRUE, 
              title="Group diffrerences in BG by time", data=df2)
ss<-sim_slopes(m1, pred=time_c, modx=group)
ss
#slope is significant for BP p value=0.05

#female
df2<-afterdf%>% filter(gender=="Female")
m1 <- lmer(bg_delta~ time_c*group +age+insulin_treatment+#gender+
             (1+time_c|id) ,data=df2, REML = T)
summary(m1)
model_performance(m1)
interact_plot(m1, pred=time_c, modx=group, interval = TRUE, 
              title="Group diffrerences in BG by time", data=df2)
ss<-sim_slopes(m1, pred=time_c, modx=group)
ss
#slope is significant for BP p value=0.05

#comparing type 1 and everyone else
t1$DM<-ifelse(t1$diabetes_type=="Type 1", 1, 2)
table1(~ gender + age + ethnicity +group +insulin_treatment +bmi +comorbidities+
         hypertension+ kidney_disease+ high_blood_lypids+ cardiovascular+ sleep_disorder+
         depression + cancer+ smoke +alcohol_consumption + activity_level + 
         stress_level+ income+time_from_diagnosis+ bg_baseline+ total_time 
       |DM , data=t1, overall=F, extra.col=list(`P-value`=pvalue))
#age is significantly lower: 44 yo vs. 63
#insulin 82% vs. 20%


#------------------------------BEFORE
#testing differences in bg fluctuations before monitoring###########
beforedf<-data%>% filter(time_c<0)
#null model-id
m_id <- lmer(bg_delta~ (1 |id) ,data=beforedf, REML = T)
summary(m0)
icc(m0)
sjPlot::tab_model(m_id, m_group, m_id_group,  
                  show.re.var= TRUE, 
                  dv.labels= "Blood glucose changes: grouping by subject id, by group, and by subject and group")
#ICC=0.348
#35% of variance is explained by between-subjects factors (level 2, id level)
#this means part of the variance is explained by between-subjects, and a mixed model is needed.
#null model-group
m_group <- lmer(bg_delta~ (1 |group) ,data=beforedf, REML = T)
summary(m0)
icc(m0)
model_performance(m0)
#icc=0.015
#testing a 3-level model
m_id_group<-lmer(bg_delta~ (1 |group/id) ,data=beforedf, REML = T)
summary(m0)
icc(m0)
#ICC=0.351, almost the same as the 2-level model

#bg changes over time (fixed effect), and the time dynamic is different for each subject (random effect)
m0 <- lmer(bg_delta~ time_c*group_c +(1+time_c |id) ,data=beforedf, REML = T)
summary(m1)
icc(m1)
#non-significant interaction
m0 <- lmer(bg_delta~ time_c+group_c +age+(1|id) ,data=beforedf, REML = T)

#adjusting for confounder age
m1 <- lmer(bg_delta~ time_c*group_c +age+(1|id) ,data=beforedf, REML = T)
summary(m1)
icc(m1)
model_performance(m1)
#interaction is not significant, 
#r2=0.354
r2beta(m1, method = 'nsj', partial = T)
model_performance(m1)

#adding predictors - doesnt converge
m2 <- lmer(bg_delta~ time_c*group +age+gender+ diabetes_type+insulin_treatment
             (1+time_c|id) ,data=beforedf, REML = T)
summary(m2)
model_performance(m2)
#r2=0.472 interaction is not significant
#AIC=6610

#----------------------------------------6666666666------------------------------------------66666
#6). test whether BP changed over time as a result of BP monitoring 
#does bp change over time? YES
#is there a dose-response? (do subjects who completed 5-6 months have better bp reduction) no
#intervention parameters: bp_events, bp_number_events, completed
#slide - model is worse as lag increases

#library(EMAtools)
#data$bg_avgB<-pmean(data$id, data$bg_avg)

#adding baseline bp
bpdata<-subset(data, group=="BP")%>%filter(time_c>-1 & !is.na(avg_bp_sys))
bpdf<-bpdata
bpdf<-bpdf[!duplicated(bpdf$id),]%>%select(id, avg_bp_sys)
bp_events<-tapply(bpdata$bp_number_events, bpdata$id, mean, na.rm=TRUE)
total_time<-tapply(bpdata$time_c, bpdata$id, max)-tapply(bpdata$time_c, bpdata$id, min)
bpdf<-cbind(bpdf, bp_events, total_time)
bpdata<-merge(bpdata, bpdf, by=c("id"))
bpdata<-bpdata[with(bpdata, order(id, time_c)),]
bpdata<-bpdata %>% rename(avg_bp_sys=avg_bp_sys.x, bp_baseline= avg_bp_sys.y)
bpdata$bp_delta<-bpdata$avg_bp_sys - bpdata$bp_baseline
#bpdata$completed<-ifelse(bpdata$total_time>4, 1,0)
#bpdata$completed<-as.factor(bpdata$completed)
#bpdata$bp_events[bpdata$bp_events>90]<-NA
bpdata$Engagement<-ifelse(bpdata$bp_events<30,0,1)
bpdata$Engagement<-factor(bpdata$Engagement, levels=c(0,1), labels=c("Low", "High"))
bpdata$bp_pp<-bpdata$avg_bp_sys-bpdata$avg_bp_dis
bpdata$bp_pp[bpdata$bp_pp>90]<-NA
bpdata$Hypertension<-ifelse(bpdata$avg_bp_sys>130,1,0)
bpdata$Hypertension<-factor(bpdata$Hypertension, levels=c(0,1), labels=c("No", "Yes"))

#comparing subjects by engagement level
bpt1<-bpdata[!duplicated(bpdata$id),]
table1(~ gender + age + diabetes_type +insulin_treatment +bmi +time_from_diagnosis+ bg_baseline+ 
         total_time + avg_bp_sys+ avg_bp_dis+ bp_events+ bg_events
       | Engagement, data=bpt1, overall=F, extra.col=list(`P-value`=pvalue))
#age is the only confounder

#group comparison by completed:
#completed:0/1
#age: 61/67 min max higher in 1
#DM:more type 1/more type 2
#time from diagnosis 8/12
#bp events 24.6/30.2 similar dispersion
#those who did 3-4 months were younger, more type 1, shorter time from diagnosis and less bp events
ggplot(bpdata, aes(x=bp_number_events)) + geom_histogram(binwidth=5) 


ggplot(bpdata, aes(y=bp_delta, x=time_c, group=id)) +  
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(bpdata, aes(x = time_c, y = avg_bp_sys, color=engage))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)


ggplot(bpdata, aes(x = time_c, y = bp_pp))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~engage)

ggplot(bpdata, aes(x = bp_events, y = avg_bp_dis))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~completed)
#completed and engaged had a slightly stronger negative slope
chisq.test(bpdata$engage, bpdata$completed)
#p value<0.001
ggplot(bpdata, aes(x=time_c, y=bp_delta, color=engage))+
  geom_smooth(method = 'loess',span =2, na.rm=TRUE)

ggplot(bpdata, aes(x = time_c, y = bp_delta))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~engage)

ggplot(bpdata, aes(x = bp_number_events, y = avg_bp_sys))+geom_hline(yintercept = 130)+
  geom_vline(xintercept=30)+  geom_point(size = 1.2, alpha = .8, position = "jitter")+
    theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~htn)

ggplot(subset(bpdata, bpdata$bp_delta!=0),aes(x = time_c, y = bp_delta, color=htn))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  #geom_hline(yintercept = 130)+
  #geom_vline(xintercept=30)+  
  facet_grid(.~engage)
#subjects with htn had a smaller delta over time, negligible slope, but they maintained their BP and
#avoided increae.subjects with no htn had a greater decrease in bp

#participants who were engaged had a bigger decrease, the difference is most prominent for participatns with htn

ggplot(bpdata, aes(x = bp_number_events, y = bp_delta))+#geom_hline(yintercept = 130)+
  geom_vline(xintercept=30)+  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~engage)


#plotting bp over time, ignoring nested structure
ggplot(bpdata, aes(x = time_c, y = avg_bp_sys, color=engage))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
#+  facet_grid(.~diabetes_type)
#slight downward trend

ggplot(bpdata, aes(x = time_c, y = bp_delta))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~completed)
#very slight difference between those who completed 5-6 months and 3-4 months of bp monitoring
#same
##@improve the visualization by adding a sample of 10 participants in bold

ggplot(subset(bpdata, bpdata$bp_events>75), aes(x = bp_events, y = bp_delta))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
#no correlation for less than 50 events, there is a correlation for >50 , but not
#many data points

#ggplot(bpdata, aes(x=bp_number_events))+geom_histogram(binwidth=5)
#line trends for each participant
ggplot(bpdata, aes(x = time_c, y = bp_pp, group =id ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~diabetes_type)
#bp changes seem equally distributed, even between different participation time

ggplot(bpdata, aes(x = bp_events, y =bg_events  ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~completed)
#no correlation between bg events and bp events on the subject level

ggplot(bpdata, aes(x = bp_delta, y =bp_number_events))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~engage)
#no correlation
ggplot(bpdata, aes(x = time_c, y = bp_number_events, group =id ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(bpdata, aes(x = time_c, y = bp_delta))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~diabetes_type)

ggplot(bpdata, aes(x = time_c, y = bp_delta))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~diabetes_type)
#prediabetes had the biggest downward trend (10 units reduction), then other, type 2
#type 1 had almost no effect

ggplot(bpdata, aes(x = time_c, y = bp_delta))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~completed)
#pretty much the same:gender, insulin, hypertension 
ggplot(bpdata, aes(x = bp_baseline, y = bp_delta))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
#the higher the baseline bp, the bigger decrease in bp

ggplot(bpdata, aes(x = bp_baseline, y = bp_delta, group =id ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~insulin_treatment)

ggplot(bpdata, aes(x = bg_std, y = bg_delta, group =id ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

#confounder: age
#predictors/moderators: diabetes type, baseline bp

ggplot(bpdata, aes(x=bp_events, y=bp_delta))+geom_point()+facet_grid(.~hypertension)+
  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
##########################testing bp_pp/bp_delta ~ bp_number_events/engage/completed * time_c
m0 <- lmer(bp_delta~ (1 |id) ,data=bpdata, REML = T)
summary(m0)
model_performance(m0)

m0<-lmer(bp_delta~time_c+ (1+time_c|id), data=bpdata, REML=T)
summary(m0)
#BP does decrease with time
m0<-lmer(bp_pp~time_c+ (1+time_c|id), data=bpdata, REML=T)
summary(m0)

#adding confounder age
m1<-lmer(bp_delta~time_c+ age+(1+time_c|id), data=bpdata, REML=T)
summary(m1)
plot(m1)
#still significant
###################################################################
#---------------
ggplot(subset(bpdata, bpdata$time_c>0) , aes(x = time_c, y = bp_delta, color=Hypertension))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  ggtitle("BP changes over time, by hypertension")+
  xlab("Time (months)")+
  ylab("Blood pressure change from baseline (mmHg)")+
  guides(fill=guide_legend(title="Hypertension"))
  
  
   facet_grid(.~completed)

#DM: bigger effects for pre-diabetics (in accordance with our findings), not enough data for type1 and other
#gedner- hypertensive men had the biggest relative difference
#completed 0: hypertensives had an increase in bp, non htn a big decrease
#completed1: same as overall trend

m1<-lmer(bp_delta~time_c*Hypertension+ age+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
plot(m1)
#residuals are not normal

interact_plot(m1, pred=time_c, modx=Hypertension, interval = TRUE, x.label ="Time (months)", 
              y.label = "BP change from baseline (mmHg)",
              main.title ="BP changes over time by hypertension", 
              legend.main = "Hypertension",
              title="Group diffrerences in BP by time", data=bpdata)

ss<-sim_slopes(m1, pred=time_c, modx=Hypertension)



htndata<-bpdata%>%filter(Hypertension=="Yes")
m1<-lmer(bp_delta~time_c* +age+ (1|id), data=htndata, REML=T)
summary(m1)
model_performance(m1)
#R2=0.468, AIC=6707
#bp_number_events is not significant. time is significant.
plot(m1)
#++++++++++++++++++++++++++++++++++++++++++++++++engagement
ggplot(bpdata, aes(y=bp_delta, fill=Engagement)) +  
  geom_boxplot()+
  ylab("BP changes from baseline")+ xlab("")
ggplot(subset(bpdata, bpdata$bp_delta!=0),aes(x = time_c, y = bp_delta, color=Engagement))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Time(months)")+
  ylab("BP change form baseline (mmHg)")

ggplot(subset(bpdata, bpdata$bp_delta!=0),aes(x = time_c, y = bp_delta, color=Engagement))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)
#high engagement is correalted to a slightly bigger decrease in BP

ggplot(subset(bpdata, bpdata$bp_delta!=0),aes(x = time_c, y = bp_delta, color=Engagement))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~Hypertension)
#for normal BP there wasnt a difference between engagement levels
#for hypertensives, those who were moe engaged had the greatest benefit

m1<-lmer(bp_delta~time_c*Engagement+age+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#R2=0.418, AIC=5517
#bp_number_events is not significant. time is significant.

m1<-lmer(bp_delta~time_c*Engagement*Hypertension+ age+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#R2=0.420, AIC=5523
#non-significant interaction between time and completed or engage
plot(m1)

interact_plot(m1, pred=time_c, modx=Engagement, mod2=Hypertension, interval = TRUE, x.label ="Time (months)", 
              y.label = "BP change from baseline (mmHg)",
              main.title = "BP changes over time by hypertension and engagement",
              legend.main = "Engagement",
              data=bpdata)

ss<-sim_slopes(m1, pred=time_c, modx=Engagement, mod2=Hypertension)
#tabulating the results:
library(huxtable)
as_huxtable(ss)

##### model: bp_delta~time+predictors
m1<-lmer(bp_delta~(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#R2=ICC=0.418

m1<-lmer(bp_delta~time_c+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#bp decreased with time
#R2=0.458, AIC=6714.43


m1<-lmer(bp_delta~time_c*engage+age+bp_baseline+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#interaction non significant
################################################################################
###################################bp_delta~bp_events
m1<-lmer(bp_delta~bp_events*bp_baseline+age+diabetes_type+hypertension+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#bp_events is not correlated to bp changes

#with lagging- model is worse
library(DataCombine)
#slide function creates the lag effect we need, in variable 
bpdata <- slide(data = bpdata, Var = 'bp_delta', GroupVar = 'id',
                NewVar = 'bp_delta1', slideBy = -1, keepInvalid = TRUE)
bpdata <- slide(data = bpdata, Var = 'bp_delta', GroupVar = 'id',
                NewVar = 'bp_delta2', slideBy = -2, keepInvalid = TRUE)

m1<-lmer(bp_delta1~time_c+bp_number_events+bp_baseline+age+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#R2=0.456, AIC=5606

#subgroup analysis for hypertension
htn1<-subset(bpdata, bpdata$hypertension==1)
htn0<-subset(bpdata, bpdata$hypertension==0)

ggplot(bpdata, aes(y = avg_bp_sys, x =time_c))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~engage)
#no difference for engage

##-------------------------------------------------77777777777777-----------------------------

#7). TEST THE ASSOCIATION BETWEEN BG AND BP

#ignoring nested structure
#############################################graph 
ggplot(bpdata, aes(x = bp_delta, y =bg_delta  ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~hypertension)
#weak correlation

ggplot(bpdata, aes(x = avg_bp_sys, y =bg_avg  ))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Monthly average systolic blood pressure (mmHg")+
  ylab("Monthly average Blood glucose (mg/dL")+
  ggtitle("Association between systolic BP and blood glucose")
  
#positive correlation
library(corrplot)
M <- cor(bpdata[,c(26:28, 30:32, 71, 73:74, 77)], use="pairwise.complete.obs", method="pearson")
corrplot.mixed(M, upper="ellipse",order = "hclust", addrect = 2)

library(psych)
pairs.panels(bpdata[,c(5,26:28, 30:32, 73, 77)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             stars=TRUE)

#bg and bp measures have a weak correlation( 0.08 or lower)
M <- cor(bpdata[,c(26:32)], use="pairwise.complete.obs", method="pearson")
corrplot.mixed(M, upper="ellipse",order = "hclust", addrect = 2)
library(psych)
pairs.panels(pdata[,c("bg_avg", "bg_number_events", )], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             stars=TRUE)
################################################################
#######significant correlation between bp_dis/sys and bg_avg
#Bg >> Bp
m1<-lmer(bg_delta~ time_c+bp_delta+age+gender+diabetes_type+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)

m1<-lmer(bg_avg ~ time_c*gender+ avg_bp_sys+age+(1|id), data=bpdata, REML=T)
summary(m1)
model_performance(m1)
#R2=0.761, AIC=7998
plot(m1)

ggplot(bpdata, aes(x = bg_avg, y =avg_bp_dis, color=diabetes_type))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(bpdata, aes(x = bg_avg, y =avg_bp_sys, color=gender))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

#############################################################################

ggplot(bpdata, aes(x = bg_avg, y =avg_bp_sys, color=gender))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Monthly average blood glucose")+
  ylab("Monthly average systolic blood pressure")+
  ggtitle("Blood glucose vs. BP by gender")

#subgroup analysis- type 2
d2data<-bpdata%>%filter(diabetes_type=="Type 2")
m2<-lmer(bp_delta ~ bg_delta+gender+age+time_c+ (1|id), data=d2data, REML=T)
summary(m2)
m2<-lmer(bp_delta ~ bg_avg+gender+age+(1|id), data=d2data, REML=T)
summary(m2)
#bp_delta ~ bg_avg+gender+age+diabetes_type+


#
ggplot(bpdata, aes(x = bp_delta, y =bg_avg  , color=diabetes_type))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  #facet_grid(.~)

#------------------------------------88888888888888888---------------------------------------
#8).MEDIATION MODEL: STRESS_LEVEL>>BP>>BG
mdata<-bpdata
mdata$bg_avg[mdata$bg_avg>250]<-NA
#unifying stress level9 (12 observations) and level10 (16 observations)
mdata$stress_level[mdata$stress_level==10]<-9
mdata<-mdata%>%filter(!is.na(mdata$stress_level)&
                        !is.na(mdata$avg_bp_sys)&!is.na(mdata$avg_bp_dis)&!is.na(mdata$bg_avg))

#re-coding smoke
mdata$smoke[mdata$smoke=="No"]<-"Yes"
mdata$smoke<-factor(mdata$smoke, levels=c("Never", "Yes"))
mdata$scat<-ifelse(mdata$stress_level<4, 0, ifelse(mdata$stress_level>7, 2,1))
mdata$scat<-ifelse(mdata$stress_level<5, 0, (ifelse(mdata$stress_level>6, 2,1)))

#118 subjects in this data set, 69% of BP group
#data preparation
ggplot(mdata, aes(x=bg_avg))+geom_histogram(binwidth=3)+
  geom_vline(xintercept = 250)+xlab("Monthly average blood glucose")+
  ggtitle("Distribution of blood glucose in the BP monitoring group")

ggplot(mdata, aes(x=as.factor(stress_level)))+geom_bar()+xlab("Stress level (self-report 0-10 scale)")


#scatter plots for stress>>BP
ggplot(mdata, aes(x=time_c, y=avg_bp_sys, group=id))+geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~as.factor(stress_level))

ggplot(mdata, aes(x=stress_level, y=avg_bp_dis))+geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)

ggplot(mdata, aes(x=stress_level, y=avg_bp_sys))+geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  #stat_smooth(aes(y = avg_bp_sys),method = "lm", formula = y ~ I((2*x)^2), size = 1)
  facet_grid(.~hypertension)
  
#hypertension is correlated to higher levels of stress and a sinusoidal relationship
#gender has no effect
ggplot(mdata, aes(x=stress_level, y=avg_bp_sys))+geom_point()+
  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~depression)
#smokers have stress<5 and pp<60. past smokers have the highest pp.

#depression is correlated to higher stress
ggplot(mdata, aes(x=age, y=avg_bp_sys-avg_bp_dis))+geom_point()
#age is related to higher pp
ggplot(mdata, aes(y=stress_level, x=age))+geom_point()
#weak correlation

#sinusoidal
ggplot(mdata, aes(x = stress_level, y = avg_bp_sys)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size=3)

ggplot(mdata, aes(y=bg_avg, fill=as.factor(stress_level))) +  
  geom_boxplot()+facet_grid(.~as.factor(stress_level))
#1st group avg is a bit higher, no significant difference

ggplot(mdata, aes(y=bg_avg, fill=as.factor(stress_level))) +  
  geom_boxplot()+facet_grid(.~smoke)

ggplot(mdata, aes(x=age, y=stress_level))+geom_point(size = 1.2, alpha = .8, position = "jitter")

ggplot(mdata, aes(x=stress_level, y=))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  geom_smooth(method="lm", formula=y~cos(5*pi*x/12) + sin(5*pi*x/12))

ggplot(mdata, aes(x=stress_level, y=bp_pp))+geom_point(size = 1.2, alpha = .8, position = "jitter")+
  geom_smooth(method="lm", formula=y~cos(5*pi*x/12) + sin(5*pi*x/12))

#trying to fit a sine curve
ssp <- spectrum(mdata$avg_bp_sys)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lmer(bg_avg ~ I(sin(2*pi/per*stress_level))+I(cos(2*pi/per*stress_level))+ (1 | id), data = mdata,REML = T,
              na.action = na.exclude)
summary(reslm)
rg <- diff(range(mdata$bp_avg_sys))
plot(mdata$bp_avg_sys~mdata$stress_level,ylim=c(min(mdata$bp_avg_sys)-0.1*rg,max(mdata$bp_avg_sys)+0.1*rg))
lines(fitted(reslm)~stress_level,col=4,lty=2) 

#3-stage algorithm:
#stress_level (x) --> bp_delta (m) --> bg_delta (y)
library(lme4)
library(mediation)
#mediation package cant work with lmerTest with the p values, so we use lme4
#Y~X
model_direct <- lme4::lmer(bg_avg ~ time_c+ I((stress_level-5)^2)+diabetes_type+gender+insulin_treatment+
                                (1|id), data = mdata, REML = T, na.action = na.exclude)
summary(model_direct)


# m~x
model_a <- lme4::lmer(avg_bp_sys~ time_c*stress_level+#depression+smoke+age+#none_comorb+#age+gender
                        (1|id), data = mdata,REML = T, na.action = na.exclude)
summary(model_a)

#Y~x+m
model_b <- lme4::lmer(bg_avg ~ time_c*stress_level+ avg_bp_sys +(1|id), data = mdata, REML = T,
                      na.action = na.exclude)
summary(model_b)

med.run <- mediate(model_a , model_b, treat = "stress_level*time_c", mediator = "avg_bp_sys", 
                   sims = 1000 ,dropobs = TRUE)
summary(med.run)
#mediation effect=-0.1217 (CI: -0.2347, -0.03) p-value=0.02
#reporting mediation
#https://towardsdatascience.com/doing-and-reporting-your-first-mediation-analysis-in-r-2fe423b92171
#why mediate? now have a confidence interval and significance levels for the entire indirect effect,
#not only its two individual parts. This is something we need for reporting the mediation.
#(the average causal mediation effect (ACME) represents the expected difference in the potential 
#outcome when the mediator took the value that would realize under the treatment condition as opposed 
#to the control condition, while the treatment status itself is held constant)
#The significant indirect effect is the a-path multiplied by the b-path. So regardless of whether one of the individual paths is insignificant, the indirect effect can be significant. 
#"Introduction to Mediation, Moderation, and Conditional Process Analysis : A Regression-Based Approach" by Andrew F. Hayes. Page 92 in Chapter 4. 

#########################-----1. model direct: STRESS >> BG
#stress>>>BG
ggplot(mdata, aes(x=stress_level, y=bg_avg))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  stat_smooth(method = "lm", formula = y ~ I((x-5)^2), size = 1)
  
m_d<-lmer(bg_avg ~ time_c+ I((stress_level-5)^2)+diabetes_type+gender+insulin_treatment+
            (1+time_c|id), data = mdata, REML = T,
          na.action = na.exclude)
summary(m_d)
model_performance(m_d)
plot(m_d)
#R2=0.806, AIC=5141, stress is significant, residuals are normal
library(sjPlot)
sjPlot::plot_model(m_d, 
                   axis.labels=c("Time(months)", "Stress level", "Diabetes, other", "Diabetes, Type 2", "Diabetes, type 1"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect size on Blood glucose")
sjPlot::tab_model(m_d, 
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Time(months)", "Stress level", "Diabetes, other", "Diabetes, Type 2", "Diabetes, type 1"),
                  dv.labels= "Effects of stress level, DM and time on BG")

m_d<-lmer(bg_avg ~ time_c*stress_level+#I((stress_level-5)^2)+#diabetes_type+gender+insulin_treatment+
            (1|id), data = mdata, REML = T,
          na.action = na.exclude)
summary(m_d)

#########################-----2. model a: STRESS >> BP
ggplot(mdata, aes(x=time_c, y=avg_bp_sys, group=id))+geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~as.factor(stress_level))

m0<-lmer(avg_bp_sys~ stress_level+age+avg_bp_dis+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)
model_performance(m0)
plot(m0)

m2<-lmer(avg_bp_sys~ stress_level+time_c+#hypertension+gender+depression+smoke+none_comorb+
           age+(1+time_c|id), data = mdata,REML = T, na.action = na.exclude)
summary(m2)
model_performance(m2)
#R2=0.749, AIC=4284.6
plot(m2)

m2<-lmer(avg_bp_sys~ time_c+stress_level+#hypertension+gender+depression+smoke+none_comorb+
           age+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m2)


#interaction analysis
interact_plot(m2, pred=time_c, modx=stress_level, interval = TRUE, 
              title="Stress level diffrerences in BG by time", data=mdata)

ss<-sim_slopes(m2, pred=time_c, modx=stress_level, modx.values = c(0:9))
library(huxtable)
as_huxtable(ss)

johnson_neyman(m2, pred=time_c, modx=stress_level, title="Johnson-Neyman plot")

#for stress level 1-2, the slope of bp_sys over time is -0.44
#for average stress level 4.3, the slope of bp_sys over time is -0.78
#for stress level 6.7, the slope of bp_sys over time is -1.13
#the higher the stress level, the greater reduction in bp over time
#hypertension+gender+depression+smoke+none_comorb+
#time, gender, age, hypertension, past/present smoking, depression and comorbidities.
m2<-lmer(avg_bp_sys~ time_c*as.factor(stress_level)+
           age+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m2)
#significant, the higher the stress level, the bigger reduction in bp

m2<-lmer(avg_bp_sys~ time_c+stress_level*comorbidities+
           age+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m2)


m0<-lmer(bp_pp~ time_c+stress_level+smoke+depression+none_comorb+
           age+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)
model_performance(m0)
#R2=0.821, AIC=3598
plot(m0)

m0<-lmer(avg_bp_sys~ time_c*stress_level+hypertension+gender+depression+smoke+none_comorb+
           age+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)
model_performance(m0)
plot(m0)

m0<-lmer(avg_bp_sys~ time_c+scat+gender+smoke+depression+none_comorb+
           age+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)
model_performance(m0)

#stress_level
m0<-lmer(avg_bp_sys~ time_c+ I((stress_level-5)^2)+gender+smoke+depression+none_comorb+
           age+  (1 | id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)
#y~cos(3*pi*x/4) + sin(2*pi*x/4)
m0<-lmer(avg_bp_sys~ time_c+ I(cos(3*pi*stress_level/4))+I(sin(2*pi*stress_level/4))+
           (1 | id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)

m0<-lmer(avg_bp_dis~ stress_level+(1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)
model_performance(m0)
#stress is not significant
#R2=0.721, AIC=4694

m0<-lmer(avg_bp_dis~ time_c+ stress_level+gender+none_comorb+ hypertension+depression+
           smoke+age+
           (1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)

m0<-lmer(avg_bp_sys~ stress_level*none_comorb+
           (1|id), data = mdata,REML = T, na.action = na.exclude)
summary(m0)
model_performance(m0)

################-------------3. model b: BG~ Stress + BP
#testing the Y~M+X, bg_delta~bp_delta+stress_level with all predictors from Q6
#this is problematic, because there are a lot of missing values in all these predictors!
m_b<-lmer(bg_avg ~ time_c+ avg_bp_sys + stress_level+age+(1|id), data = mdata, REML = T,
     na.action = na.exclude)
summary(m_b)
model_performance(m_b)
#R2=0.819, AIC=5009
plot(m_b)
#bp is significant and stress is not- complete mediation

m_b<-lmer(bg_avg ~ time_c* stress_level+ avg_bp_sys+
            (1|id), data = mdata, REML = T, na.action = na.exclude)
summary(m_b)
model_performance(m_b)
#R2=0.818, AIC=5014
#interaction is no longer significant

m_b<-lmer(bg_avg ~ time_c+ bp_pp+hypertension+insulin_treatment+
            (1|id), data = mdata, REML = T, na.action = na.exclude)
summary(m_b)
model_performance(m_b)
plot(m_b)

ggplot(mdata, aes(y=bg_avg, x=time_c))+geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~gender)


#------------------------------bootstrapping CI for ACME and ADE
library(lme4)
library(mediation)

# m~x
model_a <- lme4::lmer(avg_bp_sys~ time_c+stress_level+age+avg_bp_dis+(1|id), data = mdata,REML = T, na.action = na.exclude)
#summary(model_a)
#Y~x+m
model_b<-ime4::lmer(bg_avg ~ time_c+stress_level+avg_bp_sys +age+(1|id), data = mdata, REML = T,
                    na.action = na.exclude)
#summary(model_b)
med.run <- mediate(model_a , model_b, treat = "stress_level", mediator = "avg_bp_sys", sims = 100, dropobs = TRUE)
summary(med.run)

# Example 1: Bootstrap 95% CI for R-Squared
library(boot)
set.seed(23)
# defining a function to obtain average acme and ade from the data of a linear model
acme_avg <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  model_a<-lme4::lmer(avg_bp_sys~ time_c+stress_level+age+avg_bp_dis+(1|id), data = d, REML = T, na.action = na.exclude)
  model_b<-lme4::lmer(bg_avg~time_c+stress_level+avg_bp_sys +age+(1|id), data = d, REML = T, na.action = na.exclude)
  med.run <- mediate(model_a , model_b, treat = "stress_level", mediator = "avg_bp_sys", sims = 100, dropobs = TRUE)
  return(summary(med.run)$d.avg)
}

ade_avg <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  model_a<-lme4::lmer(avg_bp_sys~ time_c+stress_level+age+avg_bp_dis+(1|id), data = d, REML = T, na.action = na.exclude)
  model_b<-lme4::lmer(bg_avg~time_c+stress_level+avg_bp_sys +age+(1|id), data = d, REML = T, na.action = na.exclude)
  med.run <- mediate(model_a , model_b, treat = "stress_level", mediator = "avg_bp_sys", sims = 100, dropobs = TRUE)
  return(summary(med.run)$z.avg)
}

acme_p <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  model_a<-lme4::lmer(avg_bp_sys~ time_c+stress_level+age+avg_bp_dis+(1|id), data = d, REML = T, na.action = na.exclude)
  model_b<-lme4::lmer(bg_avg~time_c+stress_level+avg_bp_sys +age+(1|id), data = d, REML = T, na.action = na.exclude)
  med.run <- mediate(model_a , model_b, treat = "stress_level", mediator = "avg_bp_sys", sims = 100, dropobs = TRUE)
  return(summary(med.run)$d.avg.p)
}

ade_p <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  model_a<-lme4::lmer(avg_bp_sys~ time_c+stress_level+age+avg_bp_dis+(1|id), data = d, REML = T, na.action = na.exclude)
  model_b<-lme4::lmer(bg_avg~time_c+stress_level+avg_bp_sys +age+(1|id), data = d, REML = T, na.action = na.exclude)
  med.run <- mediate(model_a , model_b, treat = "stress_level", mediator = "avg_bp_sys", sims = 100, dropobs = TRUE)
  return(summary(med.run)$z.avg.p)
}


# bootstrapping with 1000 replications

acme_average<-boot(data=mdata, statistic=acme_avg, R=1000)
ade_average<-boot(data=mdata, statistic=ade_avg, R=1000)

acme_average<-boot(data=mdata, statistic=acme_avg, R=1000)
boot.ci(acme_average, type="bca")
#The bootstrapped indirect effect was -0.089, and the 95% confidence interval ranged from -0.191 to 0.003

ade_average<-boot(data=mdata, statistic=ade_avg, R=1000)
boot.ci(ade_average, type="bca")
#The bootstrapped indirect effect was -0.246, and the 95% confidence interval ranged from -0.918 to 0.37.

boot.ci(acme_pval, type="bca")
boot.ci(ade_pval, type="bca")

#summary(results)
# view results
results
plot(acme_pval)
#names(acme_pval)
str(results)
hist(acme_pval$t)
#----------------------------------9999999999999999999999-------------------------------------

#9). prediction model for bg>180
#predictors for avg_bg: 
#level1: bg_std, bg_number_events, avg_bp_sys/dis, (time_c?)
#level2: bg_baseline, diabetes_type, bmi, bg_events, stress_level, insulin_treatment, age, gender,
#diagnosed_since
pdata<-data
pdata$bg_180<-ifelse(pdata$bg_avg>180, 1,0)
pdata$bg_180<-as.factor(pdata$bg_180)#, levels=c(0,1), labels=c("No", "Yes"))
pdata$ageS<-pdata$age/10
pdata$bg_number_eventsS<-pdata$bg_number_events/30
#pdata$bg_std[pdata$bg_std<1]<- 1
#pdata$bg_baseline[pdata$bg_baseline<1]<-1
#pdata$avg_bp_sys[pdata$avg_bp_sys<1]<-1
#pdata$bg_stdL<-log(pdata$bg_std)
#pdata$bg_baselineL<-log(pdata$bg_baseline)
#pdata$avg_bp_sysL<-log(pdata$avg_bp_sys)
#pdata$high_std<-ifelse(pdata$bg_std>32, 1,0)
#identifying differences between normal and abnormal bg levels

#bg_baseline
ggplot(pdata, aes(y=bg_number_events, fill=bg_180)) + geom_boxplot()+
  ylab("Monthly average blood glucose (mg/dL)" )+
  #labs(fill="Peak BG (Yes=1, No=0)")+
  scale_fill_discrete(name = "Peak BG", labels = c("No", "Yes"))

ggplot(pdata, aes(x=bg_number_events, fill=bg_180)) + geom_histogram()+
  ylab("Monthly average blood glucose (mg/dL)" )+
  facet_grid(.~bg_180)
  #labs(fill="Peak BG (Yes=1, No=0)")+
  #scale_fill_discrete(name = "Peak BG", labels = c("No", "Yes"))

#bg_number_events
ggplot(pdata, aes(x=time_c, y=bg_number_events, group=id))+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  facet_grid(.~bg_180)


#testing a null model:
m0<-glmer(bg_180~ (1|id), data=pdata, family=binomial, 
          control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10)
sjPlot::tab_model(m0, 
                  show.re.var= TRUE, 
                  dv.labels= "Peak blood glucose")

#chi-square tests for categorical variables
table1(~ gender + diabetes_type +insulin_treatment +
         hypertension+ depression + bg_number_events
       | bg_180, data=pdata, overall=F, extra.col=list(`P-value`=pvalue))
#higher missing data in the 0 group (smoke, activity- 30% vs. 6%)
#significant predictors of abnormal bg:
#male gender, younger age, type2/type1 diabetes, insulin treatment, HTN, diagnosed-since 
#(co-linear with diabetes type), higher bg_baseline, higher bg_std, higher bp_sys and dis
#not significant: bmi, bg_number_events, bg_events, stress, depression
library(tidyverse)
library(broom)
# Fit the logistic regression model
#bp_number_events+ avg_bp_sys+avg_bp_dis+ avg_bp_pulse + bg_number_events+ +diagnosed_since+ bmi+
mydata <- pdata#[c(1,5, 8:10, 22, 26:30, 31:33, 34:43)]%>%filter(group=="BP"&insulin_treatment=="Yes")
#-----------------
model <- glmer(bg_180 ~log(bg_baseline)+bg_number_eventsS+ageS+insulin_treatment+gender+(1|id), 
               data=pdata, family=binomial, 
               control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10)
summary(model)
#--------------------
model <- glmer(bg_180 ~log(bg_baseline)+ageS+insulin_treatment+gender+(1|id), 
               data=pdata, family=binomial, 
               control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10)
summary(model)
performance(model)
##-----------------model assumptions-----------------
#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

#1).Linearity
# Predict the probability of peak glucose
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
#predictors <- colnames(mydata)
predictors<-c("age", "bg_baseline", "bg_number_eventsS")
#logit = log(probabilities/(1-probabilities))
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities)))# %>%  gather(key = "predictors", value = "predictor.value", -logit)
#Create the scatter plots:
  ggplot(mydata, aes(logit, bg_baseline))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()+ ylab("Baseline blood glucose (mg/dL)")
  
  ggplot(mydata, aes(logit, bg_number_eventsS))+
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") + 
    theme_bw()+ylab("Monthly BG measurements")
  
  ggplot(mydata, aes(logit, age))+
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") + 
    theme_bw()+ylab("Age")

#correlation between bg_180 and insulin
chisq.test(pdata$bg_180, pdata$insulin_treatment)
ctable(pdata$bg_180, pdata$insulin_treatment, dnn= c("Peak blood glucose", "Insulin treatment"), chisq=TRUE)
#correlation between bg_180 and gender
chisq.test(pdata$bg_180, pdata$gender)
ctable(pdata$bg_180, pdata$gender, dnn= c("Peak blood glucose", "Gender"), chisq=TRUE) 

#2. influential values
#plot(model, which = 4, id.n = 3)
model <- glmer(bg_180 ~log(bg_baseline)+bg_number_eventsS+ageS+insulin_treatment+gender+(1|id), 
               data=pdata, family=binomial, 
               control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10)

#https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html
library(influence.ME)
infl<-influence(model, "id")
D<-cooks.distance(infl)
p<-plot(infl, which = "cook")

outliers<-data.frame(D)
id<-unique(pdata$id)
id<-subset(id, (id!="608775"))
outliers<-cbind(outliers, id)
#library(xlsx)
#write.xlsx(outliers , 'F:/My Documents/MPH/D_influenceMe.xlsx',row.names = F)
library(tiff)
p<-readTIFF("F:/My Documents/MPH/plot_influenceMe.tiff")
write.csv(outliers1 , 'F:/My Documents/MPH/outliers1.csv',row.names = F)
tiff(filename="F:/My Documents/MPH/plot_influenceMe_minus1.tiff",  width=1000, height=800, res=200)
p
dev.off()

out<-outliers%>% filter(outliers$D>0.0132)
sum(outliers$D>0.0132)
mean(outliers$D>0.0132)
#21 influential subjects
max(outliers1$D)
#0.1428, 608775 
#0.0868, 488723
#0.0671, 918415

outliers1$id[outliers1$D==max(outliers$D)]
outliers_order<-outliers%>%arrange(desc(D))
head(outliers_order)

peakdata<-pdata%>%filter(bg_180==1)
peaks<-peakdata[!duplicated(peakdata$id),]
#creating a df with all 162 influential subjects observations
odata<-pdata%>% filter(id %in% out$id)
write.csv(odata , 'F:/My Documents/MPH/inf_total_data.csv',row.names = F)
inf_data<-read.csv("F:/My Documents/MPH/inf_total_data.csv")
#df with infuebtial peaks
peaks<-odata%>%filter(bg_180==1)
pdata$out<-ifelse(pdata$id %in% out$id, 1,0)
table(odata$bg_180)
table(pdata$bg_180)
id608775<-odata%>%filter(id=="608775")

##influence dataset, excluding 3 most influential subjects
#out<-pdata%>%filter(id=="608775" | id=="488723" |id=="918415")
infdata<-pdata%>%filter(id!="608775" & id!="488723" & id!="918415")
model <- glmer(bg_180 ~log(bg_baseline)+bg_number_eventsS+ageS+insulin_treatment+gender+(1|id), 
               data=infdata, family=binomial, 
               control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10)
summary(model)
model_performance()
#age is no longer significant
set.seed(123)
training.samples <- pdata$bg_180 %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- pdata[training.samples, ]
test.data <- pdata[-training.samples, ]
pdata$fitted.results <- predict(model,newdata=pdata, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(pdata$bg_180, pdata$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.851, sen: 0.956, AUC=0.956
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.0516, 1, 0)
# Model accuracy= 85.1%
mean(predicted.classes == test.data$bg_180)

#3 multicolinearity
library(psych)
pairs.panels(pdata[,c("bg_baseline", "bg_number_events", "age")], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             stars=TRUE)
library(car)
car::vif(model)
vars<-c("BG baseline", "BG measurements per month", "Age", "Insulin treatment", "Gender")
vif<-c(1.08, 1.12, 1.01, 1.09, 1.02)
vifdf<-data.frame(cbind(vars, vif))

#very low, all vif<1.09

require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)

require(GGally)
#visualizing correlations between continuous predictors
ggpairs(pdata[, c("age", "diagnosed_since", "bg_std", "bmi")])
#bp_sys and dis are strongly correlated (0.767)

#visualizing correlation between outcome and continuous predictors
ggplot(pdata, aes(y=bg_baseline, fill=bg_180)) +  geom_boxplot()
ggplot(pdata, aes(y=age, fill=bg_180)) +  geom_boxplot()
ggplot(pdata, aes(y=bg_number_events, fill=bg_180)) +  geom_boxplot()

ggplot(pdata, aes(y=age, fill=bg_180)) +  geom_boxplot() 
ggplot(pdata, aes(y=2020-diagnosed_since, fill=bg_180)) +  geom_boxplot()

#visualizing correlations between binary vars
library(sjPlot)
DF <- pdata[, c(6:8, 11, 17:19, 27, 44, 56)]
DF[] <- lapply(DF,as.integer)
sjp.corr(DF)
sjt.corr(DF)

#visualizing the correlation between std and bg_180:
pdata %>%
  mutate(prob = ifelse(bg_180 == 1 , 1, 0)) %>%
  ggplot(aes(bg_baseline, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Mixed effects logistic regression Model", 
    x = "baseline blood glucose (mg/dL)",
    y = "Probability of peak bloog glucose"
  )
#ggplot(pdata, aes(y=bg_std, fill=bg_180)) + geom_boxplot()+
#  ylab("blood glucose SD (mg/dL")+
#  labs(fill='Peak blood glucose')

#adding visualizations for predictors correlation to the outcome
#https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/

#lmer warning troubleshooting 
#https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

library(caret)
library(pROC)
set.seed(123)
training.samples <- pdata$bg_180 %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- pdata[training.samples, ]
test.data <- pdata[-training.samples, ]
pdata$fitted.results <- predict(model,newdata=pdata, type='response', allow.new.levels = TRUE)
#calculating and plotting ROC and AUC
roc_obj <- roc(pdata$bg_180, pdata$fitted.results)
plot(roc_obj, print.thres = "best",print.auc=T)
coords(roc_obj, "best", "threshold")
#spe:0.91, sen: 0.929, AUC=0.966
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.055, 1, 0)
# Model accuracy= 93%
mean(predicted.classes == test.data$bg_180)


library(sjPlot)
#plot_model(model, type = "pred", terms = "bg_baseline")+theme_sjplot2()
sjPlot::tab_model(model, 
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", 
                                 "Log(Baseline blood glucose)", "Monthly BG measurements (scaled by 1/30)", 
                                 "Age (scaled by 1/10)", "Insulin treatment (Yes/No)", "Gender"),
                  dv.labels= "Prediction model for peak blood glucose")
#The interpretation of the OR for baseline blood glucose is a bit challenging due to the necessary log transformation.
#since OR>1 we can reasonably ascertain that a higher baseline blood glucose is predictive of a higher odd for 
#peak blood glucose, and this correlation is statistically significant.
#Just as an example, a 20% increase in baseline blood glucose will translate to a 9,282^(log(1.2))=5.27 increased odd of peak blood glucose.
#every 30 additional blood glucose reports per month will result in a 45% decrease in the odd for a peak blood glucose.
#this finding strongly demonstrates the correlation between engagement and glycemic control.
#Age- the odd for peak blood glucose decreases by 29% for each decade of life.  
#insulin treatment- people who are treated with insulin are 4.76 times more likely to suffer peak blood glucose levels.
#men are 2.27 time more likely than women to suffer peak blood glucose levels.