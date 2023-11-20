setwd("C:/Users/JLiu/Desktop/EnrollmentSurvey")

##############################################################################
#########################EFA ANALYSIS#########################################
##############################################################################
###get data for PCA
teacher_evaluation_pca<-read.csv(file="full_data.csv",header=T)
pca_english<-teacher_evaluation_pca[,9:40]
pca_math<-teacher_evaluation_pca[,41:72]


###run poly correlational matrics and Scree Plots
library(psych)
polymatrix_english<- polychoric(pca_english,na.rm=T, correct=0)
polymatrix_math<- polychoric(pca_math,na.rm=T, correct=0)

###run EFA
fa.parallel(polymatrix_english$rho, fm = 'minres', fa = 'fa', n.obs = 2633)
fa.parallel(polymatrix_math$rho, fm = 'minres', fa = 'fa', n.obs = 2633)

# try 3 factors for English and 3 factors for Math
tryfactor_English <- fa(polymatrix_english$rho, nfactors =3,rotate = "promax",fm="wls")
print(tryfactor_English$loadings,cutoff = 0.3)
summary(tryfactor_English)
write.csv(tryfactor_English$loadings,"english_promax.csv")

tryfactor_Math <- fa(polymatrix_math$rho, nfactors = 3,rotate = "promax",fm="wls")
print(tryfactor_Math$loadings,cutoff = 0.3)
summary(tryfactor_Math)
write.csv(tryfactor_Math$loadings,"math_promax.csv")


################################################################################################
########## CFA ANALYSIS OF ENGLISH TEACHER SURVEY###############################################
########## DO 1-FACTOR, 3-FACTOR MODELS, 1-FACTOR MODEL FOR EACH, BI-FACTOR MODEL ##############
################################################################################################
library(lavaan)
all_score<-full_data[,2:203]
#transform variables into ordinal categorical
col_names <- names(all_score)
all_score[,col_names] <- lapply(all_score[,col_names] , ordered)

col_names<-names(pca_english)
pca_english[,col_names]<-lapply(pca_math[,col_names] , ordered)
col_names<-names(pca_math)
pca_math[,col_names]<-lapply(pca_math[,col_names] , ordered)

### Conducte 1-factor CFA for English
English1.model<-'factor1=~
ability_direction_English+ability_embrace_English+ability_homework_English+ability_led_English
+ability_overcome_English+attendance_tardi_English+capacity_English+clarity_work_English
+critical_thinking_English+listens_contemplate_English+open_ideas_English+oral_expression_English
+organization_prep_English+problem_solving_English+pursues_opportunities_English
+response_critique_English+response_setback_English+sense_humor_English'
English1.fit<-cfa(English1.model, data=pca_english, ordered=col_names, std.lv=TRUE)
### once R knows it is "ordered=..." it will use WLSMV as estimator.Constrain the varaince and mean of factors.
options(max.print=999999)
summary(English1.fit, fit.measures=TRUE, standardized=TRUE)
mi_english1<-modindices(English1.fit)
write.csv(mi_english1,"1cfa_English_modi.csv")

### Conducte 3-factor CFA for English
English3.model<-'factor1=~ability_direction_English+ability_homework_English+attendance_tardi_English
+organization_prep_English
factor2=~capacity_English+clarity_work_English+critical_thinking_English+oral_expression_English
+problem_solving_English+pursues_opportunities_English
factor3=~ability_embrace_English+ability_led_English+ability_overcome_English+listens_contemplate_English
+open_ideas_English+response_critique_English+response_setback_English+sense_humor_English'
English3.fit<-cfa(English3.model, data=pca_english, ordered=col_names, std.lv=TRUE)
### once R knows it is "ordered=..." it will use WLSMV as estimator.Constrain the varaince and mean of factors.
summary(English3.fit, fit.measures=TRUE, standardized=TRUE)
mi_english3<-modindices(English3.fit)
mi_english1[mi_english1$op=='=~',]
write.csv(mi_english3,"3cfa_English_modi.csv")

###Check Each of three factors for English
English.factor1<-'factor1=~ability_direction_English+ability_homework_English+attendance_tardi_English
+organization_prep_English'
English.factor2<-'factor2=~capacity_English+clarity_work_English+critical_thinking_English+oral_expression_English
+problem_solving_English+pursues_opportunities_English'
English.factor3<-'factor3=~ability_embrace_English+ability_led_English+ability_overcome_English+listens_contemplate_English
+open_ideas_English+response_critique_English+response_setback_English+sense_humor_English'
English.factor1.fit<-cfa(English.factor1, data=pca_english, ordered=col_names, std.lv=TRUE)
summary(English.factor1.fit, fit.measures=TRUE, standardized=TRUE)
English.factor2.fit<-cfa(English.factor2, data=pca_english, ordered=col_names, std.lv=TRUE)
summary(English.factor2.fit, fit.measures=TRUE, standardized=TRUE)
English.factor3.fit<-cfa(English.factor3, data=pca_english, ordered=col_names, std.lv=TRUE)
summary(English.factor3.fit, fit.measures=TRUE, standardized=TRUE)

### Conducte bi-factor CFA for English
English.bi<-'general.factor=~ability_direction_English+ability_embrace_English+ability_homework_English+ability_led_English
+ability_overcome_English+attendance_tardi_English+capacity_English+clarity_work_English
+critical_thinking_English+listens_contemplate_English+open_ideas_English+oral_expression_English
+organization_prep_English+problem_solving_English+pursues_opportunities_English
+response_critique_English+response_setback_English+sense_humor_English
factor1=~ability_direction_English+ability_homework_English+attendance_tardi_English
+organization_prep_English
factor2=~capacity_English+clarity_work_English+critical_thinking_English+oral_expression_English
+problem_solving_English+pursues_opportunities_English
factor3=~ability_embrace_English+ability_led_English+ability_overcome_English+listens_contemplate_English
+open_ideas_English+response_critique_English+response_setback_English+sense_humor_English
general.factor  ~~ 0*factor1
general.factor  ~~ 0*factor2
general.factor  ~~ 0*factor3
factor1         ~~ 0*factor2
factor1         ~~ 0*factor3
factor2         ~~ 0*factor3'
English.bi.fit<-cfa(English.bi,data=pca_english, ordered=col_names, std.lv=TRUE)
summary(English.bi.fit, fit.measures=TRUE, standardized=TRUE)

################################################################################################
########## CFA ANALYSIS OF MATH TEACHER SURVEY##################################################
########## DO 1-FACTOR, 3-FACTOR MODELS, 1-FACTOR MODEL FOR EACH, BI-FACTOR MODEL ##############
################################################################################################

### Conducte 1-factor CFA for Math 
Math1.model<-'factor1=~sense_humor_Math+leadership_potential_Math+ability_lead_Math+ability_communicate_Math
+maturity_Math+accountability_Math+willingness_risks_Math+conduct_behavior_Math+attendance_tardi_math
+critical_thinking_Math+problem_solving_Math+willingness_challenge_Math+understand_appreciation_Math
+computation_skills_Math+ability_homework_Math+academic_achievement_Math+organization_prep_Math'
Math1.fit<-cfa(Math1.model, data=pca_math, ordered=col_names, std.lv=TRUE)
# once R knows it is "ordered=..." it will use WLSMV as estimator.Constrain the varaince and mean of factors.
options(max.print=999999)
summary(Math1.fit, fit.measures=TRUE, standardized=TRUE)
mi_math1<-modindices(Math1.fit)
write.csv(mi_math1,"1cfa_math_modi.csv")

### Conducte 3-factor CFA for Math
Math3.model<-'factor1=~conduct_behavior_Math+attendance_tardi_math+ability_homework_Math
+organization_prep_Math+maturity_Math+accountability_Math
factor2=~critical_thinking_Math+computation_skills_Math+academic_achievement_Math+problem_solving_Math
+understand_appreciation_Math+willingness_challenge_Math+willingness_risks_Math
factor3=~sense_humor_Math+leadership_potential_Math+ability_lead_Math+ability_communicate_Math'

Math3.fit<-cfa(Math3.model, data=pca_math, ordered=col_names, std.lv=TRUE)
summary(Math3.fit, fit.measures=TRUE, standardized=TRUE)
mi_math3<-modindices(Math3.fit)
write.csv(mi_math3,"3cfa_math_modi.csv")
### Conducte 1-factor CFA for each of three factors
Math.factor1<-'factor1=~conduct_behavior_Math+attendance_tardi_math+ability_homework_Math
+organization_prep_Math+maturity_Math+accountability_Math'
Math.factor2<-'factor2=~critical_thinking_Math+computation_skills_Math+academic_achievement_Math+problem_solving_Math
+understand_appreciation_Math+willingness_challenge_Math+willingness_risks_Math'
Math.factor3<-'factor3=~sense_humor_Math+leadership_potential_Math+ability_lead_Math+ability_communicate_Math'
Math.factor1.fit<-cfa(Math.factor1, data=pca_math, ordered=col_names, std.lv=TRUE)
summary(Math.factor1.fit, fit.measures=TRUE, standardized=TRUE)
mi_math.factor1<-modindices(Math.factor1.fit)
mi_math.factor1
Math.factor2.fit<-cfa(Math.factor2, data=pca_math, ordered=col_names, std.lv=TRUE)
summary(Math.factor2.fit, fit.measures=TRUE, standardized=TRUE)
Math.factor3.fit<-cfa(Math.factor3, data=pca_math, ordered=col_names, std.lv=TRUE)
summary(Math.factor3.fit, fit.measures=TRUE, standardized=TRUE)

### Conducte bi-factor CFA for Math
Math.bi<-'general.factor=~conduct_behavior_Math+attendance_tardi_math+ability_homework_Math
+organization_prep_Math+maturity_Math+accountability_Math
+critical_thinking_Math+computation_skills_Math+academic_achievement_Math+problem_solving_Math
+understand_appreciation_Math+willingness_challenge_Math+willingness_risks_Math+
sense_humor_Math+leadership_potential_Math+ability_lead_Math+ability_communicate_Math
factor1=~conduct_behavior_Math+attendance_tardi_math+ability_homework_Math
+organization_prep_Math+maturity_Math+accountability_Math
factor2=~critical_thinking_Math+computation_skills_Math+academic_achievement_Math+problem_solving_Math
+understand_appreciation_Math+willingness_challenge_Math+willingness_risks_Math
factor3=~sense_humor_Math+leadership_potential_Math+ability_lead_Math+ability_communicate_Math
general.factor  ~~ 0*factor1
general.factor  ~~ 0*factor2
general.factor  ~~ 0*factor3
factor1         ~~ 0*factor2
factor1         ~~ 0*factor3
factor2         ~~ 0*factor3'
Math.bi.fit<-cfa(Math.bi, data=pca_math, ordered=col_names, std.lv=TRUE)
summary(Math.bi.fit, fit.measures=TRUE, standardized=TRUE)