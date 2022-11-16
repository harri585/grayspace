#### Julia and Michael Code for gray space paper ######
#SES and green spaces and resting state for seeds from the default mode network
#and specific ROI (amygdala). Using covariates like
#ADI, walkability, air pollution, and basic demographics. 

#percentage of impenetrable surface areas such as rooftops, roads, or parking lots,
#transformed to z-scores and mulitplied by -1. 
#green space: reshist_addr1_coi_zhe_green
#walkability and air pollution

#this uses the txt/csv files from ABCD 4.0 until the 4.0 is released. 
install.packages('tidyr')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('haven')
install.packages('car')
install.packages('base')
install.packages('psych')
install.packages('lme4')
install.packages('broom')
install.packages('broom.mixed')
install.packages('sjlabelled')
install.packages('lmerTest')
install.packages('ltm')
install.packages("ggpubr")
install.packages("writexl")
install.packages("readxl")
install.packages("gridExtra")
install.packages("effectsize")
install.packages("MuMIn")


library(readxl)
library(writexl)
library(ggpubr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(haven)
library(car)
library(base)
library(psych)
library(lme4)
library(broom)
library(broom.mixed)
library(sjlabelled)
library(lmerTest)
library(ggpubr)
library(ltm)
library(gridExtra)
library(effectsize)
library(MuMIn)


#includes mri info (scanner type), and reshist
setwd("/Volumes/ImagingData/ABCD/ABCD_4.0_FILES")
mri.info <- read.delim("abcd_mri01.txt",header = T, sep = '\t')
rhds <- read.table("abcd_rhds01.txt",header = T, sep = "\t")
family_id <- read.table("acspsw03.txt",header = T, sep = "\t")
handedness <- read.table("abcd_ehis01.txt",header = T, sep = "\t")
cbcl <- read.table("abcd_cbcls01.txt",header = T, sep = "\t")
DERS <- read.table("abcd_socdev_child_emr01.txt", header = T, sep = "\t")

#Took out 119 people who had missing data for DMN/amygdala and copied txt file
setwd("/Volumes/ImagingData/ABCD/Julia_AirPollution/GreenSpaces")
rsfmri <- read.delim("mrirscor02.txt",header = T, sep = "\t")
site <- read.delim("abcd_lt01.txt",header = T, sep = "\t")

#demographics
setwd("/Volumes/ImagingData/ABCD/Julia_AirPollution")
demo_4.0<- readRDS("ABCD_4.0_demographics.RDS")

#getting rid of the first row (variable name in excel).
#We deleted the first row in rhds, so we aren't deleting anything here.
rsfmri <- rsfmri[-1,]
mri.info <- mri.info[-1,]
rhds <- rhds[-1,]
site <- site[-1,]
family_id <- family_id[-1,]
handedness <- handedness[-1,]

##Joining Datasets
#join two datasets together by src_subject_id, eventname, and sex
greenspace <- left_join(rhds,mri.info,by=c("src_subject_id","eventname","sex","subjectkey"))
greenspace <- left_join(greenspace,rsfmri,by=c("src_subject_id","eventname","sex","subjectkey"))
greenspace <- left_join(greenspace,demo_4.0,by=c("src_subject_id","eventname","sex","subjectkey"))
greenspace <- left_join(greenspace,site,by=c("src_subject_id","eventname","sex","subjectkey"))
greenspace <- left_join(greenspace,family_id,by=c("src_subject_id","eventname","sex","subjectkey"))
greenspace <- left_join(greenspace,handedness,by=c("src_subject_id","eventname","sex","subjectkey"))
greenspace <- left_join(greenspace,cbcl,by=c("src_subject_id","eventname","sex","subjectkey"))
greenspace <- left_join(greenspace,DERS,by=c("src_subject_id","eventname","sex","subjectkey"))

#create a trimmed dataset with only baseline. Event name is when the data was collected
greenspace <- greenspace [greenspace$eventname=="baseline_year_1_arm_1",]

#checking to make sure they were joined
table(greenspace$eventname)

#copying over geo-coded greenspace addr1, 2, and 3 to a numeric variable
table(greenspace$reshist_addr1_coi_zhe_green, useNA = "always")
view(greenspace$reshist_addr1_coi_zhe_green)
greenspace$addr1_geo_green <- paste(greenspace$reshist_addr1_coi_zhe_green)
greenspace$addr1_geo_green <- as.numeric(as.character(greenspace$addr1_geo_green))
describe(greenspace$addr1_geo_green)
#multiplied by -1
greenspace$inverse_geo_green <- paste(greenspace$addr1_geo_green * -1)
greenspace$inverse_geo_green <- as.numeric(as.character(greenspace$inverse_geo_green))
view(greenspace$inverse_geo_green)
describe(greenspace$inverse_geo_green)


#Rsfmri_cor_ngd_df_scs_aglh is Average correlation 
#between default network and ASEG ROI left amygdala. Rsfmri_cor_ngd_df_scs_agrh is Average correlation 
#between default network and ASEG ROI right amygdala,
view(greenspace$rsfmri_cor_ngd_df_scs_aglh)
greenspace$LamygdalaDMN <- paste(greenspace$rsfmri_cor_ngd_df_scs_aglh)
greenspace$LamygdalaDMN <- as.numeric(as.character(greenspace$LamygdalaDMN))
describe(greenspace$LamygdalaDMN)
sum(is.na(greenspace$LamygdalaDMN))
view(greenspace$LamygdalaDMN)

view(greenspace$rsfmri_cor_ngd_df_scs_agrh)
greenspace$RamygdalaDMN <- paste(greenspace$rsfmri_cor_ngd_df_scs_agrh)
greenspace$RamygdalaDMN <- as.numeric(as.character(greenspace$RamygdalaDMN))
describe(greenspace$RamygdalaDMN)
sum(is.na(greenspace$RamygdalaDMN))
view(greenspace$RamygdalaDMN)

#covariates 

#age
view(greenspace$interview_age.x)
greenspace$age <- paste(greenspace$interview_age.x)
greenspace$age <- as.numeric(as.character(greenspace$age))
describe(greenspace$age)
view(greenspace$age)

#race (dummy coded)
table(greenspace$race.6level)
table(greenspace$race.4level, useNA = "always")
class(greenspace$race.4level)
greenspace$race.4level <- as.numeric(as.factor(greenspace$race.4level))

#sex (dummy coded)
table(greenspace$sex, useNA = "always")
class(greenspace$sex)

#greenspace$sex <- as.numeric(as.factor(greenspace$sex))
greenspace$sex <- as.factor(as.numeric(greenspace$sex))
table(greenspace$sex)

#ethnicity
table(greenspace$hisp)


#income (dummy coded)
table(greenspace$household.income, useNA = "always")
greenspace$household.income <- as.numeric(as.factor(greenspace$household.income))

#educ 
table(greenspace$high.educ, useNA = "always")
greenspace$high.educ <- as.numeric(as.factor(greenspace$high.educ))

#mri
table(greenspace$mri_info_manufacturer, useNA = "always")

#familial relationships
class(greenspace$rel_family_id.x)
greenspace$rel_family_id <- paste(greenspace$rel_family_id.x)
greenspace$rel_family_id <- as.numeric(as.integer(greenspace$rel_family_id))
view(greenspace$rel_family_id)

#adi percentile
#view(greenspace$reshist_addr1_adi_perc)
#greenspace$adi_perc<- paste(greenspace$reshist_addr1_adi_perc)
#greenspace$adi_perc <- as.numeric(as.character(greenspace$adi_perc))
describe(greenspace$adi_perc)

#other adi variables
#median family income
greenspace$medfamincome<- paste(greenspace$reshist_addr1_adi_income)
greenspace$medfamincome <- as.numeric(as.character(greenspace$medfamincome))
describe(greenspace$medfamincome)
describe(greenspace$medfamincome)

#percentage of residents with at least a high school diploma
greenspace$highschooldip <- paste(greenspace$reshist_addr1_adi_edu_h)
greenspace$highschooldip <- as.numeric(as.character(greenspace$highschooldip))
class(greenspace$highschooldip)
describe(greenspace$highschooldip)

#unemployment rate
greenspace$unemployment <- paste(greenspace$reshist_addr1_adi_unemp)
greenspace$unemployment <- as.numeric(as.character(greenspace$unemployment))
class(greenspace$unemployment)
describe(greenspace$unemployment)

#percentage of families living below th federal poverty line
greenspace$fedpovline <- paste(greenspace$reshist_addr1_adi_pov)
greenspace$fedpovline <- as.numeric(as.character(greenspace$fedpovline))
class(greenspace$fedpovline)
describe(greenspace$fedpovline)

#percentage of single parent households
greenspace$singleparent <- paste(greenspace$reshist_addr1_adi_sp)
greenspace$singleparent <- as.numeric(as.character(greenspace$singleparent))
class(greenspace$singleparent)
describe(greenspace$singleparent)

#histogram of green space variable
hist(greenspace$addr1_geo_green, xlab = "Green")

#cor between left resting state and green (not correlated; .04)
sample_Lrestxgeo <- cor.test(greenspace$LamygdalaDMN, greenspace$inverse_geo_green, method= "pearson")
sample_Lrestxgeo

ggscatter(greenspace, x = "LamygdalaDMN", y = "inverse_geo_green",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RestL", ylab = "Green")

#right resting state and green correlated (-0.033)
sample_Rrestxgeo <- cor.test(greenspace$RamygdalaDMN, greenspace$inverse_geo_green, method= "pearson")
sample_Rrestxgeo

ggscatter(greenspace, x = "RamygdalaDMN", y = "inverse_geo_green",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RestR", ylab = "Green")

#cor between walkability and green (positively (strong) correlated .73)
walkxgreen <- cor.test(greenspace$addr1_walk, greenspace$inverse_geo_green, method= "pearson")
walkxgreen

ggscatter(greenspace, x = "addr1_walk", y = "inverse_geo_green",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Walk", ylab = "Green")

#cor between green and ADI (positively (weakly) correlated .106)
greenxADI <- cor.test(greenspace$adi_perc, greenspace$inverse_geo_green, method= "pearson")
greenxADI

ggscatter(greenspace, x = "adi_perc", y = "inverse_geo_green",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "adi", ylab = "Green")

#cor between roads and green (negatively correlated -0.294)
describe(greenspace$addr1_roads)
roadxgreen <- cor.test(greenspace$addr1_roads, greenspace$inverse_geo_green, method= "pearson")
roadxgreen

ggscatter(greenspace, x = "addr1_roads", y = "inverse_geo_green",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Road", ylab = "Green")

#cor between ADI and left amygdala (negatively corelated (weak) -0.0195)
adixLamygdala <- cor.test(greenspace$adi_perc, greenspace$LamygdalaDMN, method= "pearson")
adixLamygdala

ggscatter(greenspace, x = "adi_perc", y = "LamygdalaDMN",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "adi", ylab = "amygdala")


#relationship between green and income
one.wayADIxIncome <- aov(adi_perc ~ household.income, data = greenspace)
summary(one.wayADIxIncome)
ggplot(data=greenspace, mapping = aes(x = household.income, y = adi_perc)) + geom_boxplot() + theme_bw()

one.wayADIxEduc <- aov(adi_perc ~ greenspace$high.educ, data = greenspace)
summary(one.wayADIxEduc)
ggplot(data=greenspace, mapping = aes(x = high.educ, y = adi_perc)) + geom_boxplot() + theme_bw()
table(greenspace$high.educ)

one.wayADIxEduc <- aov(adi_perc ~ greenspace$high.educ, data = greenspace)
summary(one.wayADIxEduc)
ggplot(data=greenspace, mapping = aes(x = high.educ, y = adi_perc)) + geom_boxplot() + theme_bw()

one.way <- aov(addr1_geo_green ~ site_id_l, data = greenspace)
summary(one.way)

#filtering sex out
#Males <- filter(df, Sex == "Male")
#filter(greenspace, sex =="Male")

#Males=filter(greenspace, sex!='F')
#table(Males$sex)
#describe(Males$LamygdalaDMN)

#MXamyg <- cor.test(Males$LamygdalaDMN, Males$addr1_geo_green, method= "pearson")
#MXamyg
#ggscatter(Males, x = "LamygdalaDMN", y = "addr1_geo_green",
      #    add = "reg.line", conf.int = TRUE,
      #    cor.coef = TRUE, cor.method = "pearson",
       #   xlab = "Lamygdala", ylab = "Green")
#females
#Females=filter(greenspace, sex!='M')
#table(Females$sex)
#describe(Females$LamygdalaDMN)

#FXamyg <- cor.test(Females$LamygdalaDMN, Females$addr1_geo_green, method= "pearson")
#FXamyg
#ggscatter(Females, x = "LamygdalaDMN", y = "addr1_geo_green",
        #  add = "reg.line", conf.int = TRUE,
        #  cor.coef = TRUE, cor.method = "pearson",
        # xlab = "Lamygdala", ylab = "Green")
#n=10125

#linear regression left
#LAmygdala = lm(LamygdalaDMN ~ inverse_geo_green + age
             #   + addr1_roads + adi_perc + addr1_walk
              #  + race.4level + sex + hisp + rel_family_id + handedness
              #  + mri_info_manufacturer, data = greenspace)
#summary(LAmygdala)

#linear regression right
#RamygdalaDMN = lm(RamygdalaDMN ~ inverse_geo_green + age
               #  + addr1_roads + adi_perc + addr1_walk
              #   + race.4level + sex + hisp + rel_family_id + handedness
               #   + mri_info_manufacturer, data = greenspace)
#summary(RamygdalaDMN)

#plot left
#ggplot(greenspace, aes(LamygdalaDMN,inverse_geo_green)) + geom_point() + geom_smooth(method="lm")

#plot right
#ggplot(greenspace, aes(RamygdalaDMN,inverse_geo_green)) + geom_point() + geom_smooth(method="lm")

#interaction
#lmRamygdalaDMN = lm(RamygdalaDMN ~ inverse_geo_green*addr1_adi_wsum + age
                #    + addr1_roads  + addr1_walk
               #     + race.4level + sex + hisp + site + rel_family_id + handedness
                  #  + mri_info_manufacturer, data = greenspace)
#summary(lmRamygdalaDMN)

#lmLamygdalaDMN = lm(LamygdalaDMN ~ inverse_geo_green*addr1_adi_wsum + age
               #     + addr1_roads  + addr1_walk
               #     + race.4level + sex + hisp + site + rel_family_id + handedness
                #    + mri_info_manufacturer, data = greenspace)
#summary(lmLamygdalaDMN)

#linear mixed effects left (w. all covariates)-significant by .01
#LMELamygdalaDMN <- lmerTest::lmer(LamygdalaDMN ~ inverse_geo_green 
                             #     + age
                            #      + addr1_roads + addr1_walk
                              #    + singleparent + fedpovline + unemployment + highschooldip + household.income 
                              #    + race.4level + sex + hisp 
                             #     + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                             #     data=greenspace,REML = T,na.action = na.omit)
#summary(LMELamygdalaDMN)

#linear mixed effects right-not significant
#LMERamygdalaDMN <- lmerTest::lmer(RamygdalaDMN ~ inverse_geo_green 
                            #      + age
                             #     + addr1_roads + addr1_walk
                             #     + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                            #      + race.4level + sex + hisp 
                             #     + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                             #     data=greenspace,REML = T,na.action = na.omit)
#summary(LMERamygdalaDMN)

#comparing models with and without site (not significant; we took out site based on geographic region)
#left
#LMELamygdalaDMNsite <- lmerTest::lmer(LamygdalaDMN ~ inverse_geo_green + age
                             #     + addr1_roads + addr1_walk
                             #     + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                             #     + race.4level + sex + hisp
                              #    + (1|mri_info_manufacturer) + (1|rel_family_id.x) + (1|site_id_l),
                              #    data=greenspace,REML = T,na.action = na.omit)
#summary(LMELamygdalaDMNsite)


#right
#LMERamygdalaDMN <- lmerTest::lmer(RamygdalaDMN ~ inverse_geo_green + age
                             #     + addr1_roads + addr1_walk
                            #      + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                             #     + race.4level + sex + hisp
                             #     + (1|mri_info_manufacturer) + (1|rel_family_id.x) + (1|site_id_l),
                             #     data=greenspace,REML = T,na.action = na.omit)
#summary(LMERamygdalaDMN)
#anova(sex,LamygdalaDMN)
#anova(LMELamygdalaDMN,LMELamygdalaDMNsite)

#table(greenspace$race.4level)
#taken out walkability and roads
#what we used in paper: left (significant at .001)

#create data file 
#cleaned and took out address errors and NAs for Lamygdala and inverse_geo_green
saveRDS(greenspace,"/Volumes/ImagingData/ABCD/Julia_AirPollution/GreenSpaces/greenspace.rds")
Greenspace <- write.csv(greenspace,"/Volumes/ImagingData/ABCD/Julia_AirPollution/GreenSpaces/greenspace.csv", row.names = FALSE)
Greenspace <- read_csv("/Volumes/ImagingData/ABCD/Julia_AirPollution/GreenSpaces/greenspace.csv")

#just to explore different comparison group), they held the same significance pattern
#even when the comparison group was White vs AIAN/NHPI 
table(Greenspace$race.6level, useNA = "always")
Greenspace$RACE_prac <- paste(Greenspace$race.6level)
table(Greenspace$RACE_prac)
Greenspace$RACE_prac[Greenspace$RACE_prac=="White"] <- 1
Greenspace$RACE_prac[Greenspace$RACE_prac=="Black"] <- 2
Greenspace$RACE_prac[Greenspace$RACE_prac=="Mixed"] <- 3
Greenspace$RACE_prac[Greenspace$RACE_prac=="Asian"] <- 4
Greenspace$RACE_prac[Greenspace$RACE_prac=="Other"] <- 5
Greenspace$RACE_prac[Greenspace$RACE_prac=="AIAN/NHPI"] <- 6
Greenspace$RACE_prac[Greenspace$RACE_prac=="NA"] <- 7


#MODEL 1: Output table for analysis (without race/ethnicity)
LMELamygdalaDMN <- lmerTest::lmer(LamygdalaDMN ~ inverse_geo_green + age
                                  + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                                  + sex 
                                  + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                                  data=Greenspace,REML = T,na.action = na.omit)
summary(LMELamygdalaDMN)
anova(LMELamygdalaDMN)

cohens_f_squared(
  LMELamygdalaDMN,
  partial = TRUE,
  ci = 0.95,
  alternative = "greater",
  squared = TRUE,
  verbose = TRUE,
  model2 = NULL,)

#effect sizes
sum(resid(LMELamygdalaDMN)^2)
#residual error = 67.90222
#sum of squares (effect)= 0.047674
0.047674/(67.90222 + 0.047674)
#eta squared = 0.0007016052
#sse(LMELamygdalaDMN, greenspace)
#sse <- sum((fitted(LMELamygdalaDMN) - greenspace$inverse_geo_green)^2)

(rsqLMELamygdalaDMN <- r.squaredGLMM(LMELamygdalaDMN))


#MODEL 2: Output table for analysis (includes household income/educ + race/ethnicity)
LMELamygdalaDMN <- lmerTest::lmer(LamygdalaDMN ~ inverse_geo_green + age
                                  + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                                  + race.6level + sex + hisp
                                  + household.income + high.educ
                                  + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                                  data=Greenspace,REML = T,na.action = na.omit)
summary(LMELamygdalaDMN)


anova(LMELamygdalaDMN)

cohens_f_squared(
  LMELamygdalaDMN,
  partial = TRUE,
  ci = 0.95,
  alternative = "greater",
  squared = TRUE,
  verbose = TRUE,
  model2 = NULL,)

#effect sizes
sum(resid(LMELamygdalaDMN)^2)
#residual error = 67.90222
#sum of squares (effect)= 0.047674
0.047674/(67.90222 + 0.047674)
#eta squared = 0.0007016052
#sse(LMELamygdalaDMN, greenspace)
#sse <- sum((fitted(LMELamygdalaDMN) - greenspace$inverse_geo_green)^2)

(rsqLMELamygdalaDMN <- r.squaredGLMM(LMELamygdalaDMN))

#right (not significant)
LMERamygdalaDMN <- lmerTest::lmer(RamygdalaDMN ~ inverse_geo_green + age
                                  + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                                  + sex 
                                  + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                                  data=greenspace,REML = T,na.action = na.omit)
summary(LMERamygdalaDMN)


#add in household income/educ left (significant at .01)
#Output table for analysis (includes household income/educ + race/ethnicity)
LMELamygdalaDMNhere <- lmerTest::lmer(LamygdalaDMN ~ inverse_geo_green + age
                                  + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                                  + race.6level + sex + hisp
                                  + household.income + high.educ
                                  + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                                  data=Greenspace,REML = T,na.action = na.omit)
summary(LMELamygdalaDMNhere)


anova(LMELamygdalaDMNhere)

cohens_f_squared(
  LMELamygdalaDMNhere,
  partial = TRUE,
  ci = 0.95,
  alternative = "greater",
  squared = TRUE,
  verbose = TRUE,
  model2 = NULL,)

#effect sizes
sum(resid(LMELamygdalaDMNhere)^2)
#residual error = 59.16514
#sum of squares (effect)= 0.033142
0.033142/(59.16514 + 0.033142)
#eta squared = 0.0005598473
#sse(LMELamygdalaDMN, greenspace)
#sse <- sum((fitted(LMELamygdalaDMN) - greenspace$inverse_geo_green)^2)

(rsqLMELamygdalaDMNhere <- r.squaredGLMM(LMELamygdalaDMNhere))



#add in household income/educ without race left (significant at .01)
#Output table for analysis (includes household income/educ)
LMELamygdalaDMN <- lmerTest::lmer(LamygdalaDMN ~ inverse_geo_green + age
                                  + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                                  + sex 
                                  + household.income + high.educ
                                  + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                                  data=greenspace,REML = T,na.action = na.omit)
summary(LMELamygdalaDMN)

pdf("Output1.pdf")
grid.table(greenspace)
dev.off()

#plot
ggplot(greenspace, aes(LamygdalaDMN,inverse_geo_green)) + geom_point() + geom_smooth(method="lm")
#clean up the graph
ggplot(data = sample, aes(x = LamygdalaDMN, y = inverse_geo_green))  + 
                                geom_smooth(method="lm") +
                                theme_classic() +
                                ylim(-1, 2) +
                                ylab('Impenetrable Surface Area (Gray Space)') +
                                xlab('Left Amygdala-DMN Connectivity') + 
                                theme(axis.text = element_text(size=12, color = 'black'),
                                axis.title = element_text(size=12, color = 'black')) 
 table(greenspace$race.6level)
#right
LMERamygdalaDMN <- lmerTest::lmer(RamygdalaDMN ~ inverse_geo_green + age
                                  + singleparent + fedpovline + unemployment + highschooldip + medfamincome 
                                  + sex  
                                  + (1|mri_info_manufacturer) + (1|rel_family_id.x),
                                  data=greenspace,REML = T,na.action = na.omit)
summary(LMERamygdalaDMN)

#plot
ggplot(greenspace, aes(RamygdalaDMN,inverse_geo_green)) + geom_point() + geom_smooth(method="lm")

#demographics
describe(LMELamygdalaDMN@frame$age)
table(LMELamygdalaDMN@frame$hisp)
table(LMELamygdalaDMN@frame$race.6level)
table(LMELamygdalaDMN@frame$sex)
table(LMELamygdalaDMN@frame$mri_info_manufacturer)
describe(LMELamygdalaDMN@frame$inverse_geo_green)
describe(LMELamygdalaDMN@frame$LamygdalaDMN)
describe(LMELamygdalaDMN@frame$age)
describe(LMELamygdalaDMN@frame$fedpovline)
describe(LMELamygdalaDMN@frame$unemployment)
describe(LMELamygdalaDMN@frame$highschooldip)
describe(LMELamygdalaDMN@frame$medfamincome)
describe(LMELamygdalaDMN@frame$singleparent)
table(LMELamygdalaDMN@frame$household.income)
describe(Greenspace$cbcl_scr_syn_internal_r.x)
describe(Greenspace$cbcl_scr_syn_external_r.x)

#correlation between amygdala and internalizing symptoms
internal <- cor.test(greenspace$cbcl_scr_syn_internal_r.x, greenspace$inverse_geo_green, method= "pearson")
internal 

external <- cor.test(greenspace$cbcl_scr_syn_external_r.x, greenspace$inverse_geo_green, method= "pearson")
external

social <- cor.test(greenspace$cbcl_scr_syn_social_r.x, greenspace$inverse_geo_green, method= "pearson")
social

DERS <- cor.test(greenspace$cbcl_scr_syn_social_r.x, greenspace$inverse_geo_green, method= "pearson")
social

describe(greenspace$cbcl_scr_syn_internal_r.x)
greenspace$cbcl_scr_syn_internal_t <- as.numeric(as.factor(greenspace$cbcl_scr_syn_internal_t.x))

class(greenspace$cbcl_scr_syn_internal_r.x)
greenspace$cbcl_scr_syn_internal_t <- as.numeric(as.factor(greenspace$cbcl_scr_syn_internal_t.x))

ggscatter(greenspace, x = "cbcl_scr_syn_internal_r.x", y = "addr1_geo_green",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "internalizing", ylab = "green")