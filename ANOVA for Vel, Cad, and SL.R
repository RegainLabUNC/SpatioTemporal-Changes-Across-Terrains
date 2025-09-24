library(readxl)
library (tidyverse)
library(ez)
library(afex)
library (ggplot2)
library(ggpubr)
library(rstatix)
library (dplyr)
library(car)
library(forcats)  # for fct_recode
library(summarytools)
library(car)
Terrain_Data_FINAL12_18_24_2_ <- read_excel("J:/Lewek Lab/Data/Terrain Gait Speed/Ben Outputs/Terrain Data_FINAL12_18_24 (2).xlsx", sheet = "Walk Ratios", col_types = c("text", 
                                                                                                  "skip", "numeric", "numeric", "numeric", 
                                                                                                  "skip", "numeric", "skip", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "skip", "numeric", "numeric", "numeric", "numeric", "skip", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "skip", "numeric", "numeric", "numeric", "numeric", "skip", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "skip", "numeric", "numeric", "numeric", "numeric", "skip", 
                                                                                                 "numeric", "numeric", "numeric", 
                                                                                                  "skip", "numeric", "numeric", "numeric", "numeric", "skip", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "skip", "numeric", "numeric", "numeric", "numeric", "skip", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "skip", "numeric", "numeric", "numeric","numeric"))
View(Terrain_Data_FINAL12_18_24_2_)


descriptive_stats <- read_excel("J:/Lewek Lab/Data/Terrain Gait Speed/Ben Outputs/Terrain Data_FINAL12_18_24 (2).xlsx", sheet = "Demographics", col_types = c("text",
                                                                                                   "text", "date","text", "date", "numeric", "numeric",  "numeric","numeric","numeric", "numeric", 
                                                                                                   "text", "text", "text", "numeric", "text", "numeric", "text", "text", "numeric"))

View(descriptive_stats)

#=================Descriptive Statistics====================================================================

dfSummary(descriptive_stats,
          style= 'grid',
          varnumbers = FALSE,
          valid.col = FALSE,
          graph.col = FALSE,
          tmp.img.dir = "/tmp",
          round.digits= 2)

#=================Reshape the Data Columns to Long Format (Easier to Handle for ANOVA)===============================================================================================
#========= Reshape Velocity=========

#Long Format
gait_vel_long<-Terrain_Data_FINAL12_18_24_2_ %>%
  select (Subject, `10MWTVel`,LgCobbleAvgVel, SmCobbleAvgVel, MulchAvgVel, 
          SandAvgVel, GravelWhole1Vel, GravelWhole2Vel) %>%
  pivot_longer(
    cols= -Subject,
    names_to= "Condition", 
    values_to = "Velocity")

# Rename condition labels
gait_vel_long <- gait_vel_long %>%
  mutate(Condition = fct_recode(Condition,
                                "10MWT" = "10MWTVel",
                                "Gravel Downslope" = "GravelWhole1Vel",
                                "Gravel Upslope" = "GravelWhole2Vel",
                                "Large Paver" = "LgCobbleAvgVel",
                                "Small Paver" = "SmCobbleAvgVel",
                                "Mulch" = "MulchAvgVel",
                                "Sand" = "SandAvgVel"
  ))

View(gait_vel_long)

#========  Reshape Cadence========

#Long Format
gait_cad_long<- Terrain_Data_FINAL12_18_24_2_ %>%
  select(Subject, `10MWTCad`, LgCobbleAvgCad, SmCobbleAvgCad, MulchAvgCad,
         SandAvgCad, GravelWhole1Cad, GravelWhole2Cad) %>%
  pivot_longer(
    cols=-Subject,
    names_to ="Condition",
    values_to = "Cadence")

# Rename condition labels
gait_cad_long <- gait_cad_long %>%
  mutate(Condition = fct_recode(Condition,
                                "10MWT" = "10MWTCad",
                                "Gravel Downslope" = "GravelWhole1Cad",
                                "Gravel Upslope" = "GravelWhole2Cad",
                                "Large Paver" = "LgCobbleAvgCad",
                                "Small Paver" = "SmCobbleAvgCad",
                                "Mulch" = "MulchAvgCad",
                                "Sand" = "SandAvgCad"
  ))


View(gait_cad_long)

#========= Reshape Step length=====

#Long Format
gait_sl_long<- Terrain_Data_FINAL12_18_24_2_ %>%
  select(Subject, `10MWT_SL`, LgCobbleAvgSL, SmCobbleAvgSL, MulchAvgSL,
         SandAvgSL, GravelWhole1_SL, GravelWhole2_SL) %>%
  pivot_longer(
    cols=-Subject,
    names_to ="Condition",
    values_to = "Step_Length")

# Rename condition labels
gait_sl_long <- gait_sl_long %>%
  mutate(Condition = fct_recode(Condition,
                                "10MWT" = "10MWT_SL",
                                "Gravel Downslope" = "GravelWhole1_SL",
                                "Gravel Upslope" = "GravelWhole2_SL",
                                "Large Paver" = "LgCobbleAvgSL",
                                "Small Paver" = "SmCobbleAvgSL",
                                "Mulch" = "MulchAvgSL",
                                "Sand" = "SandAvgSL"
  ))

View(gait_sl_long)

#========= Reshape Walk Ratio==========

#Long Format
gait_wr_long<- Terrain_Data_FINAL12_18_24_2_ %>%
  select(Subject, `10MWTWR`, LgCobbleWR, SmCobble1WR, MulchFINALWR,
         Sand1WR, GravelDownWR, GravelWhole2WR) %>%
  pivot_longer(
    cols=-Subject,
    names_to ="Condition",
    values_to = "Walk_Ratio")

# Rename condition labels
gait_wr_long <- gait_wr_long %>%
  mutate(Condition = fct_recode(Condition,
                                "10MWT" = "10MWTWR",
                                "Gravel Downslope" = "GravelDownWR",
                                "Gravel Upslope" = "GravelWhole2WR",
                                "Large Paver" = "LgCobbleWR",
                                "Small Paver" = "SmCobble1WR",
                                "Mulch" = "MulchFINALWR",
                                "Sand" = "Sand1WR"
  ))

View(gait_wr_long)
##=========Use anova_test() method using rstatix package==============================================
#==================== ANOVA Velocity===================================================
#Summary Statistics (mean (SD))
gait_vel_long%>%
  group_by (Condition) %>%
  get_summary_stats(Velocity, type="mean_sd")

#check normality
gait_vel_long %>%
  group_by(Condition) %>%
  shapiro_test(Velocity) #Shapiro-Wilks Test of Normality

ggqqplot(gait_vel_long, "Velocity", facet.by= "Condition") #Visual Inspection of QQ plot

#Homoscedasticity
levene_test(gait_vel_long, Velocity~Condition, center=mean)

#One-Way Repeated Measures ANOVA
velocity_anova <- anova_test(
  data= gait_vel_long, 
  dv ="Velocity", 
  wid="Subject", 
  within= "Condition",
  detailed=TRUE,
  effect.size="pes")
get_anova_table(velocity_anova)

velocity_anova$`Mauchly's Test for Sphericity`

#pairwise comparisons
pwcvel <- gait_vel_long%>%
  pairwise_t_test(
    Velocity~Condition, paired = TRUE,
    p.adjust.method= 'bonferroni'
    )
pwcvel

#Effect Sizes (cohen's d)
cohens_d(gait_vel_long,Velocity~Condition,
         paired=TRUE)

#Visualization: Violin plots
vlpvel<- ggplot(gait_vel_long, aes(x=Condition, y=Velocity)) +
  geom_violin(trim= FALSE, fill = "#4B9CD3", alpha = 0.5) +
  geom_jitter (width = 0.08, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.4, color = "black", linewidth = 0.6)+
  labs(
    title = "Gait Speed Across Terrains", 
    subtitle= get_test_label (velocity_anova, detailed =TRUE),
      y= "Speed (m/s)",
    x= NULL
  )+
  theme_minimal()+
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust= 0.5, size= 20),
    plot.subtitle = element_text(size=14),
    axis.line = element_line(color="black"),
    axis.ticks.y = element_line (color ="black"),
    axis.text.x= element_text(size=14, color= "black"),
    axis.title.y= element_text(size=14),
    axis.text.y= element_text (size= 14, color= "black")
    
  )
vlpvel

#==================== ANOVA Cadence ========================================================
#Summary Statistics (mean (SD))
gait_cad_long%>%
  group_by (Condition) %>%
  get_summary_stats(Cadence, type="mean_sd") %>%
  mutate(
    mean = round(mean, 4),
    sd = round(sd, 4)
  )
 
gait_cad_long %>%
  group_by(Condition) %>%
  get_summary_stats(Cadence, type = "mean_sd") %>%
  mutate(
    mean = round(as.numeric(mean), 4),
    sd = round(as.numeric(sd), 4),
    mean_sd = paste0(mean, " ± ", sd)
  ) %>%
  select(Condition, mean_sd)

#check normality
gait_cad_long %>%
  group_by(Condition) %>%
  shapiro_test(Cadence)

ggqqplot(gait_cad_long, "Cadence", facet.by= "Condition")

#Homoscedasticity
levene_test(gait_cad_long, Cadence~Condition, center=mean)

#One Way Repeated Measures ANOVA
cadence_anova <- anova_test(
  data= gait_cad_long, 
  dv ="Cadence", 
  wid="Subject", 
  within= "Condition",
  detailed=TRUE,
  effect.size="pes")
get_anova_table(cadence_anova)

cadence_anova$`Mauchly's Test for Sphericity`

#pairwise comparisons
pwccad <- gait_cad_long%>%
  pairwise_t_test(
    Cadence~Condition, paired = TRUE,
    p.adjust.method= 'bonferroni'
  )
pwccad

#Effect Sizes, Cohen's D
cohens_d(gait_cad_long, Cadence~Condition,
         paired=TRUE)

#Visualization: Violin plots 

vlpcad<- ggplot(gait_cad_long, aes(x=Condition, y=Cadence)) +
  geom_violin(trim= FALSE, fill = "#4B9CD3", alpha = 0.5) +
  geom_jitter (width = 0.08, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.4, color = "black", linewidth = 0.6)+
  labs(
    title = "Cadence Across Terrains", 
    subtitle= get_test_label (cadence_anova, detailed =TRUE),
    y= "Cadence (spm)",
    x= NULL
  )+
  theme_minimal()+
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust= 0.5, size= 20),
    plot.subtitle = element_text(size=14),
    axis.line = element_line(color="black"),
    axis.ticks.y = element_line (color ="black"),
    axis.text.x= element_text(size=14, color= "black"),
    axis.title.y= element_text(size=14),
    axis.text.y= element_text (size= 14, color= "black")
    
  )
vlpcad

#==================== ANOVA Step Length=====================================================

#Summary Statistics (mean (SD))
gait_sl_long%>%
  group_by (Condition) %>%
  get_summary_stats(Step_Length, type="mean_sd")
 
#check normality
gait_sl_long %>%
  group_by(Condition) %>%
  shapiro_test(Step_Length)

ggqqplot(gait_sl_long, "Step_Length", facet.by= "Condition")

#Homoscedasticity
levene_test(gait_sl_long, Step_Length~Condition, center=mean)

#One Way Repeated Measures ANOVA
step_length_anova <- anova_test(
  data= gait_sl_long, 
  dv ="Step_Length", 
  wid="Subject", 
  within= "Condition",
  detailed=TRUE,
  effect.size="pes")
get_anova_table(step_length_anova)

step_length_anova$`Mauchly's Test for Sphericity`

#pairwise comparisons
pwcsl <- gait_sl_long%>%
  pairwise_t_test(
    Step_Length~Condition, paired = TRUE,
    p.adjust.method= 'bonferroni'
  )
pwcsl

#Effect Size: Cohen's D
cohens_d(gait_sl_long,Step_Length~Condition,
         paired=TRUE)

#Visualization: Violin Plots

vlpsl<- ggplot(gait_sl_long, aes(x=Condition, y=Step_Length)) +
  geom_violin(trim= FALSE, fill = "#4B9CD3", alpha = 0.5) +
  geom_jitter (width = 0.08, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.4, color = "black", linewidth = 0.6)+
  labs(
    title = "Step Length Across Terrains", 
    subtitle= get_test_label (step_length_anova, detailed =TRUE),
    y= "Average Step Length (m)",
    x= NULL
  )+
  theme_minimal()+
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust= 0.5, size= 20),
    plot.subtitle = element_text(size=14),
    axis.line = element_line(color="black"),
    axis.ticks.y = element_line (color ="black"),
    axis.text.x= element_text(size=14, color= "black"),
    axis.title.y= element_text(size=14),
    axis.text.y= element_text (size= 14, color= "black")
    
  )
vlpsl

#==================== ANOVA Walk Ratio =====================================================

#Summary Statistics (mean (SD))
gait_wr_long%>%
  group_by (Condition) %>%
  get_summary_stats(Walk_Ratio, type="mean_sd")

#check normality
gait_wr_long %>%
  group_by(Condition) %>%
  shapiro_test(Walk_Ratio)

ggqqplot(gait_wr_long, "Walk_Ratio", facet.by= "Condition")

#Homoscedasticity
levene_test(gait_wr_long, Walk_Ratio~Condition, center=mean)

#One Way Repeated Measures ANOVA
walk_ratio_anova <- anova_test(
  data= gait_wr_long, 
  dv ="Walk_Ratio", 
  wid="Subject", 
  within= "Condition",
  detailed=TRUE,
  effect.size="pes")
get_anova_table(walk_ratio_anova)

walk_ratio_anova$`Mauchly's Test for Sphericity`

#pairwise comparisons
pwcwr <- gait_wr_long%>%
  pairwise_t_test(
    Walk_Ratio~Condition, paired = TRUE,
    p.adjust.method= 'bonferroni'
  )
pwcwr

#Effect Size: Cohen's D
cohens_d(gait_wr_long,Walk_Ratio~Condition,
         paired=TRUE)

#Visualization: Violin Plots

vlpwr<- ggplot(gait_wr_long, aes(x=Condition, y=Walk_Ratio)) +
  geom_violin(trim= FALSE, fill = "#4B9CD3", alpha = 0.5) +
  geom_jitter (width = 0.08, alpha = 0.8) +
  stat_summary(fun = mean, geom = "crossbar", 
               width = 0.4, color = "black", linewidth = 0.6)+
  labs(
    title = "Walk Ratio Across Terrains", 
    subtitle= get_test_label (walk_ratio_anova, detailed =TRUE),
    y= "Walk Ratio (cm/steps/min)",
    x= NULL
  )+
  theme_minimal()+
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust= 0.5, size= 20),
    plot.subtitle = element_text(size=14),
    axis.line = element_line(color="black"),
    axis.ticks.y = element_line (color ="black"),
    axis.text.x= element_text(size=14, color= "black"),
    axis.title.y= element_text(size=14),
    axis.text.y= element_text (size= 14, color= "black")
    
  )
vlpwr


#==================== Multiple Regression=============================================================

#======LargePaver===== 

attach (Terrain_Data_FINAL12_18_24_2_)
#detach(Terrain_Data_FINAL12_18_24_2_)
lgpaver_data <- data.frame(LgCobble_Delta_Vel, LgCobble_Delta_Cad, LgCobble_Delta_SL)
names(lgpaver_data) <- c("GaitSpeedChange", "CadenceChange", "StepLengthChange")

#Multiple Regression
lgpavermod <- lm (LgCobble_Delta_Vel~ LgCobble_Delta_Cad + LgCobble_Delta_SL)
summary(lgpavermod)

#Beta_Standardized_Regression_Coefficients
z_lgpaver_data <-as.data.frame(scale(lgpaver_data))
z_lgpavermod <- lm (GaitSpeedChange ~  CadenceChange + StepLengthChange, data= z_lgpaver_data)
summary(z_lgpavermod)

#z_lgpavermod2 <-lm (GaitSpeedChange ~ poly(CadenceChange,2,raw=TRUE) + poly(StepLengthChange, 2, raw=TRUE) , data= z_lgpaver_data)
#summary (z_lgpavermod2)
#Found to be non-significant

#Assess Variance Inflation Factor (should be less than 10)
#install.packages ("car")
car::vif(z_lgpavermod)

#model 1
####Check the Assumptions Linearity and Homoscedasticity
plot(z_lgpavermod, which=1)
#Check normality using qq plot
plot(z_lgpavermod, which=2)

# #polynomial model, if needed
# plot(z_lgpavermod2, which=1)
# plot(z_lgpavermod2, which=2)

#report using heteroscedastic corrected standard errors; if homoscedasticity is violated
hc_se <- car::hccm (lgpavermod)
lmtest::coeftest(lgpavermod,hc_se)

hc_se <- car::hccm (z_lgpavermod)
lmtest::coeftest(z_lgpavermod,hc_se)


#====Small Paver====

smpaver_data <- data.frame(SmCobble_Delta_Vel, SmCobble_Delta_Cad, SmCobble_Delta_SL)
names(smpaver_data) <- c("GaitSpeedChange", "CadenceChange", "StepLengthChange")

#Multiple Regression
smpavermod <- lm (SmCobble_Delta_Vel~ SmCobble_Delta_Cad + SmCobble_Delta_SL)
summary(smpavermod)

#Beta_Standardized_Regression_Coefficients
z_smpaver_data <-as.data.frame(scale(smpaver_data))
z_smpavermod <- lm (GaitSpeedChange ~  CadenceChange + StepLengthChange, data= z_smpaver_data)
summary(z_smpavermod)

#z_smpavermod2 <-lm (GaitSpeedChange ~ poly(CadenceChange,2,raw=TRUE) + poly(StepLengthChange, 2, raw=TRUE) , data= z_smpaver_data)
#summary (z_smpavermod2)
#Polynomial was non-significant using Heterskedasticity Consistent SEs

#Assess Variance Inflation Factor (should be less than 10)
#install.packages ("car")
car::vif(z_smpavermod)

#model 1
####Check the Assumptions Linearity and Homoscedasticity
plot(z_smpavermod, which=1)
#Check normality using qq plot
plot(z_smpavermod, which=2)


#report using heteroscedastic corrected standard errors; if homoscedasticity is violated
hc_se <- car::hccm (smpavermod)
lmtest::coeftest(smpavermod,hc_se)


hc_se <- car::hccm (z_smpavermod)
lmtest::coeftest(z_smpavermod,hc_se)


#====Sand====

sand_data <- data.frame(Sand_Delta_Vel, Sand_Delta_Cad, Sand_Delta_SL)
names(sand_data) <- c("GaitSpeedChange", "CadenceChange", "StepLengthChange")

#Multiple Regression
sandmod<- lm(Sand_Delta_Vel~ Sand_Delta_Cad + Sand_Delta_SL)
summary (sandmod)

#Beta_Standardized_Regression_Coefficients
z_sand_data <- as.data.frame(scale(sand_data))
z_sandmod <- lm (GaitSpeedChange~  StepLengthChange + CadenceChange, data= z_sand_data)
summary(z_sandmod)

#Model with a Polynomial
#z_sandmod2 <-lm (GaitSpeedChange ~ poly(CadenceChange,2,raw=TRUE) + poly(StepLengthChange, 2, raw=TRUE) , data= z_sand_data)
#summary (z_sandmod2)
#Found to be non-significant with homoskedasticity consistent SEs

#Assumption Checking

#Assess Variance Inflation Factor (should be less than 10)
#install.packages ("car")
car::vif(z_sandmod)
car::vif(z_sandmod2)


####Check the Assumptions Linearity and Homoscedasticity
plot(z_sandmod, which=1)
#Check normality using qq plot
plot(z_sandmod, which=2)


#report using heteroscedastic corrected standard errors; if homoscedasticity is violated
hc_se <- car::hccm (sandmod)
lmtest::coeftest(sandmod,hc_se)

hc_se <- car::hccm (z_sandmod)
lmtest::coeftest(z_sandmod,hc_se)


#====Gravel Down====

graveldown_data <- data.frame(Gravel_Down_Delta_Vel, Gravel_Down_Delta_Cad, Gravel_Down_Delta_SL)
names(graveldown_data) <- c("GaitSpeedChange", "CadenceChange", "StepLengthChange")

graveldownmod <- lm(Gravel_Down_Delta_Vel~ Gravel_Down_Delta_Cad + Gravel_Down_Delta_SL)
summary (graveldownmod)

#Beta_Standardized_Regression_Coefficients
z_graveldown_data <- as.data.frame(scale(graveldown_data))
z_graveldownmod <- lm (GaitSpeedChange~  StepLengthChange + CadenceChange, data= z_graveldown_data)
summary(z_graveldownmod)

#z_graveldownmod2 <-lm (GaitSpeedChange ~ poly(CadenceChange,2,raw=TRUE) + poly(StepLengthChange, 2, raw=TRUE) , data= z_graveldown_data)
#summary (z_graveldownmod2)
#Found to be non-signficant with Heteroskedasticity Consistent SEs

#Assess Variance Inflation Factor (should be less than 10)
#install.packages ("car")
car::vif(z_graveldownmod)
#car::vif(z_graveldownmod2)

#model 1
####Check the Assumptions Linearity and Homoskedasticity
plot(z_graveldownmod, which=1)
#Check normality using qq plot
plot(z_graveldownmod, which=2)

#report using heteroscedastic corrected standard errors; if homoscedasticity is violated
hc_se <- car::hccm (graveldownmod)
lmtest::coeftest(graveldownmod,hc_se)

hc_se <- car::hccm (z_graveldownmod)
lmtest::coeftest(z_graveldownmod,hc_se)


#====Gravel Up====

gravelup_data <- data.frame(Gravel_Up_Delta_Vel, Gravel_Up_Delta_Cad, Gravel_Up_Delta_SL)
names(gravelup_data) <- c("GaitSpeedChange", "CadenceChange", "StepLengthChange")

gravelupmod <- lm(Gravel_Up_Delta_Vel~ Gravel_Up_Delta_Cad + Gravel_Up_Delta_SL)
summary (gravelupmod)

#Beta_Standardized_Regression_Coefficients
z_gravelup_data <- as.data.frame(scale(gravelup_data))
z_gravelupmod <- lm (GaitSpeedChange~  StepLengthChange + CadenceChange, data= z_gravelup_data)
summary(z_gravelupmod)

#z_gravelupmod2 <-lm (GaitSpeedChange ~ poly(CadenceChange,2,raw=TRUE) + poly(StepLengthChange, 2, raw=TRUE) , data= z_gravelup_data)
# summary (z_gravelupmod2)
#Found to be non-significant when using Heteroskedasticity Consistent SEs

#Assess Variance Inflation Factor (should be less than 10)
#install.packages ("car")
car::vif(z_gravelupmod)
# car::vif(z_gravelupmod2)

#model 1
####Check the Assumptions Linearity and Homoscedasticity
plot(z_gravelupmod, which=1)
#Check normality using qq plot
plot(z_gravelupmod, which=2)

# #polynomial model, if needed
# plot(z_gravelupmod2, which=1)
# plot(z_gravelupmod2, which=2)

#report using heteroscedastic corrected standard errors; if homoscedasticity is violated
hc_se <- car::hccm (gravelupmod)
lmtest::coeftest(gravelupmod,hc_se)

hc_se <- car::hccm (z_gravelupmod)
lmtest::coeftest(z_gravelupmod,hc_se)


#====Mulch====

mulch_data <- data.frame(Mulch_Delta_Vel, Mulch_Delta_Cad, Mulch_Delta_SL)
names(mulch_data) <- c("GaitSpeedChange", "CadenceChange", "StepLengthChange")

mulchmod <- lm(Mulch_Delta_Vel~ Mulch_Delta_Cad + Mulch_Delta_SL)
summary (mulchmod)

#Beta_Standardized_Regression_Coefficients
z_mulch_data <- as.data.frame(scale(mulch_data))
z_mulchmod <- lm (GaitSpeedChange~  StepLengthChange + CadenceChange, data= z_mulch_data)
summary(z_mulchmod)

#Polynomial
#z_mulchmod2 <-lm (GaitSpeedChange ~ poly(CadenceChange,2,raw=TRUE) + poly(StepLengthChange, 2, raw=TRUE) , data= z_mulch_data)
#summary (z_mulchmod2)

#Assess Variance Inflation Factor (should be less than 10)
#install.packages ("car")
car::vif(z_mulchmod)
# car::vif(z_mulchmod2)

#model 1
####Check the Assumptions Linearity and Homoscedasticity
plot(z_mulchmod, which=1)
#Check normality using qq plot
plot(z_mulchmod, which=2)

#qq plot using the CAR function, 
qqPlot(z_mulchmod)
# #polynomial model, if needed
# plot(z_mulchmod2, which=1)
# plot(z_mulchmod2, which=2)

#report using heteroscedastic corrected standard errors; if homoscedasticity is violated
hc_se <- car::hccm (mulchmod)
lmtest::coeftest(mulchmod,hc_se)

hc_se <- car::hccm (z_mulchmod)
lmtest::coeftest(z_mulchmod,hc_se)


