# CODED BY: JULIA SALTZMAN 
# LAST UPDATED: AUGUST 21 2022 

# PACKAGES AND SET UP  ----------------------------------------------------
library(lme4)
library(dplyr)
library(sjPlot)
library(ggplot2) 
library(AICcmodavg)
library(ggpubr)

# LEARN MORE ABOUT LME4: https://cran.r-project.org/web/packages/lme4/lme4.pdf 
# LEARN MORE ABOUT DPLYR: https://dplyr.tidyverse.org/
# LEARN MORE ABOUT SJPLOT: https://strengejacke.github.io/sjPlot/ 
# LEARN MORE ABOUT GGPLOT: https://ggplot2.tidyverse.org/
# INSPIRATION FOR DATA VISUALIZATION: data-to-viz.com 
# LEARN MORE ABOUT AICCMODAVG: https://cran.r-project.org/web/packages/AICcmodavg/AICcmodavg.pdf 

# CONVERT DATA INTO P/A  --------------------------------------


cocos_environmental$Turtles_PA <- ifelse(cocos_environmental$Turtles > 0, "1", "0")
cocos_environmental$Turtles_PA <- as.numeric(cocos_environmental$Turtles_PA)

cocos_environmental$Tigers_PA <- ifelse(cocos_environmental$TigerSharks > 0, "1", "0")
cocos_environmental$Tigers_PA <- as.numeric(cocos_environmental$Tigers_PA)

# PA MODELS  -----------------------------------------------------------------

GLOBAL_Turtles_PA = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_PREDATORS = glmer( Turtles_PA ~  ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_LUNAR =  glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY + CHLA + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_SALINITY = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST +  CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_CHLA = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY +  LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Turtles_PA_NO_SST =glmer( Turtles_PA ~ TigerSharks + ONI + Temperature +  SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Turtles_PA_NO_TEMPDEPTH = glmer( Turtles_PA ~ TigerSharks + ONI + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_CURRENTVIS = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8  + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


# PA AIC  --------------------------------------------------------------------


list <- c("GLOBAL_Turtles_PA", "GLOBAL_Turtles_PA_NO_PREDATORS" ,"GLOBAL_Turtles_PA_NO_LUNAR", "GLOBAL_Turtles_PA_NO_SALINITY", "GLOBAL_Turtles_PA_NO_CHLA", "GLOBAL_Turtles_PA_NO_SST", "GLOBAL_Turtles_PA_NO_TEMPDEPTH", "GLOBAL_Turtles_PA_NO_CURRENTVIS")

cand.set <- list(GLOBAL_Turtles_PA, GLOBAL_Turtles_PA_NO_PREDATORS, GLOBAL_Turtles_PA_NO_LUNAR, GLOBAL_Turtles_PA_NO_SALINITY, GLOBAL_Turtles_PA_NO_CHLA, GLOBAL_Turtles_PA_NO_SST, GLOBAL_Turtles_PA_NO_TEMPDEPTH, GLOBAL_Turtles_PA_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)


# SUMMARIZING TOP PA MODEL  -------------------------------------------------


summary(GLOBAL_Turtles_PA_NO_LUNAR)
plot(GLOBAL_Turtles_PA_NO_LUNAR)
plot_model(GLOBAL_Turtles_PA_NO_LUNAR)
tab_model(GLOBAL_Turtles_PA_NO_LUNAR)


# PA MODEL FIGURES --------------------------------------------------------


tigers <- plot_model(GLOBAL_Turtles_PA_NO_LUNAR, type=("pred"), terms = "TigerSharks")+  ylab("Probablity of Surveying Turtle on Dive") + xlab("Tiger Shark Count")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") + xlim(0,5)

years <- plot_model(GLOBAL_Turtles_PA_NO_LUNAR, type=("pred"), terms = "Year.y")+  ylab("Probablity of Surveying Turtle on Dive") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 

ONI <- plot_model(GLOBAL_Turtles_PA_NO_LUNAR, type=("pred"), terms = "ONI")+  ylab("Probablity of Surveying Turtle on Dive") + xlab("ONI")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 

TEMPERATURE <- plot_model(GLOBAL_Turtles_PA_NO_LUNAR, type=("pred"), terms = "Temperature")+  ylab("Probablity of Surveying Turtle on Dive") + xlab("Temperature at Depth")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") + xlim(20,35)

SST <- plot_model(GLOBAL_Turtles_PA_NO_LUNAR, type=("pred"), terms = "SST")+  ylab("Probablity of Surveying Turtle on Dive") + xlab("SST")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 

CHLA <- plot_model(GLOBAL_Turtles_PA_NO_LUNAR, type=("pred"), terms = "CHLA")+  ylab("Probablity of Surveying Turtle on Dive") + xlab("Chlorophyll A")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 

ggarrange(tigers, years, ONI, SST, CHLA)

range(cocos_environmental$Temperature, na.rm = "TRUE")

ggplot(cocos_environmental, aes(Turtles,SST)) + geom_smooth()

ggplot(cocos_environmental, aes(StudyJulianDate, Turtles)) + geom_smooth()

range(cocos_environmental$TigerSharks, na.rm = TRUE,.05, .95)
quantile(cocos_environmental$TigerSharks, probs=c(.025,.999),  na.rm = TRUE)

tab_model(GLOBAL_Turtles_PA_NO_LUNAR, show.intercept = FALSE)

# COUNT MODELS  -----------------------------------------------------------

GLOBAL_TURTS_ZINB <- glmmTMB(Turtles ~ TigerSharks + SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_TURTS_ZINB_NO_PREDATORS <- glmmTMB(Turtles ~ SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_TURTS_ZINB_NO_LUNAR <- glmmTMB(Turtles ~ TigerSharks + SST + Temperature  + SALINITY + CHLA + ONI +  CurrentCode + Visibility + SIN_TIME + COS_TIME + Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_TURTS_ZINB_NO_SALINITY <- glmmTMB(Turtles ~ TigerSharks + SST + Temperature  + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_TURTS_ZINB_NO_CHLA <- glmmTMB(Turtles ~ TigerSharks + SST + Temperature  + SALINITY + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_TURTS_ZINB_NO_SST <- glmmTMB(Turtles ~ TigerSharks + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_TURTS_ZINB_NO_TEMPDEPTH <- glmmTMB(Turtles ~ TigerSharks + SST   + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_TURTS_ZINB_NO_CURRENT_VIS <- glmmTMB(Turtles ~ TigerSharks + SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 +  SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)


# SUMMARIZNG TOP COUNT MODEL -----------------------------------------------------


summary(GLOBAL_TURTS_ZINB_NO_CURRENT_VIS)
plot(GLOBAL_TURTS_ZINB_NO_CURRENT_VIS)
plot_model(GLOBAL_TURTS_ZINB_NO_CURRENT_VIS)
tab_model(GLOBAL_TURTS_ZINB_NO_CURRENT_VIS)

# COUNT MODEL AIC ---------------------------------------------------------

list <- c("GLOBAL_TURTS_ZINB", "GLOBAL_TURTS_ZINB_NO_PREDATORS" ,"GLOBAL_TURTS_ZINB_NO_LUNAR", "GLOBAL_TURTS_ZINB_NO_SALINITY", "GLOBAL_TURTS_ZINB_NO_CHLA", "GLOBAL_TURTS_ZINB_PA_NO_SST", "GLOBAL_TURTS_ZINB_PA_NO_TEMPDEPTH", "GLOBAL_TURTS_ZINB_NO_CURRENT_VIS")

cand.set <- list(GLOBAL_TURTS_ZINB, GLOBAL_TURTS_ZINB_NO_PREDATORS, GLOBAL_TURTS_ZINB_NO_LUNAR, GLOBAL_TURTS_ZINB_NO_SALINITY, GLOBAL_TURTS_ZINB_NO_CHLA, GLOBAL_TURTS_ZINB_NO_SST, GLOBAL_TURTS_ZINB_NO_TEMPDEPTH, GLOBAL_TURTS_ZINB_NO_CURRENT_VIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)

tab_model(GLOBAL_TURTS_ZINB_NO_LUNAR, show.intercept = FALSE, show.zeroinf = FALSE)


summary(GLOBAL_TURTS_ZINB_NO_LUNAR)
exp( -2.319859 *.10)
summary(GLOBAL_Turtles_PA_NO_LUNAR)
exp( -1.558844  *.10)



# COUNT MODEL FIGURES  ----------------------------------------------------

tigers1 <- plot_model(GLOBAL_TURTS_ZINB_NO_LUNAR, type=("pred"), terms = "TigerSharks")+  ylab("Turtle Count") + xlab("Tiger Shark Count")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") + xlim(0,5)

years1 <- plot_model(GLOBAL_TURTS_ZINB_NO_LUNAR, type=("pred"), terms = "Year.y")+  ylab("Turtle Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 

ONI1 <- plot_model(GLOBAL_TURTS_ZINB_NO_LUNAR, type=("pred"), terms = "ONI")+  ylab("Turtle Count") + xlab("ONI")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 

TEMPERATURE1 <- plot_model(GLOBAL_TURTS_ZINB_NO_LUNAR, type=("pred"), terms = "Temperature")+  ylab("Turtle Count") + xlab("Temperature at Depth")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") + xlim(25,30)+ ylim(0,1)


SST1 <- plot_model(GLOBAL_TURTS_ZINB_NO_LUNAR,type=("pred"), terms = "SST")+  ylab("Turtle Count") + xlab("SST")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 

CHLA1 <- plot_model(GLOBAL_TURTS_ZINB_NO_LUNAR, type=("pred"), terms = "CHLA")+  ylab("Turtle Count") + xlab("Chlorophyll A")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))  + ggtitle("") 


ggarrange(tigers1, years1, ONI1, TEMPERATURE1, SST1, CHLA1)

