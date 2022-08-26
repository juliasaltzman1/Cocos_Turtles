# CODED BY: JULIA SALTZMAN 
# LAST UPDATED: AUGUST 21 2022 

# PACKAGES AND SET UP  ----------------------------------------------------
library(lme4)
library(dplyr)
library(sjPlot)
library(ggplot2) 
library(AICcmodavg)

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

# MODELS  -----------------------------------------------------------------

GLOBAL_Turtles_PA = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_PREDATORS = glmer( Turtles_PA ~  ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_LUNAR =  glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY + CHLA + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_SALINITY = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST +  CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_CHLA = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY +  LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Turtles_PA_NO_SST =glmer( Turtles_PA ~ TigerSharks + ONI + Temperature +  SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Turtles_PA_NO_TEMPDEPTH = glmer( Turtles_PA ~ TigerSharks + ONI + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Turtles_PA_NO_CURRENTVIS = glmer( Turtles_PA ~ TigerSharks + ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8  + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


# AIC  --------------------------------------------------------------------


list <- c("GLOBAL_Turtles_PA", "GLOBAL_Turtles_PA_NO_PREDATORS" ,"GLOBAL_Turtles_PA_NO_LUNAR", "GLOBAL_Turtles_PA_NO_SALINITY", "GLOBAL_Turtles_PA_NO_CHLA", "GLOBAL_Turtles_PA_NO_SST", "GLOBAL_Turtles_PA_NO_TEMPDEPTH", "GLOBAL_Turtles_PA_NO_CURRENTVIS")

cand.set <- list(GLOBAL_Turtles_PA, GLOBAL_Turtles_PA_NO_PREDATORS, GLOBAL_Turtles_PA_NO_LUNAR, GLOBAL_Turtles_PA_NO_SALINITY, GLOBAL_Turtles_PA_NO_CHLA, GLOBAL_Turtles_PA_NO_SST, GLOBAL_Turtles_PA_NO_TEMPDEPTH, GLOBAL_Turtles_PA_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)


# SUMMARIZING TOP MODEL  -------------------------------------------------


summary(GLOBAL_Turtles_PA_NO_LUNAR)
plot(GLOBAL_Turtles_PA_NO_LUNAR)
plot_model(GLOBAL_Turtles_PA_NO_LUNAR)
tab_model(GLOBAL_Turtles_PA_NO_LUNAR,  show.intercept = FALSE)

library(ggpubr)

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
