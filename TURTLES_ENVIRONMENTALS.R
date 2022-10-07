# CODED BY JULIA SALTZMAN 
# LAST UPDATED OCTOBER 7 2022 
# READ IN DATA ------------------------------------------------------------
Cocos_Final <- read.csv("~/Downloads/Cocos_Final.csv")
COCOS_UNCLEAN<- Cocos_Final
HADLEY_SALINITY <- read_excel("~/Desktop/Graduate_Research/Environmental Data/Data_Set_1/HADLEY_SALINITY.xls")
CHLORO <- read_excel("~/Desktop/Graduate_Research/Environmental Data/Data_Set_1/CHLORO.xls")



# LUNAR CYCLE  ------------------------------------------------------------

library(lunar)

Format_Date <- as.Date(Cocos_Final$Date,format = "%Y-%m-%d")

Cocos_Final$DateNEW = Format_Date

Cocos_Final$Year_Month <- format(Cocos_Final$DateNEW, "%Y/%m")


Cocos_Final$LunarDistance= lunar.distance(as.Date(Cocos_Final$DateNEW), shift = 6)

Cocos_Final$LunarIlluminationMean = lunar.illumination.mean(as.Date(Cocos_Final$DateNEW),shift= 6)

Cocos_Final$LunarPhase4 = lunar.phase(as.Date(Cocos_Final$DateNEW), shift= 6, name = TRUE)


Cocos_Final$LunarPhase8 = lunar.phase(as.Date(Cocos_Final$DateNEW), shift= 6, name = 8)

# JOIN IN SALINITY AND CHLA ------------------------------------------------------------
library(dplyr)

CHLA_Format_Date <- as.Date(CHLORO$DATE, format = "%Y-%m-%d")

CHLORO$Formatted_Date= CHLA_Format_Date

CHLORO$Month <- format(CHLORO$Formatted_Date, "%m")

CHLORO$Year <- format(CHLORO$Formatted_Date, "%Y")

CHLORO$Year_Month <- format(CHLORO$Formatted_Date, "%Y/%m")

SAL_CHL = CHLORO %>% right_join(HADLEY_SALINITY, by = "Year_Month")

cocos_environmental = SAL_CHL %>% inner_join(Cocos_Final, by = "Year_Month")
write.csv(cocos_environmental, "cocos_environmental.csv")


