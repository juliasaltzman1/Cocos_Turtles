# CODED BY: JULIA SALTZMAN 
# LAST UPDATED: AUGUST OCTOBER 7 2022 


# BASED ON THE FOLLOWING VIDEO: https://www.youtube.com/watch?v=YJHCEv6rQ_U 

install.packages("biwavelet")
library(biwavelet)

### y is date, t1 is tigers, t2 is turtles 

y <- cbind(1:300, TURTLES_TIGERS$CHRONO_SORT)
t1 <- cbind(1:300, TURTLES_TIGERS$Mean_Monthly_Tigers)
t2 <- cbind(1:300, TURTLES_TIGERS$Mean_Monthly_Turtles)
t3<- cbind(1:300, TURTLES_TIGERS$TT)

TURTLES_TIGERS$TT <- TURTLES_TIGERS$Mean_Monthly_Tigers /10
nrands= 1000

# CHECK TO SEE IF THERE ARE ANY MISSING VALUES, WAVELET ANALYSIS WILL NOT WORK IF THERE ARE MISSING VALUES 

sum(is.na(t1))
sum(is.na(t2))
sum(is.na(y))

# THERE ARE NO MISSING VALUES... SO YOUR DATA IS READY FOR PARTIAL WAVELET ANALYSIS 

pwc <- pwtc(y, t1, t2, nrands = nrands)

plot(pwc, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",  lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.08,  ylab = "Scale", xlab = "Period", plot.cb=TRUE) 

plot.biwavelet(pwc, plot.phase = TRUE, col.coi = "grey",  lwd.coi = 2, lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.08)




plot(pwc, xaxt = 'n', lty.coi = 1, col.coi="black", lwd.coi = 4, lwd.sig = 2, ylab = "Scale", xlab = "Study Month", plot.cb = T, main = "PWC: X vs. Y | Z")
