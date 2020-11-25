#Week 3 Exercise
#Baran Karapunar


# Answer 1


{rad <- function(d, m, s){ #function converting degrees (d), minutes (m), seconds (s) into radian
  radi <- (pi/180)*(d+(m/60)+(s/3600))
    return(radi)
  
}

rad_ans <- rad (1, 21, 0)
rad_err <- rad (0, 1, 0) #error of radian

h_build <- round(2550*tan(rad_ans), 4) #height of the building

err_build <- round((25/2550)*(tan(rad_err)/tan(rad_ans)), 4)

print(paste0("The height of the building is ", h_build, " +- ", err_build, " meters"))}

# Answer 2

print(paste0("The longevity of the volcanic activity is ", 29.66-25.53, " +- ", 0.1+0.2, " My"))

# Answer 3

eqs <- read.delim("C:/Users/Baran/Desktop/Computer in Geosciences Course/Week3_Errors/Exercise/ex3_eqscals.txt", sep = "", header = F)

colnames(eqs) <- c("x", "r", "Mo") # "X(km)", "r(m)", "Mo(Nm)"

#3a)mean, median, standard deviation, and median absolute deviation (MAD) for r and Mo? 

mean(eqs$r)
median(eqs$r)
sd(eqs$r)
mad(eqs$r)

mean(eqs$Mo)
median(eqs$Mo)
sd(eqs$Mo)
mad(eqs$Mo)

#b) Make one descriptive plot each, of Mo and r (eg. histograms, boxplots, scatter, logged etc). 
#Are there any obvious outliers that can cause problems?

boxplot(eqs$Mo)
boxplot(eqs$r)

#c) Are there any outliers apparent, either from looking at the numbers or plotting? 
#A good criterion is eliminating points exceeding 3*MAD from the median. 
#Eliminate them to make a TRIMMED set, and recalculate the mean, median and standard deviation. 

ss1 <- subset(eqs[eqs$Mo < median(eqs$Mo)+3*mad(eqs$Mo),])

ss2 <- subset(ss1[ss1$Mo > median(eqs$Mo)-3*mad(eqs$Mo),])

mean(ss2$Mo)
median(ss2$Mo)
sd(ss2$Mo)

ss1 <- subset(eqs[eqs$r < median(eqs$r)+(3*mad(eqs$r)),])

ss2 <- subset(ss1[ss1$r > median(eqs$r)-(3*mad(eqs$r)),])

mean(ss2$r)
median(ss2$r)
sd(ss2$r)

print(paste0("No outliers found"))

#What is your “best” estimate, and uncertainty, in Mo?

error <- qnorm(0.975)*((sd(eqs$Mo))/sqrt(length(eqs$Mo))) # 95% confidence interval

print(paste0(mean(eqs$Mo), " +- ", error))

#d)The “moment” magnitude of an earthquake is calculated from the equation

Mw <- (log10(mean(eqs$Mo))/1.5)-6
Mw_e <- (log10(error)/1.5)-6

print(paste0(Mw, " +- ", Mw_e))
  