#Week 3 Exercise
#Baran Karapunar


# Answer 1
#function converting degrees (d), minutes (m), seconds (s) into radian

{rad <- function(d, m, s){
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

#3a)

#mean, median, standard deviation, and median absolute deviation (MAD) for r and Mo? 

mean(eqs$r)
median(eqs$r)
sd(eqs$r)
mad(eqs$r)

mean(eqs$Mo)
median(eqs$Mo)
sd(eqs$Mo)
mad(eqs$Mo)
