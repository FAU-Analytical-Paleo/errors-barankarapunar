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

