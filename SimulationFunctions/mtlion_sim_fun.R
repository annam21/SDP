<<<<<<< HEAD
    #  Mountain lion population simulation model
    #  Mukacs Lab
    #  02/2016
################################################################################
    #  Example call - for debugging
    # sims <- sim_pop(
      # nyr = 10, 
      # n1 = matrix(c(30, 30, 
              # 20, 20,    
              # 60, 40,
              # 120, 80), ncol = 2, byrow = T),
      # fphi = matrix(c(0.6, 0.7, 
              # 0.4, 0.6, 
              # 0.7, 0.8, 
              # 0.8, 0.9), ncol = 2, byrow = T),
      # mphi = matrix(c(0.6, 0.7, 
              # 0.4, 0.6, 
              # 0.5, 0.8, 
              # 0.6, 0.9), ncol = 2, byrow = T),
      # fec = c(1, 3))
################################################################################
    #  Function definition
    sim_pop <- function(nyr, n1, fphi, mphi, fec){
      #  A function to simulate mountain lion population dynamics
      #  Takes 
      #   nyr - a single numeric value representing the number of years to
      #     simulate
      #   n1 - a 4 x 2 matrix of population sizes in year 1, rows 
      #    represent the age of the animal while columns are sex
      #   fphi, mphi - a 4 x 2 matrix where the rows represent the 
      #    ageclass of the animals and the columns the lower and upper 
      #    limits of survival, fphi is female, mphi is male
      #   fec - a vector of length 2 (numeric) representing the lower (1)
      #     and upper (2) limits of fecundity (litter size) for the 4th 
      #     age class
      
      #  Returns - sex = 1 when female
      #   N - an array of population sizes [year, age, sex]
      #   PHI - an array of survival terms [year, age, sex]
      #   FEC - a numeric vector of fecundity terms
      
      #  TODO 
      #   add harvest
      #   add process error?
      
      #  Check inputs
      stopifnot(nrow(n1) == 4 && ncol(n1) == 2)
      stopifnot(nrow(fphi) == 4 && ncol(fphi) == 2)
      stopifnot(nrow(mphi) == 4 && ncol(mphi) == 2)      
      stopifnot(length(fec) == 2)
      stopifnot(is.numeric(n1) && is.numeric(fphi) && is.numeric(mphi) &&
        is.numeric(fec))
      
      #  Initialize parameters
      N <- PHI <- array(NA, dim = c(nyr, 4, 2), 
        dimnames = list(Year = 1:nyr, Age = 1:4, 
          Sex = c("Female", "Male")))
      
      #  First year population size
      N[1,,] <- n1

      #  Draw survival values for each year
      #  Female survival
      PHI[,,1] <- sapply(1:4, function(i){
                round(runif(nyr, fphi[i,1], fphi[i,2]), 2)
      })  
      #  Male Survival
      PHI[,,2] <- sapply(1:4, function(i){
                round(runif(nyr, mphi[i,1], mphi[i,2]), 2)
      })  
      
      #  Draw fecundity values for each year    
      FEC <- round(runif(nyr, fec[1], fec[2]), 2)      
=======
	#  Mountain lion population simulation model
		#  Mukacs Lab
		#  02/2016
################################################################################
		#  Example call - for debugging
		# sims <- sim_pop(
			# nyr = 10, 
			# n1 = matrix(c(30, 30, 
						  # 20, 20,		
						  # 60, 40,
						  # 120, 80), ncol = 2, byrow = T),
			# fhm = matrix(c(0.6, 0.7, 
							# 0.4, 0.6, 
							# 0.7, 0.8, 
							# 0.8, 0.9), ncol = 2, byrow = T),
			# mhm = matrix(c(0.6, 0.7, 
							# 0.4, 0.6, 
							# 0.5, 0.8, 
							# 0.6, 0.9), ncol = 2, byrow = T),
			# fec = c(1, 3),
			# om = 0.1
			# )
################################################################################
		#  Function definition
		sim_pop <- function(nyr, n1, fhm, mhm, fec){
			#  A function to simulate mountain lion population dynamics
			#  Takes 
			#   nyr - a single numeric value representing the number of years to
			#     simulate
			#   n1 - a 4 x 2 matrix of population sizes in year 1, rows 
			#    represent the age of the animal while columns are sex
			#   fphi, mphi - a 4 x 2 matrix where the rows represent the 
			#    ageclass of the animals and the columns the lower and upper 
			#    limits of survival, fphi is female, mphi is male
			#   fec - a vector of length 2 (numeric) representing the lower (1)
			#     and upper (2) limits of fecundity (litter size) for the 4th 
			#     age class
			
			#  Returns - sex = 1 when female
			#   N - an array of population sizes [year, age, sex]
			#   PHI - an array of survival terms [year, age, sex]
			#   FEC - a numeric vector of fecundity terms
			
			#  TODO 
			#   add harvest
			#   add process error?
			
			#  Check inputs
			stopifnot(nrow(n1) == 4 && ncol(n1) == 2)
			stopifnot(nrow(fhm) == 4 && ncol(fhm) == 2)
			stopifnot(nrow(mhm) == 4 && ncol(mhm) == 2)			
			stopifnot(length(fec) == 2)
			stopifnot(is.numeric(n1) && is.numeric(fhm) && is.numeric(mhm) &&
				is.numeric(fec))
			
			#  Initialize parameters
			N <- HM <- PHI <- array(NA, dim = c(nyr, 4, 2), 
				dimnames = list(Year = 1:nyr, Age = 1:4, 
					Sex = c("Female", "Male")))
			
			#  First year population size
			N[1,,] <- n1

			#  Draw survival values for each year
			#  Female survival
			HM[,,1] <- sapply(1:4, function(i){
                round(runif(nyr, fhm[i,1], fhm[i,2]), 2)
			})	
			#  Male Survival
			HM[,,2] <- sapply(1:4, function(i){
                round(runif(nyr, mhm[i,1], mhm[i,2]), 2)
			})	

			for(y in 1:nyr){
				for(age in 1:4){
					for(sex in 1:2){
						PHI[y,age,sex] <- 1 - HM[y,age,sex] - om
						}
					}
				}


#stopifnot(phi[y] > 0) 			

			#  Draw fecundity values for each year		
			FEC <- round(runif(nyr, fec[1], fec[2]), 2)			
>>>>>>> origin/master

          #  Loop over years and sexes to grow population
      for(y in 2:nyr){
        for(sex in 1:2){
          #  0-6 months 
          N[y,1,sex] <- round((N[y-1,4,1] * PHI[y-1,4,1] ^ (7/12)) * 
            FEC[y-1] * 0.25 * PHI[y-1,1,sex] ^ (5/12))

          #  6-18 months
          N[y,2,sex] <- N[y-1,1,sex] * PHI[y-1,2,sex]

          #  18-30 months
          N[y,3,sex] <- N[y-1,2,sex] * PHI[y-1,3,sex]

          #  30+ months
          N[y,4,sex] <- (N[y-1,4,sex] + N[y-1,3,sex]) * PHI[y-1,4,sex]       
        }  #  Close sex loop
      } #  Close year loop
    
    list(N = N, PHI = PHI, FEC = FEC)
    }