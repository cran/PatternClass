buildsurfs <-
function(reps=1000, verbose=TRUE) {

  #--------------------------------------------------------------
  # 
  # TITLE:     buildsurfs()
  # AUTHOR:    TARMO REMMEL
  # DATE:      23 JANUARY 2020
  # CALLS:     CARsimu(), calculate_lsm()
  # CALLED BY: NA
  # NEEDS:     landscapemetrics, raster
  # NOTES:     USED TO BUILD AN ARRAY OF EXPECTED CLASS METRIC
  #            RESULTS ALONG WITH THEIR VARIABILITY BASED ON
  #            1000 REALIZATIONS ACROSS 9 LEVELS OF THEMATIC
  #            PROPORTION, AND 11 LEVELS OF RHO. RESULTS ARE
  #            STORED FOR 55 CLASS METRICS (x2 CLASS LEVELS).
  #--------------------------------------------------------------

  # SAVE GRAPHIC PARAMETERS AND RESTATE THEM ON EXIT
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))

  # DEFINE BINARY PROPORTION INTERVALS (9 OF THEM)
  propvals <- seq(10,90,by=10)

  # DEFINE RHO INTERVALS (11 OF THEM)
  rhovals <- seq(0,0.2499999, by=0.2499999/10)

  # BUILD AN ARRAY TO STORE RESULTS DIMENSIONS:[METRIC,PROPORTION,RHO,REPLICATE]
  storage <- array(data=NA, dim=c(110,9,11,reps))
  
  # LOOP THROUGH COMBINATIONS AND SIMULATE REPLICATES COMPUTING CLASS METRICS FOR EACH
  for(prop in 1:9) {
    for(rho in 1:11) {
      for(replicate in 1:reps) {
        
        if(verbose) {
          # INDICATE WHICH COMBINATION AND REPLICATE IS CURRENTLY BEING PROCESSED
          cat(propvals[prop], rhovals[rho], replicate, "\n", sep=" ")
        }
        
        # PRODUCE SIMULATED REALIZATION WITH GIVEN RHO AND PROPORTION PARAMETERS
        realizationtemp <- CARsimu(rho = rhovals[rho], rajz = FALSE)
        realization <- quantile(realizationtemp, propvals[prop]/100)
        GARB <- realizationtemp > realization[1]
        GARB <- factor(GARB)
        GARB <- as.numeric(GARB)
        realization <- GARB
        dim(realization) <- c(64,64)

        # COMPUTE AND STORE CLASS METRICS   
        results <- calculate_lsm(raster(realization), level="class")
              
        # WRITE METRICS TO APPROPRIATE ARRAY LOCATION
        storage[,prop,rho,replicate] <- t(as.vector(results[,6]))

        dim(storage) <- c(110,9,11,reps)
        
      } # END FOR: REPLICATE
    } # ENF FOR: RHO 
  } # END FOR: PROP
  
  if(verbose) {
    cat("\nDone.\n")
  }

  storage <- as.numeric(storage)
  storage <- array(storage, c(110,9,11,reps))
  return(storage)
  
}
