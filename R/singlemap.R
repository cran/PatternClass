singlemap <- function(IMG =data$demoimage1, CORRECTIONMAT=data$DIFF50, VERBOSE=TRUE, reps=1, LEVEL=6) {

  #--------------------------------------------------------------
  # 
  # TITLE:     singlemap()
  # AUTHOR:    TARMO REMMEL
  # DATE:      23 January 2020
  # CALLS:     findrow(), findcol(), CARsimu(), calculate_lsm(), wi()
  # CALLED BY: NA
  # NEEDS:     landscapemetrics, raster
  # NOTES:     USED TO PERFORM THE RHO BIAS CORRECTION AND TO 
  #            PRODUCE DATAFRAMES OF CLASS METRIC RESULTS FOR
  #            reps NUMBER OF REALIZATIONS.  RESULTS ARE STORED
  #            IN TWO OBJECTS, ONE WITH METRICS COMPUTED FOR THE
  #            LOWER CLASS VALUE IN THE IMAGE, A SECOND FOR THE
  #            HIGHER CLASS VALUE IN THE IMAGE (E.G., 0 AND 1)
  #            
  #--------------------------------------------------------------

  # SAVE GRAPHIC PARAMETERS AND RESTATE THEM ON EXIT
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))

  # READ THE WHITTLE CORRECTION MATRIX FROM THE APPROPRIATE ENVIRONMENT
  DIFFERENCEMAT <- CORRECTIONMAT

  # COMPUTE THE WHITTLE ESTIMATION OF RHO
  rho <- wi(BE=IMG, CONTROL=TRUE, SIZE=LEVEL)

  # COMPUTE THE ESTIMATED PROPORTION OF THE LOWER CATEGORY VALUE
  
  proportion <- table(IMG)[1]/sum(table(IMG))

  rindex <- findrow(autocorr=rho, DIFFMAT=DIFFERENCEMAT, VERBOSE=FALSE)
  cindex <- findcol(prop=proportion, DIFFMAT=DIFFERENCEMAT, VERBOSE=FALSE)

  # APPLY BIAS CORRECTION AND DEAL WITH SPECIAL CASES OF 99
  if(rindex == 99 | cindex == 99) {
    correctionfactor <- 0
  } # END IF
  else {
    correctionfactor <- DIFFERENCEMAT[rindex, cindex]
  } # END ELSE

  # APPLY BIAS CORRECTION FACTOR
  fixedrho <- rho + correctionfactor
  # IF RHO IS 0.25 OR GREATER, IT EXCEEDS THE LIMIT FOR CARsimu().

  if(fixedrho >= 0.25) {
    fixedrho <- 0.2499999
  } # END IF
 
  # PROVIDE USER FEEDBACK IF REQUESTED
  if(VERBOSE) {
    cat("rho:      ", rho, "\n", sep="")
    cat("adj. rho: ", fixedrho, "\n", sep="")
    cat("True rho: ", fixedrho * 4, "\n", sep="")
  } # END IF

  # NOTE: USE fixedrho IN CARsimu() AS THE R1 AND C1 PARAMETERS
  cat("\n...about to simulate ", reps, " realizations of binary images \nhaving a proportion of ", proportion, " low-value class pixels and a \nspatial autocorrelation parameter of ", fixedrho * 4, ".\n", sep="")

  # PREPARE DATAFRAME TO HOLD METRIC RESULTS FOR EACH REALIZATION
  # COLUMNS ARE METRICS, ROWS ARE REPLICATES
  tab <- as.data.frame(matrix(data=NA, ncol=110, nrow=reps))

  for(a in 1:reps) {
  
    # PROVIDE USER FEEDBACK ON SCREEN
    cat("\nProcessing realization: ", a, "\n", sep="")
    
    # PRODUCE SIMULATED REALIZATION WITH GIVEN RHO AND PROPORTION PARAMETERS
    realizationtemp <- CARsimu(rho = fixedrho, rajz = FALSE)
    realization <- quantile(realizationtemp, proportion)
    GARB <- realizationtemp > realization[1]
    GARB <- factor(GARB)
    GARB <- as.numeric(GARB)
    realization <- GARB
    dim(realization) <- c(64,64)

    # COMPUTE AND STORE THE 55 CLASS METRICS USING PACKAGE: landscapemetrics
    results <- calculate_lsm(raster(realization), level="class")
    tab[a,] <- t(as.vector(results[,6]))

    # ADD COLUMN NAMES IF THIS IS THE FIRST ITERATION
    if(a==1) {
      # ADD NAMES TO THE DATAFRAME TO DIFFERENTIATE THE COLUMNS
      names(tab) <- paste(rep(c("LOW.", "HIGH."),55), t(as.vector(results[,5])))	
    }

  } # END FOR: a


  
  # PROVIDE USER FEEDBACK IF REQUESTED
  if(VERBOSE) {
    cat("\n---------------------------------------\n")
    cat("Summary:\n")
    cat("rho:      ", round(rho, 6), "\n", sep="")
    cat("adj. rho: ", round(fixedrho, 6), "\n", sep="")
    cat("True rho: ", round(fixedrho * 4, 6), "\n", sep="")
    cat("LOW (black): ", table(IMG)[1], " pixels\n", sep="")
    cat("HIGH (white): ", table(IMG)[2], " pixels\n", sep="")
    cat("---------------------------------------\n")
  } # END IF
    
  # RETURN OUTPUTS FROM FUNCTION
  return(tab)

} # END FUNCTION: singlemap
