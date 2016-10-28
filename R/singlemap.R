singlemap <-
function(IMG = data$demoimage1, CORRECTIONMAT="DIFF50", ENV="WhittleData", VERBOSE=TRUE, reps=10, LEVEL=6) {

  #--------------------------------------------------------------
  # 
  # TITLE:     singlemap()
  # AUTHOR:    TARMO REMMEL
  # DATE:      26 October 2016
  # CALLS:     findrow(), findcol(), CARsimu(), ClassStat(), wi()
  # CALLED BY: NA
  # NEEDS:     SDMTools LIBRARY
  # NOTES:     USED TO PERFORM THE RHO BIAS CORRECTION AND TO 
  #            PRODUCE DATAFRAMES OF CLASS METRIC RESULTS FOR
  #            reps NUMBER OF REALIZATIONS.  RESULTS ARE STORED
  #            IN TWO OBJECTS, ONE WITH METRICS COMPUTED FOR THE
  #            LOWER CLASS VALUE IN THE IMAGE, A SECOND FOR THE
  #            HIGHER CLASS VALUE IN THE IMAGE (E.G., 0 AND 1)
  #--------------------------------------------------------------

  # READ THE WHITTLE CORRECTION MATRIX FROM THE APPROPRIATE ENVIRONMENT
  DIFFERENCEMAT <- get(CORRECTIONMAT, envir=get(ENV))

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
  if(fixedrho >= 0.25) {
    fixedrho <- 0.12499975
  } # END IF
 
  # PROVIDE USER FEEDBACK IF REQUESTED
  if(VERBOSE) {
    cat("rho:      ", rho, "\n", sep="")
    cat("adj. rho: ", fixedrho, "\n", sep="")
    cat("True rho: ", fixedrho * 4, "\n", sep="")
  } # END IF

  # NOTE: USE fixedrho IN CARsimu() AS THE R1 AND C1 PARAMETERS
  cat("\n...about to simulate ", reps, " realizations of binary images \nhaving a proportion of ", proportion, " low-value class pixels and a \nspatial autocorrelation parameter of ", fixedrho * 4, ".\n", sep="")

  # PREPARE DATAFRAMES TO HOLD LPI RESULTS FOR EACH REALIZATION
  lowclass <- as.data.frame(matrix(data = NA, nrow = reps, ncol = 38, byrow = FALSE,dimnames=NULL))
  highclass <- as.data.frame(matrix(data = NA, nrow = reps, ncol = 38, byrow = FALSE,dimnames=NULL))
  
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

    # COMPUTE AND STORE CLASS METRICS   
    results <- ClassStat(realization)
    lowclass[a,] <- results[1,]
    highclass[a,] <- results[2,]
  
  } # END FOR: a

  # ADD NAMES TO THE DATAFRAMES TO DIFFERENTIATE THE VARIOUS METRICS
  colnames(lowclass) <- colnames(results)
  colnames(highclass) <- colnames(results)

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
    
  # RETURN OUTPUTS FROM FUNCTION AS A LIST
  return(list=c(LOW=lowclass,HIGH=highclass))

} # END FUNCTION: singlemap
