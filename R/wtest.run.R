wtest.run <-
function(LEVEL=6, REPSIM=20, RHO=0.2499999, CPROP=0.5, RAJZ=T, CIM="CIM", ENV="data") {

  #--------------------------------------------------------------
  # 
  # TITLE:     wtest.run()
  # AUTHOR:    TARMO REMMEL (FERKO CSILLAG)
  # DATE:      23 January 2020
  # CALLS:     wibi(), CARsimu(), wi() --> wicc()
  # CALLED BY: NA
  # NEEDS:     NA
  # NOTES:     THIS PROGRAM USES THE FFT-METHOD TO SIMULATE IMAGES
  #            WITH A GIVEN LEVEL OF RHO.  THEN THE (ISOTROPIC,
  #            FIRST-NEIGHBOUR AUTOCORRELATION PARAMETER IS
  #            ESTIMATED TWICE: (1) ON THE ACTUAL CONTINUOUS
  #            SIMULATED IMAGE, AND (2) ON A "CUT" BINARY IMAGE,
  #            WHERE THE PROPORTION OF WHITE (0) IS DEFINED BY
  #            CPROP USING P. WHITTLE'S (1954!) METHOD (FOR
  #            REGULAR GRIDS) [ON STATIONARY PROCESSES IN THE PLANE.
  #            BIOMETRIKA 41:434-449.] AND THE RESULTS ARE SUMMARIZED.
  #
  #--------------------------------------------------------------

  # SAVE GRAPHIC PARAMETERS AND RESTATE THEM ON EXIT
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))

  wibi <- function(N=LEVEL) {
    KI <- rep(2, N)
    for(n in 2:N) {
      szor <- (2 * (2 * n - 1))/n
      KI[n] <- KI[n - 1] * szor
    } # END FOR: rep
    KI <- KI^2
    KI <- KI/c(1:N)
    return(list(KI, N))
  } # END FUNCTION: wibi

  RESULT <- rep(0, 2 * REPSIM)
  dim(RESULT) <- c(2, REPSIM)

  # SETUP ARRAYS FOR THE WHITTLE ESTIMATION
  #setuparray <- wibi(LEVEL)
  #assign("WIBI", wibi(LEVEL)[[1]], envir=get(ENV))
  #assign("WITRUNC", wibi(LEVEL)[[2]], envir=get(ENV))

  for(lup in 1:REPSIM) {
    W <- CARsimu(rho = RHO, rajz = F)
    RESULT[1, lup] <- wi(BE = W, CONTROL = RAJZ, SIZE=LEVEL)
    TEMP <- quantile(W, CPROP)
    GARB <- W > TEMP[1]
    GARB <- factor(GARB)
    GARB <- as.numeric(GARB)
    W.0 <- GARB
    dim(W.0) <- c(2^LEVEL, 2^LEVEL)
    if(RAJZ) {
      par(mfrow = c(1, 2))
      par(pty = "s")
      image(W)
      image(W.0)
      cat("\r... PRESS ENTER TO CONTINUE (1 TO LET IT RUN)...")
      ans <- readline()
      if(ans == 1)
      RAJZ <- F
    } # END IF
    RESULT[2, lup] <- wi(BE = W.0, CONTROL = RAJZ, SIZE=LEVEL)
    cat("\r                          ", lup, "ITERATION OUT OF:", REPSIM)
  } # END FOR: lup
  RESULT <- unlist(RESULT)
  dim(RESULT) <- c(2, REPSIM)

  # PLOTTING
  par(mfrow = c(1, 1))
  par(pty = "s")
  if(CIM != "") {
    boxplot(RESULT[1,  ], RESULT[2,  ], names = c("ORIGINAL-CONTINUOUS", "BINARY"), ylim = c(0, 0.25))
    mastertitle <- paste("RHO | PROP : ", as.character(RHO), " | ", as.character(CPROP), sep = " ")
    title(mastertitle)
  } # END IF
  
  return(RESULT)
  	
} # END FUNCTION: wtest.run
