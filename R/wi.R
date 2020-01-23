wi <-
function(BE=data$demoimage1, CONTROL=FALSE, SIZE=6) {

  #--------------------------------------------------------------
  # 
  # TITLE:     wi()
  # AUTHOR:    TARMO REMMEL (FERKO CSILLAG, SANDOR KABOS) 
  # DATE:      23 January 2020
  # CALLS:     NA
  # CALLED BY: wtest.loop()
  # NEEDS:     NA
  # NOTES:     VARIOUS HELPER CODE BITS THAT WORK WITH THE WHITTLE
  #            ESTIMATOR AND BIAS CORRECTOR
  #         
  #            TO RUN ON AN IMAGE WITHOUT THE LOOPING WHITTLE MATERIAL:
  #            rho <- wi(BE=demoimage2, CONTROL=TRUE, SIZE=6)
  #
  #            OTHERWISE, THIS FUNCTION IS CALLED BY wtest.run(), 
  #            WHICH MAY BE CALLED BY build.lut()
  #
  #--------------------------------------------------------------

  # SAVE GRAPHIC PARAMETERS AND RESTATE THEM ON EXIT
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))

  wibi <- function(N=SIZE) {
    KI <- rep(2, N)
    for(n in 2:N) {
      szor <- (2 * (2 * n - 1))/n
      KI[n] <- KI[n - 1] * szor
    }
    KI <- KI^2
    KI <- KI/c(1:N)
    return(list(KI, N))
  }

  wiga <- function(gamma=0.1) {
    g2 <- gamma^2
    KI <- array(dim = wibi(SIZE)[[2]])
    KI[1] <- g2
    for(i in 2:wibi(SIZE)[[2]]) {
      KI[i] <- KI[i - 1] * g2
    }
    KI <- t(KI) %*% wibi(SIZE)[[1]]
    KI <- KI/2
    KI <- KI + log(CC$C0 - CC$C1 * gamma)
    return(KI)
  }

  wicc <- function(BE) {
    NR <- dim(BE)[1]
    NC <- dim(BE)[2]
    BE <- BE - mean(BE)
    BU <- c((BE[c(NR, (1:(NR - 1))),  ]))
    BF <- c((BE[, c((2:NC), 1)]))
    FUGG <- t(as.vector(BE)) %*% as.vector(BU)
    VIZSZ <- t(as.vector(BE)) %*% as.vector(BF)
    ITT <- t(as.vector(BE)) %*% as.vector(BE)
    return(list(C0 = ITT, C1 = 2 * (FUGG + VIZSZ)))
  }

  CC <- wicc(BE)
  CO <- unlist(CC)[1]
  C1 <- unlist(CC)[2]
  KI <- nlminb(0.1, wiga, lower = 0, upper = 0.25)
  if(CONTROL) {
      # REMOVING THIS LINE (WRITE TO FILE) WITH WRITE TO SCREEN
      # write(c("rho , tau2 : ", round(KI$par, 4), round(CC$C0 - CC$C1 * KI$par, 4)))
      cat("rho , tau2 : ", round(KI$par, 4), round(CC$C0 - CC$C1 * KI$par, 4))
  }
  GARB <- list(value = KI$par)

  return(GARB$value)
  
} # END FUNCTION: wi
