findcol <-
function(prop = 0.32, DIFFMAT=data$DIFF50, VERBOSE=FALSE) {

  #--------------------------------------------------------------
  # 
  # TITLE:     findcol()
  # AUTHOR:    TARMO REMMEL
  # DATE:      26 October 2016
  # CALLS:     N/A
  # CALLED BY: singlemap()
  # NEEDS:     LOOKUP TALBE DIFF50 FOR BIAS CORRECTION AND
  #            A PROPORTION VALUE TO LOOK UP
  # NOTES:     USED TO FIND THE CLOSEST VALUE IN THE DIFF50
  #            LOOKUP TABLE MATRIX FOR RHO BIAS CORRECTION
  #            USED IN CONJUNCTION WITH findrow()
  #--------------------------------------------------------------


  # RUN ONLY IF THE PROPORTION IS > 0.1 and < 0.9
  if(prop > 0.1 & prop < 0.9) {

    # IDENTIFY WHICH COLUMN BEST REPRESENTS THE PROPORTION GIVEN THE PROPORTION (IN THIS CASE prop)
    mincol <- max(which(colnames(DIFFMAT) < prop))
    maxcol <- min(which(colnames(DIFFMAT) > prop))

    val1 <- abs(as.numeric(colnames(DIFFMAT)[mincol]) - prop)
    val2 <- abs(as.numeric(colnames(DIFFMAT)[maxcol]) - prop)

    if(VERBOSE) {
      cat("mincol: ", mincol, "\n", sep="")    
      cat("maxcol: ", maxcol, "\n", sep="")
      cat("val1: ", val1, "\n", sep="")    
      cat("val2: ", val2, "\n", sep="")
    } # END IF

    if(val1 < val2) col <- as.numeric(mincol) else col <- as.numeric(maxcol)

  } # END IF
  else {
    # A SPECIAL CASE, INDICATING THAT THE CORRECTION IS NEGLIGIBLE AT THIS LEVEL
    col <- 99
  } # END ELSE

  return(col)

} # END FUNCTION: findcol
