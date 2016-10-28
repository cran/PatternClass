imaks <-
function (BE=data$demoimage1, numcol=NULL, LENG=4, colour=FALSE) { 

  #--------------------------------------------------------------
  # 
  # TITLE:     imaks()
  # AUTHOR:    SANDOR KABOS, MODIFIED BY TARMO REMMEL
  # DATE:      26 October 2016
  # CALLS:     NA
  # CALLED BY: NA
  # NEEDS:     IMAGE MATRIX OBJECT
  # NOTES:     PROVIDES PROPER ORIENTATION OF IMAGE FOR VIEWING
  #            LENG CONTROLS THE LABELING ALONG THE X AND Y AXES
  #--------------------------------------------------------------

  cim <- function (x) {
    attr(x,"cim")
  } # END FUNCTION

  sorrev <- function (BEs=BE) { 
    NR<-dim(BEs)[1] 
    NRR<-1:NR 
    REVENR<-rev(NRR) 
    BE[REVENR,] 
  } # END FUNCTION

  NR <- dim(BE)[1] 
  NC <- dim(BE)[2] 
  LENG <- min(LENG,NR,NC) 
  par(pty="s") 
  LR <- NR%/%LENG 
  LNR <- 1:LENG 
  LNR <- LNR * LR 
  LNR <- c(1, LNR) 
  LABR <- 1:NR 
  LABR <- LABR[LNR] 
  LABR <- as.character(LABR) 
  ATR <- seq(0, 1, length=(NR)) 
  ATR <- ATR[LNR] 
  
  if(!colour) {
    # WHEN colour=FALSE, PRODUCE A BLACK & WHITE IMAGE
    COL <- palette(c("black", "white")) 
  } # END IF
  else {
    # IF numcol IS NOT GIVEN IN FUNCTION CALL, SET IT TO THE MAXIMUM NUMBER OF COLOURS IN THE IMAGE
    if(is.null(numcol)) {
      numcol <- length(table(BE))
      COL <- topo.colors(numcol)  
    } # END IF
    else {
      COL <- topo.colors(numcol)
    } # END ELSE
  } # END ELSE
    
  image(t(sorrev(BE)), col=COL, xaxt="n", yaxt="n", axes=FALSE) 
  axis(1, at=ATR, labels=LABR) 
  axis(2, at=(1-ATR), labels=(LABR)) 
  title(cim(BE)) 
  par(pty="m") 
  
} # END FUNCTION: imaks
