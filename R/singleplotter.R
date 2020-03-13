singleplotter <- function(dat=data$result1, img=data$demoimage1, metrics=c(1,5,10), rows=1, cols=3, addactual=TRUE, colour=TRUE) {

  #--------------------------------------------------------------
  # 
  # TITLE:     singleplotter()
  # AUTHOR:    TARMO REMMEL
  # DATE:      23 January 2020
  # CALLS:     calculate_lsm(), raster()
  # CALLED BY: NA
  # NEEDS:     landscapemetrics, raster
  # NOTES:     USED TO DRAW A BOXPLOT OF THE RANGE OF EXPECTED
  #            CLASS METRIC VALUES GIVEN A PROPORTION AND A RHO
  #            THAT IS CORRECTED FOR BIAS.  IT IS POSSIBLE TO
  #            ADD THE LANDSCAPE METRIC VALUE TO THE BOXPLOT
  #            FROM THE ORIGINAL IMAGE FROM WHICH THE PARAMETERS
  #            WERE ESTIMATED TO ILLUSTRATE HOW EXPECTED OR
  #            THAT LANDSCAPE IS, GIVEN SIMULATED RESULTS.
  #            SETTING colour=FALSE RESULTS IN A BW PLOT
  # USAGE:     singleplotter(data=result1, metrics=c(2,7,18,20,21,22), rows=2, cols=3, addactual=TRUE)
  #
  #--------------------------------------------------------------
     
  # METRICS BY CLASS (FOR SELECTING WHICH ONES TO PLOT)
  # [1] "LOW.ai"         "HIGH.ai"        "LOW.area_cv"    "HIGH.area_cv"   "LOW.area_mn"    "HIGH.area_mn"   "LOW.area_sd"   
  # [8] "HIGH.area_sd"   "LOW.ca"         "HIGH.ca"        "LOW.cai_cv"     "HIGH.cai_cv"    "LOW.cai_mn"     "HIGH.cai_mn"   
  # [15] "LOW.cai_sd"     "HIGH.cai_sd"    "LOW.circle_cv"  "HIGH.circle_cv" "LOW.circle_mn"  "HIGH.circle_mn" "LOW.circle_sd" 
  # [22] "HIGH.circle_sd" "LOW.clumpy"     "HIGH.clumpy"    "LOW.cohesion"   "HIGH.cohesion"  "LOW.contig_cv"  "HIGH.contig_cv"
  # [29] "LOW.contig_mn"  "HIGH.contig_mn" "LOW.contig_sd"  "HIGH.contig_sd" "LOW.core_cv"    "HIGH.core_cv"   "LOW.core_mn"   
  # [36] "HIGH.core_mn"   "LOW.core_sd"    "HIGH.core_sd"   "LOW.cpland"     "HIGH.cpland"    "LOW.dcad"       "HIGH.dcad"     
  # [43] "LOW.dcore_cv"   "HIGH.dcore_cv"  "LOW.dcore_mn"   "HIGH.dcore_mn"  "LOW.dcore_sd"   "HIGH.dcore_sd"  "LOW.division"  
  # [50] "HIGH.division"  "LOW.ed"         "HIGH.ed"        "LOW.enn_cv"     "HIGH.enn_cv"    "LOW.enn_mn"     "HIGH.enn_mn"   
  # [57] "LOW.enn_sd"     "HIGH.enn_sd"    "LOW.frac_cv"    "HIGH.frac_cv"   "LOW.frac_mn"    "HIGH.frac_mn"   "LOW.frac_sd"   
  # [64] "HIGH.frac_sd"   "LOW.gyrate_cv"  "HIGH.gyrate_cv" "LOW.gyrate_mn"  "HIGH.gyrate_mn" "LOW.gyrate_sd"  "HIGH.gyrate_sd"
  # [71] "LOW.iji"        "HIGH.iji"       "LOW.lpi"        "HIGH.lpi"       "LOW.lsi"        "HIGH.lsi"       "LOW.mesh"      
  # [78] "HIGH.mesh"      "LOW.ndca"       "HIGH.ndca"      "LOW.nlsi"       "HIGH.nlsi"      "LOW.np"         "HIGH.np"       
  # [85] "LOW.pafrac"     "HIGH.pafrac"    "LOW.para_cv"    "HIGH.para_cv"   "LOW.para_mn"    "HIGH.para_mn"   "LOW.para_sd"   
  # [92] "HIGH.para_sd"   "LOW.pd"         "HIGH.pd"        "LOW.pladj"      "HIGH.pladj"     "LOW.pland"      "HIGH.pland"    
  # [99] "LOW.shape_cv"   "HIGH.shape_cv"  "LOW.shape_mn"   "HIGH.shape_mn"  "LOW.shape_sd"   "HIGH.shape_sd"  "LOW.split"     
  # [106] "HIGH.split"     "LOW.tca"        "HIGH.tca"       "LOW.te"         "HIGH.te"       

  # SAVE GRAPHIC PARAMETERS AND RESTATE THEM ON EXIT
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))

  # COMPUTE ACTUAL CLASS METRICS
  actual <- calculate_lsm(landscape=raster(img), level="class")
  actual2 <- as.data.frame(t(as.vector(actual[,6])))
  actual <- actual2
  names(actual) <- names(dat)

  # SETUP GRAPHIC ENVIRONMENT
  plot.new()   
  par(mfrow=c(rows,cols), pty="s")
 
  for(num in 1:length(metrics)) {
    if(addactual==TRUE) {
      # DEFINE ylim VALUES TO FORCE RANGE THAT INCLUDES ACTUAL IF OUTSIDE OF SIMULATED RANGE
      # STORE MIN AND MAX FOR SIMULATED RANGE
      ymin <- min(dat[[metrics[num]]], na.rm=TRUE)
      ymax <- max(dat[[metrics[num]]], na.rm=TRUE)
      # NOW, ADJUST FOR ACTUAL VALUE IF NECESSARY
      if(actual[1,metrics[num]] > ymax) {
        ymax <- actual[1,metrics[num]]
      } # END IF
      if(actual[1,metrics[num]] < ymin) {
        ymin <- actual[1,metrics[num]]
      } # END IF
      boxplot(dat[metrics[num]], ylim=c(ymin,ymax))
    } # END IF
    else {
      boxplot(dat[metrics[num]])
    } # END ELSE
    title(names(dat[metrics[num]]))
    
    # ADD DATA POINT FOR ACTUAL LPI VALUE IF DESIRED
    if(addactual == TRUE) {
       if(metrics[num] < 110) {  # DO NOT ALLOW PLOTTING OF METRICS THAT DO NOT EXIST
         if(colour) {
           # THIS IS THE NUMBER OF SIMULATED MAPS
           numsim <- length(dat[[metrics[num]]])
           points(actual[1,metrics[num]], pch=21, col="red", bg="red", cex=1.75)
           actualval <- actual[1,metrics[num]]
           cat("Actual Metric Value (", names(dat[metrics[num]]), "): ", actualval, "\n")
           higherthan <- sum(dat[[metrics[num]]] > actualval)
           if(is.na(higherthan)) { higherthan <- 0 }
           lowerthan <- sum(dat[[metrics[num]]] < actualval)
           if(is.na(lowerthan)) { lowerthan <- 0 }
           sameas <- numsim - lowerthan - higherthan
           probhighereq <- (higherthan + sameas) / numsim
           problowereq <- (lowerthan + sameas) / numsim
           cat(higherthan, " higher values, ", lowerthan, " lower values, and ", sameas, " identical values as the map.\n", sep="")
           cat("Probability of map having a value <= to expectation: P=", formatC(probhighereq, digits=4, format="f"), "\n", sep="")
           cat("Probability of map having a value >= to expectation: P=", formatC(problowereq, digits=4, format="f"), "\n\n", sep="")
         } # END IF
         else {
           # THIS IS THE NUMBER OF SIMULATED MAPS
           numsim <- length(dat[[metrics[num]]])
           points(actual[1,metrics[num]], pch=21, cex=1.75)
           actualval <- actual[1,metrics[num]]
           cat("Actual Metric Value (", names(dat[metrics[num]]), "): ", actualval, "\n")
           higherthan <- sum(dat[[metrics[num]]] > actualval)
           if(is.na(higherthan)) { higherthan <- 0 }
           lowerthan <- sum(dat[[metrics[num]]] < actualval)
           if(is.na(lowerthan)) { lowerthan <- 0 }
           sameas <- numsim - lowerthan - higherthan
           probhighereq <- (higherthan + sameas) / numsim
           problowereq <- (lowerthan + sameas) / numsim
           cat(higherthan, " higher values, ", lowerthan, " lower values, and ", sameas, " identical values as the map. \n", sep="")
           cat("Probability of map having a value <= to expectation: P=", formatC(probhighereq, digits=4, format="f"), "\n", sep="")
           cat("Probability of map having a value >= to expectation: P=", formatC(problowereq, digits=4, format="f"), "\n\n", sep="")
         } # END ELSE
       } # END IF
       else {
         if(colour) {
           # THIS IS THE NUMBER OF SIMULATED MAPS
           numsim <- length(dat[[metrics[num]]])
           points(actual[2,metrics[num] - 38], pch=21, col="red", bg="red", cex=1.75)
           actualval <- actual[2,metrics[num] - 38]
           cat("Actual Metric Value (", names(dat[metrics[num]]), "): ", actualval, "\n")
           higherthan <- sum(dat[[metrics[num]]] > actualval)
           if(is.na(higherthan)) { higherthan <- 0 }
           lowerthan <- sum(dat[[metrics[num]]] < actualval)
           if(is.na(lowerthan)) { lowerthan <- 0 }
           sameas <- numsim - lowerthan - higherthan
           probhighereq <- (higherthan + sameas) / numsim
           problowereq <- (lowerthan + sameas) / numsim
           cat(higherthan, " higher values, ", lowerthan, " lower values, and ", sameas, " identical values as the map. \n", sep="")
           cat("Probability of map having a value <= to expectation: P=", formatC(probhighereq, digits=4, format="f"), "\n", sep="")
           cat("Probability of map having a value >= to expectation: P=", formatC(problowereq, digits=4, format="f"), "\n\n", sep="")
         } # END IF
         else {
           # THIS IS THE NUMBER OF SIMULATED MAPS
           numsim <- length(dat[[metrics[num]]])
           points(actual[2,metrics[num] - 38], pch=21, cex=1.75)
           actualval <- actual[2,metrics[num] - 38]
           cat("Actual Metric Value (", names(dat[metrics[num]]), "): ", actualval, "\n")
           higherthan <- sum(dat[[metrics[num]]] > actualval)
           if(is.na(higherthan)) { higherthan <- 0 }
           lowerthan <- sum(dat[[metrics[num]]] < actualval)
           if(is.na(lowerthan)) { lowerthan <- 0 }
           sameas <- numsim - lowerthan - higherthan
           probhighereq <- (higherthan + sameas) / numsim
           problowereq <- (lowerthan + sameas) / numsim
           cat(higherthan, " higher values, ", lowerthan, " lower values, and ", sameas, " identical values as the map. \n", sep="")
           cat("Probability of map having a value <= to expectation: P=", formatC(probhighereq, digits=4, format="f"), "\n", sep="")
           cat("Probability of map having a value >= to expectation: P=", formatC(problowereq, digits=4, format="f"), "\n\n", sep="")
         } # END ELSE
       } # END IF
    } # END ELSE
    
  } # END FOR: num
  
} # END FUNCTION: singleplotter
