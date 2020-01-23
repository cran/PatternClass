doubleplotter <- function(data1=data$result1, data2=data$result2, metric=5) {

  #--------------------------------------------------------------
  # 
  # TITLE:     doubleplotter()
  # AUTHOR:    TARMO REMMEL
  # DATE:      22 JANUARY 2020
  # CALLS:     NA
  # CALLED BY: NA
  # NEEDS:     SDMTools LIBRARY
  # NOTES:     USED TO DRAW A BOXPLOT OF THE RANGE OF EXPECTED
  #            CLASS METRIC VALUES GIVEN A PROPORTION AND A RHO
  #            THAT IS CORRECTED FOR BIAS.  IT IS POSSIBLE TO
  #            ADD THE LANDSCAPE METRIC VALUE TO THE BOXPLOT
  #            FROM THE ORIGINAL IMAGE FROM WHICH THE PARAMETERS
  #            WERE ESTIMATED TO ILLUSTRATE HOW EXPECTED OR
  #            THAT LANDSCAPE IS, GIVEN SIMULATED RESULTS.
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
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))

  # DRAW BOXPLOTS FOR THE TWO SPCIFIED METRICS
  
  # EXTRACT PROPER METRICS FOR EACH MAP RESULT
  map1 <- data1[,metric]
  map2 <- data2[,metric]
  
  # COMBINE BY COLUMNS THE TWO EXTRACTED METRIC VECTORS
  tempobj <- cbind(map1, map2)
  
  # ATTACH LABELS FOR EACH MAP
  dimnames(tempobj)[[2]] <- c("Map 1", "Map 2")
  
  # DRAW THE BOXPLOT
  boxplot(tempobj, notch=FALSE, ylab=names(data1)[metric])
  
} # END FUNCTION: doubleplotter
