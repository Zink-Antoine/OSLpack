#' OSLpack
#'
#'  Various function to analyze and plot TL measurement using MCMC code based on Gibbs and Slice samplers.
#'
#' @docType package
#' @name <OSLpack>
#' @import rv
#' @import R2WinBUGS
#' @import Luminescence
#' @importFrom graphics axis box lines par plot plot.default points text title
#' @importFrom grDevices png dev.off
#' @importFrom stats lm var
#' @importFrom tcltk tk_choose.dir
#' @importFrom utils getS3method
#'

NULL

#' OSL data of Anatolian figurine (genuine)
#'
#' An example of actual data from a OSL measurement
#'
#' This data set gives the OSL glow curve of SH112481
#'
#' @docType data
#' @keywords datasets
#' @name Anatolian1
#' @usage data(Anatolian1)
#' @format A Risoe.BINFileData object
#' @source 1203214.binx OSL SAR SH112481, SH112482 {N; 200sb; 250sb; 300sb; 0sb; 200sb}; 1503211.binx OSL SAR SH112481{100sb}
#'

NULL

#' OSL data of Anatolian figurine (fake)
#'
#' An example of actual data from a OSL measurement
#'
#' This data set gives the OSL glow curve of SH112482
#'
#' @docType data
#' @keywords datasets
#' @name Anatolian2
#' @usage data(Anatolian2)
#' @format A Risoe.BINFileData object
#' @source 1203214.binx TL OSL SAR SH112481, SH112482 {N; 200sb; 250sb; 300sb; 0sb; 200sb}
#'

NULL
