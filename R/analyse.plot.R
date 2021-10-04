#' analyse.plot
#'
#' analyse OSL-SAR measurement at various preheat and plot the result
#'
#' @inheritParams Sn
#' @inheritParams GrowthCurve.plot
#' @param nom  [character] (**optional**) BIN/BINX file name
#' @param NomEch [character] (**optional**) sample name
#'
#' @return several plots
#' @return WinBUg results objects
#' @export
#'
#' @examples
#' data(Anatolian2, envir = environment())
#' file<-Anatolian2$FILE
#'
#'\dontrun{
#' png("Analyse BL SH112482.png",width=1063,height=1500, units = "px", pointsize = 24)
#' analyse.plot(file,ech=1,OSL=2,Dose=c(0,200,250,300,0,200),ph0=c(1,2,3,4),
#'                   Unique=FALSE,NomEch=c("figurine Anatolie SH112482"))
#' dev.off()
#' }
#'
`analyse.plot` <-
function(file,nom="nomFile",
         ech=1,OSL=2,
         Dose=c(0,30,50,70,0,30),
         Unique=FALSE,
         ph0=seq(1,4),
         NomEch=c(ech1="ech1",ech2="ech2",ech3="ech3",ech4="ech4"),
         signal.integral=seq(6,10),
         background.integral=seq(85,104),
         debug=FALSE,
         ...)
	{

par(mfrow=c(3,2),mar=c(4,4,3,2))
Figures<-GrowthCurve.plot(Sn(file=file,ech=ech,OSL=OSL,Dose=Dose,signal.integral=signal.integral,background.integral=background.integral),ph0=ph0,Unique=Unique,debug=debug,...)
par(mfg=c(3,1))
if (!Unique){
	plateau.plot(Figures,Dose,alpha=FALSE)
	}
plot(c(0,100), c(0,100), type="n", axes=FALSE,ylab="", xlab="")
text(50,90,nom)
TypOSL<-switch(OSL,"IR-OSL","BL-OSL")
nomEch<-switch(ech,NomEch[1],NomEch[2],NomEch[3],NomEch[4])
text(50,70,TypOSL)
text(50,50,nomEch)
text(50,30,"D?bit source 0,09 Gy/s")

Figures
}
