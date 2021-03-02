#' analyse.plot
#'
#' @param file  Risoe.BINfileData-class (required): BIN/BINX file
#' @param nom  character (optional) BIN/BINX file name
#' @param ech numeric (required): sample number
#' @param OSL numeric (required): 1: IR-OSL; 2: BL-OSL
#' @param Dose doses in seconds
#' @param ph0 numeric (with default): selected preheat
#' @param signal.integral vector (with default): vector with the limits for the signal integral.
#' @param background.integral vector (with default): vector with the bounds for the background integral.
#' @param NomEch character (optional) sample name
#' @param Unique logical (with default) TRUE if the equivalent dose is undependant from the preheat

#'
#' @return
#' @export
#'
#' @examples
`analyse.plot` <-
function(file=file,nom=nomFile,ech=1,OSL=2,
         Dose=c(0,30,50,70,0,30),
         Unique=FALSE,ph0=seq(1,4),
         NomEch=c(ech1="ech1",ech2="ech2",ech3="ech3",ech4="ech4"),
         signal.integral=seq(6,10),
         background.integral=seq(85,104))
	{

par(mfrow=c(3,2),mar=c(4,4,3,2))
Results<-GrowthCurve.plot(Sn(file=file,ech=ech,OSL=OSL,Dose=Dose,signal.integral=signal.integral,background.integral=background.integral),Unique=Unique,ph0=ph0)
par(mfg=c(3,1))
if (!Unique){
	plateau.plot(Results,Dose,alpha=FALSE)
	}
plot.default(c(0,100), c(0,100), type="n", axes=FALSE,ylab="", xlab="")
text(50,90,nom)
TypOSL<-switch(OSL,"IR-OSL","BL-OSL")
nomEch<-switch(ech,NomEch[1],NomEch[2],NomEch[3],NomEch[4])
text(50,70,TypOSL)
text(50,50,nomEch)
text(50,30,"D?bit source 0,09 Gy/s")

Results
}