#' plateau.plot
#'
#' plot a preheat plateau test
#'
#' @param Figures [list] (**required**) WinBUGS simulation result
#' @param alpha [logical] (**with default**)) True if alpha measurements included
#' @param Dose [numeric] or [list] (**required**): doses in seconds
#'
#' @return a plot of a preheat plateau test
#'
#' @importFrom graphics points
#' @importFrom utils getS3method
#'
#' @export
#'
`plateau.plot` <-
function(Figures,alpha=TRUE,Dose) #
	{

Moy<-c(Figures$a$mean$X0,Figures$b$mean$X0,Figures$c$mean$X0,Figures$d$mean$X0)
ectp<-c(Figures$a$sd$X0,Figures$b$sd$X0,Figures$c$sd$X0,Figures$d$sd$X0)
D0<-rvnorm(1,Moy,ectp)
plot.rv<-getS3method("plot","rv")
points.rv<-getS3method("points","rv")

ylim=range(Moy)
if (prod(ylim)<0) {ylim<-ylim*1.2}else {
		if (ylim[1]<c(0)){ylim<-c(min(Moy)*1.2,0)}	else {ylim<-c(0,max(Moy)*1.2)}}

plot.rv(c(250,275,300,325),D0,ylim=ylim,xlab=paste("preheat Temperature (","\U00B0","C",sep=""))

if (alpha==TRUE){
	par(mfrow=c(2,1))

	Moya<-c(Figures$a$mean$Xba,Figures$b$mean$Xba,Figures$c$mean$Xba,Figures$d$mean$Xba)
	ectpa<-c(Figures$a$sd$Xba,Figures$b$sd$Xba,Figures$c$sd$Xba,Figures$d$sd$Xba)
	D0a<-rvnorm(1,Moya,ectpa)
	plot.rv(c(250,275,300,325),D0a,ylim=c(0,max(Dose*1.2,max(Moy,Moya)*1.2)),xlab=paste("preheat Temperature (","\U00B0","C",sep=""),rvcol="green")
	points.rv(c(250,275,300,325),D0)

	plot.rv(Moy,Moya,xlim=c(0,max(Moy)*1.2),ylim=c(0,max(Moya)*1.2),xlab="Palaodose (s beta)",ylab="Dose alpha (s beta)")
	}
}

