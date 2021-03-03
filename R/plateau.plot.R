#' plateau.plot
#'
#' plot a preheat plateau test
#'
#' @param Results [list] (**required**) WinBUGS simulation result
#' @param alpha [logical] (**with default**)) True if alpha measurements included
#' @param Dose [numeric] or [list] (**required**): doses in seconds
#'
#' @return
#' @export
#'
#' @examples
`plateau.plot` <-
function(Results,alpha=TRUE,Dose) #
	{

Moy<-c(Results$a$mean$X0,Results$b$mean$X0,Results$c$mean$X0,Results$d$mean$X0)
ectp<-c(Results$a$sd$X0,Results$b$sd$X0,Results$c$sd$X0,Results$d$sd$X0)
D0<-rvnorm(1,Moy,ectp)

ylim=range(Moy)
if (prod(ylim)<0) {ylim<-ylim*1.2}else {
		if (ylim[1]<c(0)){ylim<-c(min(Moy)*1.2,0)}	else {ylim<-c(0,max(Moy)*1.2)}}

plot.rv(c(250,275,300,325),D0,ylim=ylim,xlab="preheat Temperature (?C)")

if (alpha==TRUE){
	par(mfrow=c(2,1))

	Moya<-c(Results$a$mean$Xba,Results$b$mean$Xba,Results$c$mean$Xba,Results$d$mean$Xba)
	ectpa<-c(Results$a$sd$Xba,Results$b$sd$Xba,Results$c$sd$Xba,Results$d$sd$Xba)
	D0a<-rvnorm(1,Moya,ectpa)
	plot.rv(c(250,275,300,325),D0a,ylim=c(0,max(Dose*1.2,max(Moy,Moya)*1.2)),xlab="preheat Temperature (?C)",rvcol="green")
	points.rv(c(250,275,300,325),D0)

	plot.rv(Moy,Moya,xlim=c(0,max(Moy)*1.2),ylim=c(0,max(Moya)*1.2),xlab="Pal?odose (s beta)",ylab="Dose alpha (s beta)")
	}
}

