#' multiplot.OSL
#'
#' OSL glowcurve for various preheat
#'
#' @inheritParams Sn
#' @param nomFile [character] (**with default**) name of the BIN/BINX file
#' @param NumInv [character] (**with default**) the name of the sample (eg. inventory number)
#'
#'
#' @return a page with multiplot
#'
#' @importFrom graphics axis box lines par plot.default text title
#'
#' @export
#'
#' @examples
#' data(Anatolian2, envir = environment())
#' file<-Anatolian2$FILE
#' multiplot.OSL(file,ech=2,OSL=1,Dose=c(0,200,250,300,0,200),ph0=seq(1,4),NumInv=c("SH112482"))
#'
`multiplot.OSL` <-
function(file,ech=1,OSL=2,Dose=c(0,30,50,70,0,30),nomFile="nomFile",ph0=seq(1,4),NumInv=c(ech1="ech1",ech2="ech2",ech3="ech3",ech4="ech4")) #
	{

	L<-Sn(file=file,ech=ech,OSL=OSL,Dose=Dose)$Lum
	cycle<-length(L)/16
	cycleirr<-cycle-1
	ph.nom<-paste(c("250","275","300","325"),"\U00B0","C",sep="")
	typOSL<-switch(OSL,"IR-","BL-")
	typOSL2<-switch(OSL,"IRSL","OSL")
	NumInv<-NumInv[ech]
	n.x<-mean(file[[1]]@METADATA$NPOINTS[file[[1]]@METADATA$LTYPE==typOSL2])

	par(fig=c(0,1,0.8,1),mar=c(0,1,1,1))
	plot.default(c(0,100), c(0,10), type="n", axes=FALSE,ylab="", xlab="")
	text(50,8,paste("Courbes de ",typOSL,"OSL", NumInv))
	text(10,5,"en rouge, la courbe correspondant au signal naturel",cex=0.7,adj=0)
	text(10,3,"la mesure dure 110s, la simulation optique commence au bout de 5s et dure 100s",cex=0.7,adj=0)

	x<-c(0.45,0)
	y<-c(0.4,0)
	for (j in ph0){
		u<-j%%2+1
		v<-trunc(j/3)+1
		F<-c(x[u],x[u]+0.45,y[v],y[v]+0.4)
		par(fig=F,new=TRUE,mar=c(2,2,0,0))

		Y1<-L[OSL,1,j][[1]]
		Y2<-array(dim=cycleirr)
		for (i in 1:cycleirr){
			Y2[i]<-L[OSL,i+1,j]
			}
		plot(seq(1,n.x),Y1, ylim=c(0,max(max(Y1),max(unlist(Y2)))*1.2),type="l",axes=FALSE,xaxs="i",yaxs="i",col=2) #
		for (i in 1:cycleirr){
			lines(seq(1,n.x),Y2[[i]],lty="dashed")
			}
		axis(1,tcl=-0.2,padj=-2,cex.axis=0.7)
		axis(2,tcl=-0.2,padj=2,cex.axis=0.7)
		box()
		title(xlab="stimulation (s)",ylab="luminescence (cps)",cex.lab=0.7,line=0.7)
		text(80,max(max(Y1),max(unlist(Y2)))*0.9,ph.nom[j],cex=1,adj=0)

		}
}

