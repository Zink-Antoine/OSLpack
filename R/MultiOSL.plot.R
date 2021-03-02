#' MultiOSL.plot
#'
#' @param file
#' @param ech
#' @param OSL
#' @param Dose
#' @param nom
#' @param ph0
#' @param NomEch
#'
#' @return
#' @export
#'
#' @examples
`MultiOSL.plot` <-
function(file=file,ech=1,OSL=2,Dose=c(0,30,50,70,0,30),nom=nomFile,ph0=seq(1,4),NomEch=c(ech1="ech1",ech2="ech2",ech3="ech3",ech4="ech4")) #
	{

	L<-Sn(file=file,ech=ech,OSL=OSL,Dose=Dose)$Lum
	cycle<-length(L)/16 # 6 sans alpha et 7 avec alpha num?rateur = 24 si ph et 16 sans ph
	ph.nom<-c("250?C","275?C","300?C","325?C")
	typOSL<-switch(OSL,"IR-","BL-")
	typOSL2<-switch(OSL,"IRSL","OSL")
	nomEch<-switch(ech,NomEch[1],NomEch[2],NomEch[3],NomEch[4])
	n.x<-mean(file[[1]]@METADATA$NPOINTS[file[[1]]@METADATA$LTYPE==typOSL2])

	par(fig=c(0,1,0.8,1),mar=c(0,1,1,1))
	plot.default(c(0,100), c(0,10), type="n", axes=FALSE,ylab="", xlab="")
	text(50,8,paste("Courbes de ",typOSL,"OSL", nomEch))
	text(10,5,"en rouge, la courbe correspondant au signal arch?ologique",cex=0.7,adj=0)
	text(10,3,"la mesure dure 110s, la simulation optique d?marre au bout de 5s et dure 100s",cex=0.7,adj=0)

	x<-c(0.45,0)
	y<-c(0.4,0)
	for (j in ph0){
		u<-j%%2+1
		v<-trunc(j/3)+1
		F<-c(x[u],x[u]+0.45,y[v],y[v]+0.4)
		par(fig=F,new=TRUE,mar=c(2,2,0,0))

		Y1<-L[OSL,1,j][[1]]
		Y2<-array(dim=5)
		for (i in 1:5){
			Y2[i]<-L[OSL,i+1,j]
			}
		plot(seq(1,n.x),Y1, ylim=c(0,max(max(Y1),max(unlist(Y2)))*1.2),type="l",axes=FALSE,xaxs="i",yaxs="i",col=2) #
		for (i in 1:5){
			lines(seq(1,n.x),Y2[[i]],lty="dashed")
			}
		axis(1,tcl=-0.2,padj=-2,cex.axis=0.7)
		axis(2,tcl=-0.2,padj=2,cex.axis=0.7)
		box()
		title(xlab="dur?e stimulation (s)",ylab="luminescence (cps)",cex.lab=0.7,line=0.7)
		text(80,max(max(Y1),max(unlist(Y2)))*0.9,ph.nom[j],cex=1,adj=0)

		}
}

