#' fonction affichant une page d'analyse
#'
#' @param File fichier Bin
#' @param ech numero echantillon
#' @param Dose doses en secondes
#' @param ph0 prechauffe
#' @param NomEch nom de l echantillon
#' @param Unique TRUE si la dose equivalente est independant des prechauffe
`analyse.page` <-
function(File=File,ech=seq(1,2),Dose=c(0,30,50,70,0,30),ph0=seq(1,4),NomEch=c(ech1="ech1",ech2="ech2",ech3="ech3",ech4="ech4"),Unique=FALSE) {
	file<-File$FILE
	nomFile<-File$NFILE

	for (j in ech){
		for (i in 2:2){
			titre<-paste(switch(i,"IR-","BL-"),"OSL",switch(j,NomEch[1],NomEch[2],NomEch[3],NomEch[4]))

			png(paste("growth curve ",titre,".png"),width=1063,height=1450, units = "px", pointsize = 24)
			analyse.plot(file,ech=j,OSL=i,nom=nomFile[[1]][1],NomEch=NomEch,Unique=Unique,Dose=Dose)
			dev.off()

			#png(paste("courbes ",titre,".png"),width=1063,height=1450, units = "px", pointsize = 24)
			#MultiOSL.plot(file,ech=j,OSL=i,NomEch=NomEch)
			#dev.off()
			}
		}
}
