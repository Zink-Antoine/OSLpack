############################# fonction Sn ####################################
#' Sn
#'
#' determines the sensitivity values for OSL
#'
#' @param file  [Risoe.BINfileData-class] or [list] (*required*): BIN/BINX file
#' @param ech [numeric] (*required*): sample number
#' @param OSL [numeric] (*required*): 1: IR-OSL; 2: BL-OSL
#' @param Dose [numeric] or [list] (*with default*): doses in seconds
#' @param TypLum [character] or [list] (*with default*): type of luminescence LIR; LBL; TIR; TBL
#' @param Temp [character] or [list] (*with default*): preheat temperature
#' @param ph0 [numeric] (*with default*): selected preheat
#' @param signal.integral [vector] (*with default*): vector with the limits for the signal integral.
#' @param background.integral [vector] (*with default*): vector with the bounds for the background integral.
#'
#' @return a list object with the following items
#' @return $S [data.frame] efficiency S=Lx/Tx values with uncertainties
#' @return $alpha [logical] True if alpha measurements included
#' @return $Lum [matrix] OSL data
#'
#' @export
#'
#' @examples
#' data(Anatolian2, envir = environment())
#' file<-Anatolian2$FILE
#' Sn(file,2,1)
#'
`Sn` <-
function(file,ech,OSL,
         Dose=c(0,50,80,110,0,50),
         TypLum=c("LIR","LBL","TIR","TBL"),
         Temp=c("250","275","300","325"),
         ph0=seq(1,4),
         signal.integral=seq(6,10),
         background.integral=seq(85,104))
	{

	alpha<-FALSE
	nbCycleb<-length(Dose)
	nbCycle<-nbCycleb
	B1<-file[[1]]@DATA[file[[1]]@METADATA$LTYPE!="TL"]
	L<-length(B1)

	if (length(file)==2) {
		alpha<-TRUE
		B2<-file[[2]]@DATA[file[[2]]@METADATA$LTYPE!="TL"]
		L<-L+length(B2)
	#	nbCycle<-nbCycle+1
		}

	cycle0<-2*2 #IRSL+BLSL*{Lx,Tx}
	cycleSAR<-cycle0*nbCycle #ph+IRSL+BLSL*{Lx,Tx}*{nat,b1,b2,b3,0,b1[,a]}
	aliquot<-length(ph0) # number of preheating temperatures
	nech<-L/(aliquot*cycleSAR)
	nbd<-aliquot*nech

	BgSg_ratio<-length(background.integral)/length(signal.integral)

	B1<-array(B1,dim=c(cycle0,nbCycleb,nech,aliquot),dimnames=list(TypLum,Dose,seq(1,nech),Temp[ph0]))
	B<-B1
	if (alpha==TRUE){
		B2<-array(B2,dim=c(cycle0,1,nech,aliquot),dimnames=list(TypLum,"a",seq(1,nech),Temp[ph0]))
		for (j in 1:aliquot){
			for (i in 1:nech){
				if (i==1 && j==1){B<-B1[,,i,j];B<-cbind(B,B2[,,i,j])}
				else
				B<-cbind(B,B1[,,i,j],B2[,,i,j])
				}
			}
		B<-array(B,dim=c(cycle0,nbCycle,nech,aliquot),dimnames=list(TypLum,c(Dose,"a"),seq(1,nech),Temp[ph0]))
		}


	irrx<-switch(OSL,c(TypLum[1],TypLum[3]),c(TypLum[2],TypLum[4]))#OSL=1 => IRSL; OSL=2 => BLOSL
	CycleSARirr<-cycle0*nbCycleb-4 #nb cycle with beta dose only
	Sn<-data.frame(meanY=rep(0,CycleSARirr),sdY=rep(0,CycleSARirr),meanX=rep(Dose[seq(2,nbCycleb)],4),sdX=rep(1,CycleSARirr),meanY0=rep(0,CycleSARirr),sdY0=rep(0,CycleSARirr),meanXa=c(90,90,90,90,rep(0,CycleSARirr-4)),sdXa=c(rep(1,4),rep(0,CycleSARirr-4)),meanYa=rep(0,CycleSARirr),sdYa=rep(0,CycleSARirr))

	for (j in 1:aliquot){
		Lx<-B[irrx[1],,ech,j]
		Tx<-B[irrx[2],,ech,j]
		try(if(unique(lengths(Lx))<max(background.integral)) stop("background.integral out of bound"))

		llx<-Lx
		for (i in 1:nbCycle){
			S<-(sum(Lx[[i]][signal.integral])-(sum(Lx[[i]][background.integral]))/BgSg_ratio)/(sum(Tx[[i]][signal.integral])-(sum(Tx[[i]][background.integral]))/BgSg_ratio)
			if (i==1){Sn$meanY0[j]<-S; Sn$sdY0[j]<-S*0.01}
				else {if(i==7& alpha==TRUE){Sn$meanYa[j]<-S; Sn$sdYa[j]<-S*0.01}
					else {Sn$meanY[(j-1)*nbCycle+i-2*j+1]<-S; Sn$sdY[(j-1)*nbCycle+i-2*j+1]<-S*0.01}
					}
				}
		}
	Sn<-list(S=Sn,alpha=alpha, Lum=B[,,ech,])
	return(Sn)
	}


