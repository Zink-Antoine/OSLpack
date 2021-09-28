#' GrowthCurve.plot
#'
#'growthcurve plot at a single preheat temperature
#'
#' @param Sn [list] efficiencies S table (see Sn function)
#' @param ph0  [numeric] (**with default**): selected preheat
#' @param Unique [logical]  (**with default**)  TRUE a single De, independent from preheat temperature
#' @param debug [logical] (**required**) TRUE debug the WinBug code
#' @param ... further arguments that will be passed to the function BayesCal
#'
#' @return a plot
#' @return WinBUg results object
#'
#' @importFrom graphics points plot
#' @importFrom utils getS3method
#'
#' @export
#'
`GrowthCurve.plot` <-function(Sn,ph0=seq(1,4),Unique=FALSE,debug,...) #
	{

  debug<-FALSE
  Figures<-alist(a=,b=,c=,d=)
  ph.nom<-paste(c("250","275","300","325"),"\U00B0","C",sep="")[ph0]

  points.rv<-getS3method("points","rv")

  if (!Unique){#a De for each preheat
      for (ph in 1:length(ph0)){
        Results<-BayesCal(Sn,ph,debug,...)
        print(Results)
        Cal.sim<-Results$Cal.sim
        mu.X<-Results$mu.X
        mu.Y<-Results$mu.Y
        mu.Y0<-Results$mu.Y0
        t.Y0<-Results$t.Y0
        if (Sn$alpha){
          mu.Ya<-Results$mu.Ya
          t.Ya<-Results$t.Ya
        }


        if (!Sn$alpha){
          plot(mu.X,mu.Y,ylim=c(-0.02,max(mu.Y)*1.2),ylab="Sn",xlab="Dose (s beta)")
        }
        else {
          plot(mu.X,mu.Y,ylim=c(-0.02,max(mu.Y,mu.Ya[1])*1.2),ylab="Sn",xlab="Dose (s beta)")
          xa<-rvnorm(1,Cal.sim$mean$Xba,Cal.sim$sd$Xba)
          ya<-rvnorm(1,mu.Ya[1],1/sqrt(t.Ya[1]))
          points.rv(0, ya,rvcol="green")
          points.rv(xa, 0,rvcol="green")
        }

        n<-rvnorm(1,Cal.sim$mean$n,Cal.sim$sd$n)
        m<-rvnorm(1,Cal.sim$mean$m,Cal.sim$sd$m)
        xn<-rvnorm(1,Cal.sim$mean$X0,Cal.sim$sd$X0)
        yn<-rvnorm(1,mu.Y0[1],1/sqrt(t.Y0[1]))
        rvpar(line.sample=20)
        abline.rv(n,m,col=5)
        points.rv(mu.X, n+m*mu.X)
        points.rv(mu.X,mu.Y,col=2)
        points.rv(0, yn,rvcol="red")
        points.rv(xn, 0,col=4)
        text(5,max(mu.Y)*1.1,ph.nom[ph],cex=1,adj=0)
        x0text<- paste("x0 = ",round(Cal.sim$mean$X0,1),"(",round(Cal.sim$sd$X0,1),")")
        atext<- paste("y =",round(Cal.sim$mean$m,3),"x +",round(Cal.sim$mean$n,3))
        text(5,max(mu.Y)*1,x0text,cex=0.6,adj=0)
        text(5,max(mu.Y)*0.9,atext,cex=0.6,adj=0)
        Figures[[ph]]<-Cal.sim
      }
    }

    else {# a single De
      Results<-BayesCal(Sn,ph0,debug,...)
      print(Results)
      Cal.sim<-Results$Cal.sim
      mu.X<-Results$mu.X
      mu.Y<-Results$mu.Y
      mu.Y0<-Results$mu.Y0
      t.Y0<-Results$t.Y0
      if (Sn$alpha){
        mu.Ya<-Results$mu.Ya
        t.Ya<-Results$t.Ya
      }
      for (ph in 1:length(ph0)){
        k<-seq(5*(ph-1)+1,5*(ph-1)+5)#index linked to the preheat number

        if (!Sn$alpha){
          plot(mu.X[seq((ph-1)*5+1,ph*5)],mu.Y[seq((ph-1)*5+1,ph*5)],ylim=c(-0.02,max(mu.Y[seq((ph-1)*5+1,ph*5)])*1.2),ylab="Sn",xlab="Dose (s beta)")
        }
        else {
          plot(mu.X[seq((ph-1)*5+1,ph*5)],mu.Y[seq((ph-1)*5+1,ph*5)],ylim=c(-0.02,max(mu.Y[seq((ph-1)*5+1,ph*5)],mu.Ya[ph])*1.2),ylab="Sn",xlab="Dose (s beta)")
          xa<-rvnorm(1,Cal.sim$mean$Xba,Cal.sim$sd$Xba)
          ya<-rvnorm(1,mu.Ya[ph],1/sqrt(t.Ya[ph]))
          points.rv(0, ya,rvcol="green")
          points.rv(xa, 0,rvcol="green")
        }

        n<-rvnorm(1,Cal.sim$mean$n[ph],Cal.sim$sd$n[ph])
        m<-rvnorm(1,Cal.sim$mean$m[ph],Cal.sim$sd$m[ph])
        xn<-rvnorm(1,Cal.sim$mean$X0,Cal.sim$sd$X0)
        yn<-rvnorm(1,mu.Y0[ph],1/sqrt(t.Y0[ph]))
        rvpar(line.sample=20)
        abline.rv(n,m,col=5)
        points.rv(mu.X[k], n+m*mu.X[k])
        points.rv(mu.X[k],mu.Y[k],col=2)
        points.rv(0, yn,rvcol="red")
        points.rv(xn, 0,col=4)

        text(5,max(mu.Y)*1.1,ph.nom[ph],cex=1,adj=0)
        x0text<- paste("x0 = ",round(Cal.sim$mean$X0,1),"(",round(Cal.sim$sd$X0,1),")")
        atext<- paste("y =",round(Cal.sim$mean$m,3),"x +",round(Cal.sim$mean$n,3))
        text(5,max(mu.Y)*1,x0text[ph],cex=0.6,adj=0)
        text(5,max(mu.Y)*0.9,atext[ph],cex=0.6,adj=0)
        Figures[[ph]]<-Cal.sim
      }
    }
    Figures
  }
