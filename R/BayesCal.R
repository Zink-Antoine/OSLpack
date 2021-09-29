#####################################Function BayesCal################################################
#' BayesCal
#'
#' De calculation using gibbs sampler (WinBUGS code call)
#'
#' @inheritParams R2WinBUGS::bugs
#' @param Sn [list] (**required**) efficiencies S table (see Sn function)
#' @param ph  [numeric] (**required**): selected preheat
#' @param n.chains [numeric] (**with default value**) number of Markov chains (default: 1)
#'
#' @return WinBUGS simulation results (see R2WinBUGS::bugs help page)
#'
#' @export
#'
#' @examples
#' data(Anatolian2, envir = environment())
#' file<-Anatolian2$FILE
#'
#'\dontrun{
#' ## You may need to edit "bugs.directory",
#' ## also you need write access in the working directory:
#' BayesCal(Sn(file,ech=1,OSL=2,Dose=c(0,200,250,300,0,200)),ph=c(1,2,3,4),
#'          bugs.directory="C:~/Program Files/WinBUGS14",debug=TRUE)
#' }
#'
`BayesCal` <-
function(Sn,ph,n.chains=1, n.iter=2000,codaPkg=FALSE,n.thin= max(1, floor(n.chains * (n.iter-n.burnin) / 1000)), n.burnin=n.iter/2,
         bugs.directory="C:/Users/antoine.zink/Documents/Lecteur D/Program Files/WinBUGS14",debug=FALSE) #appel Bug
	{

	### version with alpha ##################
	ModelBayesCala <- function(){

	#prior
	X0 ~ dnorm(mu.X0,t.X0)
	Xba ~ dnorm(mu.Xba,t.Xba)
	tau ~ dgamma(a,b)
	for (j in 1:l){
		n [j]~ dnorm(mu.N0,t.N0)
		m[j]~dnorm(mu.M0,t.M0)
		}

	#logical link
	for (j in 1:l){
		for (i in 1:5)
			{
			Y[(j-1)*5+i]~dnorm(y[(j-1)*5+i],tau)
			y[(j-1)*5+i]<-n[j]+m[j]*X[(j-1)*5+i]
			}
		Y0[j]<-n[j]+m[j]*X0
		Ya[j]<-n[j]+m[j]*Xba
		}

	#data
	for (j in 1:l){
   		mu.Y0[j]~dnorm(Y0[j],t.Y0[j])
		for (i in 1:5){
			mu.Y[(j-1)*5+i]~dnorm(Y[(j-1)*5+i],t.Y[(j-1)*5+i])
			X[(j-1)*5+i]~dnorm(mu.X[(j-1)*5+i],t.X[(j-1)*5+i])
			}
   		mu.Ya[j]~dnorm(Ya[j],t.Ya[j])
		}
	}
	##### end alternative version #######################


	ModelBayesCal <- function(){

	#prior
	X0 ~ dnorm(mu.X0,t.X0)
	tau ~ dgamma(a,b)
	for (j in 1:l){
	n [j]~ dnorm(mu.N0,t.N0)
	m[j]~dnorm(mu.M0,t.M0)
	}

	#logical link
	for (j in 1:l){
		for (i in 1:d)
			{
			Y[(j-1)*d+i]~dnorm(y[(j-1)*d+i],tau)
			y[(j-1)*d+i]<-n[j]+m[j]*X[(j-1)*d+i]
			}
		Y0[j]<-n[j]+m[j]*X0
		}

	#data
	for (j in 1:l){
   		mu.Y0[j]~dnorm(Y0[j],t.Y0[j])
		for (i in 1:d){
			mu.Y[(j-1)*d+i]~dnorm(Y[(j-1)*d+i],t.Y[(j-1)*d+i])
			X[(j-1)*d+i]~dnorm(mu.X[(j-1)*d+i],t.X[(j-1)*d+i])
			}
		}
	}


	filename<-file.path("BayesCal.bug")

	DataEch<-Sn$S
	alpha<-Sn$alpha

	write.model(ModelBayesCal,filename)#model.bug)
	model.file<-ModelBayesCal

	if (alpha) {
		filename<-file.path("BayesCal.bug")
		write.model(ModelBayesCala,filename)#model.bug)
		model.file<-ModelBayesCala
		}


	#data
	d<-length(levels(factor(DataEch$meanX)))
	j<-(ph-1)*d+1
	aliquot<-0
	for (i in 1:length(j)){
		aliquot<-c(aliquot,seq(j[i],j[i]+4))
	}
	aliquot<-aliquot[-1]
	l<-length(ph)



	mu.Y <- DataEch$meanY[aliquot];
	t.Y<-1/DataEch$sdY[aliquot]^2;
	mu.X<-DataEch$meanX[aliquot];
	t.X<-1/DataEch$sdX[aliquot]^2;
	mu.Y0 <- c(DataEch$meanY0[ph],NA);#NA permits that the index was superior or equal to 2
	t.Y0<-c(1/DataEch$sdY0[ph]^2,NA);
	mu.Ya <- c(DataEch$meanYa[ph],NA);
	t.Ya<-c(1/DataEch$sdYa[ph]^2,NA);
	mu.X0<-mean(mu.X[mu.X!=0])
	wX0<-range(mu.X[mu.X!=0])
 	t.X0<-12/(max(wX0)-min(wX0))^2
	mu.Xba<-2*mu.X0
	wXba<-range(mu.X[mu.X!=0])
 	t.Xba<-12/(2*max(wX0)-min(wX0))^2

	Prior<-summary(lm(mu.Y~mu.X))$coefficients

	mu.N0<-Prior[1]
	t.N0<-1/Prior[3]^2
	mu.M0<-Prior[2]
	t.M0<-1/Prior[4]^2

	if (l==1){
		a<-5.5 #2.15
		b<-0.001 #0.0367
		}
	else {
	my<-rep(0,d)
	mx<-rep(0,d)
	sigy<-rep(0,l)
	for (j in 1:l){
	for (i in 1:d)
		{
		my[i]<-mu.Y[(j-1)*d+i]
		mx[i]<-mu.X[(j-1)*d+i]
		}
		sigy[j]<-summary(lm(my~mx))$sigma
	}

	n0<-2*mean(sigy^2)^2/var(sigy^2)+4
	S0<-(n0-2)*mean(sigy^2)/n0
	a<-n0/2
	b<-n0*S0/2
	}

	data <- c("l","d","mu.Y0","t.Y0","mu.X","t.X","mu.Y","t.Y","mu.N0","t.N0","mu.M0","t.M0","mu.X0","t.X0","a","b")

	#parameters
	inits <- function(){
    			list(Age=3.5)
		}

	parameters <- c("X0","n","m")

	if (alpha) {
		data <- c(data,"mu.Ya","t.Ya","mu.Xba","t.Xba")
		parameters <- c(parameters,"Xba")}

	Cal.sim <- bugs(data, parameters, inits=NULL, model.file,
    	n.chains=n.chains, n.iter=n.iter,codaPkg=codaPkg,n.thin=n.thin, n.burnin=n.burnin,
    	bugs.directory=bugs.directory,debug=debug)

  Results<-list(mu.X=mu.X,mu.Y=mu.Y,mu.Y0=mu.Y0,t.Y0=t.Y0,mu.Ya=mu.Ya,t.Ya=t.Ya,Cal.sim=Cal.sim)
	}


