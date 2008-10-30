"Prop.ratio" <-
function(x, y, conf.level=0.95, alternative="two.sided", CImethod=c("Score","GNC"))
{

CImethod<-match.arg(CImethod)

alternative<-match.arg(alternative, choices=c("two.sided","less","greater"))

switch(CImethod,

"GNC"={
METHOD<-"Gart-Nam crude log interval"
 if( is.data.frame(x) && is.data.frame(y) )
  {
   colsx<-colSums(x)
   colsy<-colSums(y)

   nx<-sum(colsx)
   ny<-sum(colsy)
   XI<-colsx[1]+0.5
   YI<-colsy[1]+0.5
   nxI<-nx+0.5
   nyI<-ny+0.5
   estimate <- (colsx[1]/nx)/(colsy[1]/ny)
  }
  else
   {
    if((is.numeric(x) && is.numeric(y)) && ( length(x)==2 && length(y)==2 ))
     {
      nx<-sum(x)
      ny<-sum(y)
      XI<-x[1]+0.5
      YI<-y[1]+0.5
      nxI<-nx+0.5
      nyI<-ny+0.5
      estimate <- (x[1]/nx)/(y[1]/ny)
     }
   else{stop("Prop.ratio needs two data.frames or two numeric vectors of length 2 as input")}
   }

 estI <- log( (XI/nxI)/(YI/nyI) )

 stderrlog <- sqrt( 1/XI + 1/YI - 1/nxI - 1/nyI )

switch(alternative,

"two.sided"=={
   zts <- qnorm(p = 1-(1-conf.level)/2 ) 
   lower <- estI - zts * stderrlog 
   upper <- estI + zts * stderrlog
  },

"less"={
   zos <- qnorm(p = conf.level ) 
   lower <- (-Inf)
   upper <- estI + zos * stderrlog
  },

"greater"={ 
   zos <- qnorm(p = conf.level )
   lower <- estI - zos * stderrlog
   upper <- Inf
  })

 if(is.na(lower)){lower <- -Inf}
 if(is.na(upper)){upper <- Inf}

conf.int<-exp(c(lower=lower, upper=upper))

},


"Score"={

METHOD<-"Gart-Nam Score interval"

 if( is.data.frame(x) && is.data.frame(y) )
  {
   colsx<-colSums(x)
   colsy<-colSums(y)

   n1<-sum(colsx)
   n0<-sum(colsy)
   x1<-x1I<-as.numeric(colsx[1])
   x0<-x0I<-as.numeric(colsy[1])

  }
  else
   {
    if((is.numeric(x) && is.numeric(y)) && ( length(x)==2 && length(y)==2 ))
     {
      n1<-sum(x)
      n0<-sum(y)
      x1<-x1I<-x[1]
      x0<-x0I<-y[1]
     }
   else{stop("Prop.ratio needs two data.frames or two numeric vectors of length 2 as input")}
   }


if(x0==0 & x1==0)
{
conf.int<-c(lower=NA, upper=NA); estimate<-NA
}
else{

if(x0==0) {x0I<-0.5}

if(x1==0) {x1I<-0.5}

if(x0==n0) {x0I<-n1-0.5}

if(x1==n1) {x1I<-n1-0.5}


# Solution to a quadratic root:

Quad.root <- function(Aj, Bj, Cj, plus.minus) {
        Discrimi <- Bj^2 - 4 * Aj * Cj
        Limit.s <- (-Bj + plus.minus * sqrt(Discrimi))/(2 * Aj)
        return(Limit.s)
    }
# 

varpi<-function(p0, p1, n0, n1) 
{(1-p0)/(n0*p0) + (1-p1)/(n1*p1)}

# Gart Nam 3.3

MLEScore<-function(psi, x0, x1, n0, n1)
{
a<-(n0+n1)*psi
b<-(-((x0+n1)*psi + x1 + n0))
c<-x0+x1

p0<-Quad.root(Aj=a, Bj=b, Cj=c, plus.minus= c(-1,1))
p1<-p0*psi

return(list(p0=p0, p1=p1))
}

Funni<-function(psi, x0, x1, n0, n1, quantile)
{
p0<-x0/n0
p1<-x1/n1

MLE<-MLEScore(psi=psi, x0=x0, x1=x1, n0=n0, n1=n1)
pt0<-MLE$p0[1]
pt1<-MLE$p1[1]

vpsipt0<-1/varpi(p0=pt0, p1=pt1, n0=n0, n1=n1)

out<-( (x1-n1*pt1)^2 ) / ( ((1-pt1)^2) * vpsipt0 )-quantile^2
return(out)
}

# # # #

# point estimate:

estpsi<-(x1/n1)/(x0/n0)

estpsiI<-(x1I/n1)/(x0I/n0)

switch(alternative,

"two.sided"=={
quantile<-qnorm(p=1-(1-conf.level)/2)
lwr<-uniroot(Funni, interval=c(exp(-20),estpsiI),
 x0=x0I, x1=x1I, n0=n0, n1=n1, quantile=quantile)
upr<-uniroot(Funni, interval=c(estpsiI, exp(20)),
 x0=x0I, x1=x1I, n0=n0, n1=n1, quantile=quantile)
conf.int<-c(lower=lwr$root, upper= upr$root)
},

"less"={
quantile<-qnorm(p=1-(1-conf.level))
lwr<-0
upr<-uniroot(Funni, interval=c(estpsiI, exp(20)),
 x0=x0I, x1=x1I, n0=n0, n1=n1, quantile=quantile)

conf.int<-c(lower=lwr, upper=upr$root)
},

"greater"={
quantile<-qnorm(p=1-(1-conf.level))
lwr<-uniroot(Funni, interval=c(exp(-20),estpsiI),
 x0=x0I, x1=x1I, n0=n0, n1=n1, quantile=quantile)
upr<-Inf

conf.int<-c(lower=lwr$root, upper= upr)
})

estimate <-estpsi

}

})

attr(conf.int, which="methodname")<-METHOD

return(
list(conf.int=conf.int,
estimate=estimate)  
) 
}


# Examples in Gart & Nam (1988), Section 5

#Prop.ratio(x=c(8,7), y=c(4,11), CImethod="GNC")
#Prop.ratio(x=c(8,7), y=c(4,11), CImethod="Score")



#Prop.ratio(x=c(6,4), y=c(6,14), CImethod="GNC")
#Prop.ratio(x=c(6,4), y=c(6,14), CImethod="Score")



#Prop.ratio(x=c(8,7), y=c(4,11), CImethod="GNC", alternative="less")
#Prop.ratio(x=c(8,7), y=c(4,11), CImethod="Score", alternative="less")

#Prop.ratio(x=c(8,7), y=c(4,11), CImethod="GNC", alternative="greater")
#Prop.ratio(x=c(8,7), y=c(4,11), CImethod="Score", alternative="greater")



#Prop.ratio(x=c(0,15), y=c(1,15), CImethod="GNC")
#Prop.ratio(x=c(0,15), y=c(1,15), CImethod="Score")
#

#Prop.ratio(x=c(0,15), y=c(0,5), CImethod="GNC")
#Prop.ratio(x=c(0,15), y=c(0,5), CImethod="Score")

