

HD.ratio<-function(x, y, conf.level=0.95, alternative="two.sided", ...)
{

require(Hmisc)

addargs<-list(...)

x<-as.numeric(x)
y<-as.numeric(y)

if(alternative=="two.sided"){conf.levelI=conf.level}
else{conf.levelI=1-(1-conf.level)*2}

ni<-c(length(x), length(y))

data<-data.frame(resp=c(x,y), trt=as.factor(rep(1:2, ni)) )

HD50ratio <- function(d, i)
{
 ind1 <- i[1:ni[1]]
 ind2 <- i[-(1:ni[1])]

 x1<-d[ind1,1]
 x2<-d[ind2,1]

 hdquantile(x1,0.5)/hdquantile(x2,0.5)
}

if(is.null(addargs$R))
 {addargs$R<-999}
if(is.null(addargs$sim))
 {addargs$sim<-"ordinary"}

# stype in boot must be always == "i":
# statistics must be always HD50ratio

bootargs<-addargs
bootargs$stype<-"i"
bootargs$statistic<-HD50ratio
bootargs$data<-data
bootargs$strata<-data[,2]

boot.out<-do.call("boot", bootargs)   

 
# boot.out <- boot(data, HD50ratio, R=999, stype="i")
conf.int <- boot.ci(boot.out=boot.out, conf = conf.levelI,type =c("perc"))$perc[4:5]

if(alternative=="less")
 {conf.int[1]<-0}
else
 {if(alternative=="greater")
  {conf.int[2]<-Inf}
 }

estimate <- hdquantile(x,0.5)[[1]]/hdquantile(y,0.5)[[1]]

METHOD<-"Ratio of Harrell-Davis estimates for location (percentile bootstrap)"

attr(conf.int, which="methodname")<-METHOD

return(list(
conf.int=conf.int,
estimate=estimate
))

}
