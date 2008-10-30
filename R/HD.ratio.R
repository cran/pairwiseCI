

HD.ratio<-function(x, y, conf.level=0.95, alternative="two.sided", ...)
{

hdq<-
function (x, probs = seq(0, 1, 0.25), se = FALSE, na.rm = FALSE, 
    names = TRUE, weights = FALSE) 
{
    if (na.rm) {
        na <- is.na(x)
        if (any(na)) 
            x <- x[!na]
    }
    x <- sort(x, na.last = TRUE)
    n <- length(x)
    if (n < 2) 
        return(rep(NA, length(probs)))
    m <- n + 1
    ps <- probs[probs > 0 & probs < 1]
    qs <- 1 - ps
    a <- outer((0:n)/n, ps, function(x, p, m) pbeta(x, p * m, 
        (1 - p) * m), m = m)
    w <- a[-1, ] - a[-m, ]
    r <- drop(x %*% w)
    rp <- range(probs)
    pp <- ps
    if (rp[1] == 0) {
        r <- c(x[1], r)
        pp <- c(0, pp)
    }
    if (rp[2] == 1) {
        r <- c(r, x[n])
        pp <- c(pp, 1)
    }
    r <- r[match(pp, probs)]
    if (names) 
        names(r) <- format(probs)
    if (weights) 
        attr(r, "weights") <- structure(w, dimnames = list(NULL, 
            format(ps)))
    if (!se) 
        return(r)
    if (n < 3) 
        stop("must have n >= 3 to get standard errors")
    l <- n - 1
    a <- outer((0:l)/l, ps, function(x, p, m) pbeta(x, p * m, 
        (1 - p) * m), m = m)
    w <- a[-1, ] - a[-n, ]
    storage.mode(x) <- "double"
    storage.mode(w) <- "double"
    nq <- length(ps)
    S <- matrix(.Fortran("jacklins", x, w, as.integer(n), as.integer(nq), 
        res = double(n * nq), PACKAGE = "Hmisc")$res, ncol = nq)
    se <- l * sqrt(diag(var(S))/n)
    if (rp[1] == 0) 
        se <- c(NA, se)
    if (rp[2] == 1) 
        se <- c(se, NA)
    se <- se[match(pp, probs)]
    if (names) 
        names(se) <- names(r)
    attr(r, "se") <- se
    r
}

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

 hdq(x1,0.5)/hdq(x2,0.5)
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

estimate <- hdq(x,0.5)[[1]]/hdq(y,0.5)[[1]]

METHOD<-"Ratio of Harrell-Davis estimates for location (percentile bootstrap)"

attr(conf.int, which="methodname")<-METHOD

return(list(
conf.int=conf.int,
estimate=estimate
))

}
