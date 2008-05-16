`subsetfl` <-
function(data, order)
{

if(!is.data.frame(data))
 {stop("Argument 'data' must be a of class 'data.frame'!")}

if(!is.list(order))
 {stop("Argument 'order' must be a named list")}

if(is.null(names(order)))
 {stop("Argument 'order' must be a named list")}

namo<-names(order)
namd<-names(data)

if( any(! unlist(lapply(order,is.vector)) ) )
 {stop("Elements of 'order' must be vectors!")}


if(any(!namo %in% namd))
 {
 wm<-which(!namo %in% namd)
 stop(paste("Element(s) ",paste(namo[wm],collapse=", ")," can not be found in 'data'!", sep=""))
 }

tnamo<-table(namo)

if(any(tnamo>1))
 {
 wg1 <- which(tnamo>1)
 stop(paste("Element(s) named ",paste(namo[wg1],collapse=", ")," occur(s) more than ones in 'order'!", sep=""))
 } 


NF<-length(namo)

for(i in 1:NF)
{

dat <- data[[namo[i]]]

if(!is.factor(dat))
 {dat<-as.factor(dat)}

ll <- levels(data[[namo[i]]])

ordi <- order[[i]]

if(length(ordi)!=length(ll))
 {warning(paste("There are not as many elements in ", namo[i] , "as are levels in data for this variable! ", sep=""))}

 if(is.character(ordi))
 {
 tabo<-table(ordi)
 if(length(tabo)<length(ordi))
  {
   wg1<-which(tabo>1)
   warning(paste("Level(s) ",paste(names(tabo)[wg1],collapse=", ")," occur(s) more than once in 'order'!"))
  }

 if(any(!ordi %in% ll))
  {  
   wm<-which(!ordi %in% ll)
   warning(paste("Element(s) ",paste(ordi[wm],collapse=", ")," in 'order' do(es) not occur as levels in 'x'!"))
  }

  data[[namo[i]]]<-factor(dat, levels=ordi)
 }


 if(is.numeric(ordi) | is.integer(ordi))
 {

 ordii <- as.integer(ordi)

 tabo<-table(ordi)

 if(length(tabo)<length(ordi))
  {
   wg1<-which(tabo>1)
   warning(paste("Level(s) ",paste(names(tabo)[wg1],collapse=", ")," occur(s) more than once in 'order'!"))
  }

 if(max(ordii>length(ll)))
  {  
   wm<-which.max(ordii)
   stop(paste("Maximal number in element ",paste(namo[wm],collapse=", ")," of 'order' is larger than the number of levels of this variable in 'data'!"))
  }
  ordc<-ll[ordii]
  data[[namo[i]]] <- factor(dat, levels=ordc)
 }

}
return(na.omit(data))
}

