"nonsigletters" <- 
function(factor1, factor2, pvalue, alpha=0.05)
 {

pdat <- data.frame(labelA=factor1, labelB=factor2, p=pvalue)

plevel <- levels(data.frame(c(as.character(pdat$labelA), as.character(pdat$labelB)))[,1])
anzf <- length(plevel)

if (all(pvalue > alpha)) 
 {
  out <- list(nonsigletters=rep("a", length(plevel)),
   groupnames=as.character(plevel),
   alpha=alpha)
 }
else
 {
 pmat <- diag(anzf)
 row.names(pmat) <- as.character(plevel)
 colnames(pmat) <- as.character(plevel)

 for (i in 1:length(pdat[,1]))
  {
   pmat[as.character(pdat[i,1]), as.character(pdat[i,2])] <- pdat[i,3]
   pmat[as.character(pdat[i,2]), as.character(pdat[i,1])] <- pdat[i,3]
  }

 tmat <- pmat > alpha

 motif.find <- function(mo, ve)
 {
  moma <- matrix(c(mo,ve), nrow=2, byrow=TRUE)
  sumo <- apply(moma,2,sum)
  nemo <- matrix(c(mo,sumo), nrow=2, byrow=TRUE)
  mumo <- apply(nemo,2,prod)
  return(sum(mumo == 1)==0)
 }

 equal.find <- function(mo, ve)
 {
  moma <- matrix(c(mo,ve), nrow=2, byrow=TRUE)
  sumo <- apply(moma,2,sum)
  return(sum(sumo == 1)==0)
 }

 imat <- rep("",anzf)
 names(imat) <- as.character(plevel)

 k <- 1
 index <- as.character(plevel)
 names(index) <- as.character(plevel)

 equal <- matrix(rep(FALSE, anzf*anzf), nrow=anzf)
 row.names(equal) <- as.character(plevel)
 colnames(equal) <- as.character(plevel)
 for (j in index){
   for (i in index){
     if (equal.find(tmat[j,index],tmat[i,index])) {
       equal[i, j] <- TRUE
       if (j != i) {index <- index[index!=i]}
     }
   }
 }

 index <- as.character(plevel)[diag(equal)]
 names(index) <- as.character(plevel)[diag(equal)]

 while (length(index) > 1){
   sumrow <- apply(tmat[index,index], 1, sum)
   chooserow <- names(sumrow[sumrow==min(sumrow)][1])
   for (i in index){
     if (motif.find(tmat[chooserow,index],tmat[i,index])) {imat[i] <- paste(c(imat[i],letters[k]), collapse="")}
   }
   index <- index[names(index)!=chooserow]
   k <- k+1
   if (all(tmat[index, index]==1)){
     imat[index] <- paste(imat[index],letters[k], sep="")
     index <- 0
   }
 }

 index <- as.character(plevel)[diag(equal)]
 names(index) <- as.character(plevel)[diag(equal)]

 bstb <- rep("", anzf)
 names(bstb) <- as.character(plevel)
 for (i in as.character(plevel)){
   bstb[i] <- imat[index][equal[i,index]]
 }

out<-
list(nonsigletters=bstb,
 groupnames=as.character(plevel),
 alpha=alpha
)
}

class(out)<-"nonsigletters"
return(out)

}


"print.nonsigletters"<-
function(x,...)

{

cat("Groups followed by the same letter", "\n",
"can not be considered significantly","\n",
"different at level", x$alpha, "\n")

print(data.frame(group=x$groupnames,letter=x$nonsigletters))

invisible(x)
}
