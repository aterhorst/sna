
#############################################################
####							#####
####	   ALGEBRAIC ANALYSIS OF MULTIPLE, SIGNED,	#####
####	  AND AFFILIATION NETWORKS WITH `multiplex?	#####
####							#####
####		ANTONIO RIVERO OSTOIC			#####
####	   multiplex@post.com, jaro@econ.au.dk		#####
####							#####
####		INSNA SUNBELT 2016 CONFERENCE 		#####
####		    NEWPORT BEACH, CA			#####
####							#####
####	      5 April, 18:30-21:30, Cardiff Room	#####
####							#####
#############################################################


### Set working directory (e.g.)
setwd("C:/sunbelt")


### DATA

## Start by creating a data frame with pseudo-random numbers
set.seed(123)
dtf <- as.data.frame( cbind( round(round(runif(10),5)*10), 
	round(round(runif(10),5)*10), 
	round(runif(10)), round(runif(10)) ) )


## load the multiplex package
library("multiplex")
net <- read.srt(dtf, header=FALSE, toarray=TRUE)


## First save 'net' into a map
write.dat(net, path="data/")

## Read delimited data and assing to objects
co <- read.delim(file="data/net_V3.dat", header=FALSE, sep=" ")
fr <- read.delim(file="data/net_V4.dat", header=FALSE, sep=" ")


## Label the nodes
rownames(co) <- 0:10
colnames(co) <- 0:10

rownames(fr) <- rownames(co)
colnames(fr) <- colnames(co)


net1 <- zbind(co, fr)

all.equal(net, net1)
#[1] "Attributes: < Component ?dimnames?: Component 3: 2 string mismatches >"


## Label the relation types in 'net1'
dimnames(net1)[[3]] <- c("V3","V4")


all.equal(net, net1)



### MULTIGRAPHS

## load the multigraph package
library("multigraph")

## net
multigraph(net)
multigraph(net,cx=3)
multigraph(net,cx=3,lw=1.8)
multigraph(net,cx=3,lw=1.8,ecol="#000000")
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"))
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=1)
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=1,vcol="green")
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green")
#
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=1,vcol="green",directed=FALSE)
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=1,vcol="green",directed=FALSE,collRecip=TRUE)
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=1,vcol="green",directed=FALSE,collRecip=TRUE,bw=.5)

multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5)

multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5,layout="rand")
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5,layout="rand",directed=FALSE)

multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5,layout="stress")
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5,layout="stress",seed=17)
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5,layout="stress",seed=17,pos=0)
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5,layout="stress",seed=17,pos=0,alpha=.2)
multigraph(net,cx=3,lw=1.8,ecol=c("red","blue"),lt=c(1,3),vcol="green",bw=.5,layout="stress",seed=17,pos=0,pch=2)



## FLORENTINE
flbm <- read.srt(file="C:/Latex/Sunbelt16/Rnw/data/florentine.srt", header=TRUE)
load("C:/Latex/Sunbelt16/Rnw/data/florentineAttrs.rda")
flbmat <- florentineAttrs[,1]
flbmat <- dichot(flbmat, c=40)

multigraph(flbm,directed=FALSE,"stress",cx=6,pos=0,tcx=.75,alpha=.4,lw=2,collRecip=TRUE,ecol="#000000",bw=.5,seed=2)
multigraph(flbm,directed=FALSE,"stress",cx=6,pos=0,tcx=.75,alpha=.4,lw=2,collRecip=TRUE,ecol="#000000",bw=.5,seed=2,showAtts=TRUE,att=flbmat,lbat="$$")
multigraph(flbm,directed=FALSE,"stress",cx=6,pos=0,tcx=.75,alpha=.4,lw=2,collRecip=TRUE,ecol="#000000",bw=.5,seed=2,vclu=flbmat,vcluc=c("red","blue"))



# INCUBATOR A
data(incubA)
netA <- incubA$net[,,1:3]
nAat <- incubA$net[,,4:5]

multigraph(netA,"stress",seed=1)
multigraph(netA,"stress",seed=1,att=nAat,ecol="black")
multigraph(netA,"stress",att=nAat,ecol="black",pos=0,cx=3,tcx=.75,vcol="gray",seed=1)



# SOUTHERN WOMEN
sw <- read.srt(file="C:/Latex/Sunbelt16/Rnw/data/swomen.srt", attr=TRUE, toarray=FALSE)




########################################
# %%%%%% 2. RELATIONAL STRUCTURES %%%% #
########################################


## First distinguish the label relations
dimnames(net)[[3]] <- c("C","F")

strings(net)

semigroup(net, type="numerical")

semigroup(net, type="symbolic")

strings(net, equat=TRUE)


multigraph(flbm,directed=FALSE,"stress",cx=6,pos=0,tcx=.75,bw=.5,
     vcol="blue",alpha=.4,lw=2,collRecip=TRUE,ecol="#808080",seed=21)


## Relation Box with default arguments
flbm.rb <- rbox(flbm)

cph(flbm.rb)


## Let's look at compounds with length 4
cph(rbox(flbm, k=4))


## ... compounds with length 5
cph(rbox(flbm, k=5))


## Need to load the suggested package first
if( require("Rgraphviz", quietly = TRUE)) {
     diagram(cph(rbox(flbm, k=5))) }


## Assign H to an object
flbm.cph5 <- cph(rbox(flbm, k=5))

diagram.levels(flbm.cph5)

## By setting "perm" into TRUE we can get the clustering...
clu <- diagram.levels(flbm.cph5, perm=TRUE)$clu


flbm.ps <- reduc(flbm, clu=clu)
flbm.ps


semigroup(flbm.ps, type="numerical")

semigroup(flbm.ps, type="symbolic")


multigraph(flbm,directed=FALSE,"stress",cx=6,pos=0,tcx=.75,bw=.5,collRecip=TRUE,
     vclu=flbmat,vcluc=c("red","blue"),alpha=.4,lw=2,ecol="#808080",seed=21)


## a vector already represents families' Wealth
flbmat


## we need to create an empty matrix first
flw <- dichot(flbm[,,1], c=2)

diag(flw) <- flbmat



## now the network has 3 relations
flbmw <- zbind(flbm, flw)
flbmw

## we put the corresponding generator label
dimnames(flbmw)[[3]][3] <- "W"


## evaluate differentiation, different lengths
cph(rbox(flbmw, k=3))

cph(rbox(flbmw, k=4))

cph(rbox(flbmw, k=5))


if( require("Rgraphviz", quietly = TRUE)) {
     diagram(cph(rbox(flbmw, k=5))) }


#flbmw.ps <- reduc(flbmw, clu=c(3,3,1,1,2,3,2,2,1,2,1,4,3,2,2,1))
flbmw.ps <- reduc(flbmw, 
                clu=diagram.levels(cph(rbox(flbmw,k=5)),perm=TRUE)$clu)
flbmw.ps


semigroup(flbmw.ps, type="symbolic")$S

strings(flbmw.ps, equat=TRUE, k=3)$equat

partial.order(strings(flbmw.ps), type="strings")


if( require("Rgraphviz", quietly = TRUE)) {
     diagram(partial.order(strings(flbmw.ps),type="strings")) }



## CE: Directed with attributes
multigraph(netA,"stress",att=nAat,ecol="black",pos=0,cx=3,tcx=.75,vcol="gray",seed=1)


rbox(netA, transp=TRUE)

rbox(netA, transp=TRUE, tlabels=c("D","G","L"))


## "incubA" already has the network with actor attributes
netAat <- incubA$net


## we avoid specific tie transposes by typing NA 
## within "tlabels" in the respective locations
rbox(netAat, transp=TRUE, tlabels=c("D","G","L",NA,NA))


## with k = 3 (the default) there is already a nice differentiation
netAat.rb <- rbox(netAat,transp=TRUE,tlabels=c("D","G","L",NA,NA))
netAat.cph <- cph(netAat.rb)

if( require("Rgraphviz", quietly = TRUE)) { diagram(netAat.cph) }


# netAat.rb$W contains the Relation Box
netAat.rb$W


## we look at the strings labels
dimnames(netAat.rb$W)[[3]]


## select the generators, now with transposes
netAat.gens <- netAat.rb$W[,,1:8]

## create the network positional system with customized classes
netAat.ps <- reduc(netAat.gens,
	    clu=c(3,2,1,1,1,2,4,2,2,3,3,1,4,3,1,1,3,4,1,1,1,1,1,4,3,4))


## role table
netAat.rt <- semigroup(netAat.ps)
netAat.rt$S


## set of equations
netAat.st <- strings(netAat.ps, equat=TRUE, k=3)
netAat.st$equat


## set of inclusions
netAat.po <- partial.order(netAat.st, type="strings")
netAat.po


if( require("Rgraphviz", quietly = TRUE)) { diagram(netAat.po) }


## Decomposition

# abstract semigroup
cngr(netAat.rt)

# partially ordered semigroup
cngr(S=netAat.rt, PO=netAat.po, unique=TRUE)

netAat.cngpos <- cngr(S=netAat.rt, PO=netAat.po, unique=TRUE)


# based on "congruence classes"
decomp(netAat.rt, netAat.cngpos, type="cc")

# with reduction
decomp(netAat.rt, netAat.cngpos, type="cc", reduc=TRUE)




####################################
# %%%%%% 3. SIGNED NETWORKS %%%%%% #
####################################


mat <- matrix(nrow=6, ncol=6)
rownames(mat) <- letters[21:26]
colnames(mat) <- rownames(mat)
## assing values
mat[1,]<-c(0,0,1,-1,0,1)
mat[2,]<-c(-1,0,0,0,0,0)
mat[3,]<-c(0,0,0,-1,-1,0)
mat[4,]<-c(0,1,0,0,0,0)
mat[5,]<-c(0,0,0,1,0,-1)
mat[6,]<-c(0,0,0,-1,0,0)
mat

## requires a "Signed" class object
semiring(as.signed(mat), type="balance")


## remove "isolates" in this system
netAcc <- rm.isol(netA[,,c(1,3)])


multigraph(netAcc,"stress",ecol=c("green","red"),lt=c(2,3),pos=0,
       cx=2,tcx=.6,vcol="blue",alpha=.5,bw=.5,seed=99)


netA.sgn <- signed(netAcc)


## Balance semiring deafult: 2-paths
semiring(netA.sgn, type="balance")
## 3-paths
semiring(netA.sgn, type="balance", k=3)
## 2-semipaths
semiring(netA.sgn, type="balance", symclos=FALSE)
## ...


## Cluster semiring deafult: 2-paths
semiring(netA.sgn, type="cluster")
## 3-paths
semiring(netA.sgn, type="cluster", k=3)
## 2-semipaths
semiring(netA.sgn, type="cluster", symclos=FALSE)
## ...


all.equal(
semiring(netA.sgn, type="balance", k=3)$Q,
semiring(netA.sgn, type="balance", k=4)$Q
)

all.equal(
semiring(netA.sgn, type="cluster", k=3)$Q,
semiring(netA.sgn, type="cluster", k=4)$Q
)


## balance semiring structure, 4-paths
netA.sb4 <- semiring(netA.sgn, type="balance", symclos=FALSE, k=4)$Q

## here the clustering is manually computed
perm(netA.sb4, clu=c(1,6,1,2,6,6,3,2,2,3,6,2,4,2,5,2,2,1,1,2,6,2))


multigraph(netAcc,"stress",seed=22,alpha=.5,cx=2,pos=0,ecol="gray",
       lt=c(2,3),vclu=c(1,0,1,2,0,0,3,2,2,3,0,2,4,2,5,2,2,1,1,2,0,2),
       vcluc=c("white","blue","red","green","orange","darkgreen"))


rel.sys(netA)

rel.sys(netAcc)

ccc <- which(rel.sys(netA)$nodes %in% rel.sys(netAcc)$nodes)
nAat[ccc,ccc,]


multigraph(netAcc,"stress",seed=22,alpha=.5,cx=2,pos=0,showAtts=TRUE,att=nAat[ccc,ccc,],
       ecol="gray",lt=c(2,3),vclu=c(1,0,1,2,0,0,3,2,2,3,0,2,4,2,5,2,2,1,1,2,0,2),
       vcluc=c("white","blue","red","green","orange","darkgreen"))




#########################################
# %%%%%% 4. AFFILIATION NETWORKS %%%%%  #
#########################################


sw

galois(sw)
galois(sw, labeling="reduced")


sw.gc <- galois(sw, labeling="reduced")

sw.gc$reduc[48:length(sw.gc$reduc)]

str(sw.gc)


partial.order(sw.gc, type="galois")

partial.order(sw.gc, type="galois",
              labels=paste0("c",1:length(sw.gc$full)))


sw.pogc <- partial.order(sw.gc, type="galois")

## plot it as a lattice diagram
if( require("Rgraphviz", quietly = TRUE)) {
	diagram(sw.pogc)
}


## principal filter of the first concept
fltr(1, sw.pogc)

## principal ideal of the first concept with the label
fltr("E1", sw.pogc, ideal=TRUE)


## active "lb2lb" allows preserving the labels
transf(sw.pogc, type="matlist", lb2lb=TRUE)





#### END ###


