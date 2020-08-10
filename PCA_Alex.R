# Principal Components and Factor Models

rm(list=ls())
library(Quandl)
library(stargazer)
library(zoo)

Quandl.api_key("PFrF21zhdga11c2f1BBF")

# Series info
start   <- "2000-01-01"
end     <- "2018-12-31"
type    <- "xts"     

# download everything with lapply
ngnames<-lapply(1:12, function(i) paste("CHRIS/CME_NG", i, sep=""))
ng <- lapply(ngnames, Quandl, type = "xts", start_date = start, end_date = end)
ngSettle  <- lapply(ng, function(i) i$Settle)
NGmatrix <- do.call(merge.xts, ngSettle)


# make returns (note that we're accessing the Settle column!)
prices2returns <- function(x) diff(log(x$Settle))

# get all the returns, omitting NAs
ret_ng <- na.omit( do.call( cbind, lapply(NGmatrix, prices2returns)) )

# name things nicely
names(ret_ng) <- paste("ret_ng", 1:12, sep="")

# ------------------------------------------------------
# Plotting parameters
# ------------------------------------------------------

pWidth <- 12
pHeight <- 8

# ------------------------------------------------------
# PCA 
# ------------------------------------------------------
  
# matrix of pair-wise plots - takes a long time
#pairs(as.matrix(rwti))

# do pca
pca <-  prcomp(ret_ng)
summary(pca)  

# a) plot variance due to the first 5 factors
plot(pca, main="Variance due to first 5 PC", xlab="Component", ylab = "Proportion of variance")


# b) plot 5 PC



# c) Loadings
plot.zoo( x=zoo(pca$rotation[,1:5], order.by=1:12),
          plot.type = "m",  
          type="b",
          col       = c(1:5),                 
          xlab      = "Tenors",                              
          main      = "Factor Loadings (FL)"
)

#PC1 has higher impact in the first tenors and decreases with time
#PC2 impact is increasing with time
#PC3 increases un to the 4th tenor and then diminishes the impact
#4 and 5 PC's have a random impact, which makes sense with the fact that their
#variance does not contribute much

#Q2 oil and ng
wtinames<-lapply(1:6, function(i) paste("CHRIS/CME_CL", i, sep=""))
wti <- lapply(wtinames, Quandl, type = "xts", start_date = start, end_date = end)
wtiSettle  <- lapply(wti, function(i) i$Settle)
wtimatrix <- do.call(merge.xts, wtiSettle)

CL_NG <- merge(wtimatrix,NGmatrix[,1:6])
names(CL_NG) <- c("CL1","CL2","CL3","CL4","CL5","CL6","NG1","NG2","NG3","NG4","NG5","NG6")

returns <- function(x) diff(log(x))
Returns <- na.omit( do.call( cbind, lapply(CL_NG, returns)) )
pca_clng <-  prcomp(Returns)
summary(pca_clng)  
barplot(pca_clng, main="Variance due to first 3 PC", height=pHeight, width=pWidth, type="p",col="green", xlab="Component", ylab = "Proportion of variance")











# show sqrt(eigenvalues) and the actual eigenvectors
pca

# First we demean the PCs
sapply(roj, mean) - pca$center

# demean roj
roj_demeaned <- sweep(roj, 2, sapply(roj, mean), `-`)

# Define matrix A as cov(roj)
A = cov(roj)

# get eigenvalues (lambda) + eigenvectors (columns of v)
lambda = eigen(A)$values
v = eigen(A)$vectors

# What's special about eigenvectors?
A %*% v[,1] - v[,1] * lambda[1]
A %*% v     - v %*% diag(lambda)

# the SDs of the principle components are the sqrt(eigenvalues) of the vcov matrix
sqrt(lambda) - pca$sdev

# Rotations are the just the eigenvectors
v - pca$rotation

# This is how we make the rotations
T = roj_demeaned %*% pca$rotation
head(pca$x - T)

# correlation
cor(roj)

# dev.copy(png, "Hwk04/writeup/pc-ng-variance.png", width=pWidth, height=pHeight, units="in", res=300)
# dev.off()




# make zoo time series of principal compoents (a bit hacky)
comp <- zoo(pca$x, order.by = index(roj))

# Need a special function to add dotted lines at zeros
my.panel <- function(x, ...) {
  lines(x, ...)
  panel.number <- parent.frame()$panel.number
  abline(h = 0, col = "black", lty = "dotted", lwd = 1.5)
}

# plot tenor vs loading
plot.zoo(
  x=zoo(pca$rotation[,1:5], order.by=1:12),
  screens=c(1,1,2,2,2),
  type="b",
  col=1:5,
  xlab="Tenor",
  main="Factor loadings",
  ylab="",
  panel=my.panel
)

legend(x="top", legend=names(comp)[1:5], fill=1:5, horiz=T)


# plotting things with a bar plot. note use of "t()" transpose function
barplot( t(pca$rotation[,1:4]), beside=T, col=1:4)
legend(x="top", horiz=T, legend=paste("PC",1:4), fill=1:4)

# biplot
biplot(pca)