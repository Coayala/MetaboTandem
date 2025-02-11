############### extract.biplot.pcoa() ###############
extract.biplot.pcoa <- function(x,y, plot.axes=c(1,2), dir.axis1=1, dir.axis2=1,rn=NULL,main=NULL){

  ### Description
  # Function extract.biplot.pcoa reproduces the PCoA biplot function
  # (ape::biplot.pcoa) by extracting the PCoA axis and eigenvector loadings
  # for each variable, allowing plotting in ggplot instead of a pre-packaged output.
  #
  # The function takes two inputs:
  #
  # x = pcoa object from ?pcoa
  # y = dataframe used in pcoa()
  #
  # and returns a list with two outputs:
  #
  #   $PCoA_dimensions is Axis.1 and Axis.2 from the pcoa object
  #
  #   $eigenvectors is Axis.1 and Axis.2 are the endpoints for the projections of
  #       each variable in the PCoA input (scaled and centered), see line 13
  #
  #
  # The biplot.pcoa function allows both objects to be plotted on the same plot,
  # the two outputs from this function makes it easier to reproduce the same plots
  # in ggplot from the raw output. See example above for use.
  #
  # Note that the positions of the variables in the PCoA plot calculated here
  # (as in biplot.pcoa) are equivalent to PCA scaling 1 and are euclidean
  # distances, whereas the underlying PCoA is non-euclidian depending on the
  # choice of distance matrix.

  # PCoA
  if (!inherits(x, "pcoa")) stop("Object of class 'pcoa' expected")
  pr.coo <- x$vectors
  if(x$correction[2] > 1) pr.coo <- x$vectors.cor
  k <- ncol(pr.coo)
  if(k < 2) stop("There is a single eigenvalue. No plot can be produced.")
  if(k < plot.axes[1]) stop("Axis",plot.axes[1],"does not exist.")
  if(k < plot.axes[2]) stop("Axis",plot.axes[2],"does not exist.")

  if(!is.null(rn)) rownames(pr.coo) <- rn
  labels = colnames(pr.coo[,plot.axes])
  diag.dir <- diag(c(dir.axis1,dir.axis2))
  pr.coo[,plot.axes] <- pr.coo[,plot.axes] %*% diag.dir

  # Eigenloadings
  Y = apply(y, 2, scale, center=TRUE, scale=TRUE) # scale the vectors
  n <- nrow(Y)
  points.stand <- scale(pr.coo[,plot.axes])
  S <- cov(Y, points.stand)
  U <- S %*% diag((x$values$Eigenvalues[plot.axes]/(n-1))^(-0.5))
  colnames(U) <- colnames(pr.coo[,plot.axes])

  # write outputs
  extract.biplot.pcoa.output <- NULL
  extract.biplot.pcoa.output[[1]] <- pr.coo[,1:2]
  extract.biplot.pcoa.output[[2]] <- U
  names(extract.biplot.pcoa.output) <- c("PCoA_dimensions","eigenvectors")
  return(extract.biplot.pcoa.output)

}
