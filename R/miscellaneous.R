#' Estimate the twist in a bullet land
#' 
#' Estimation of the twist in a barrel follows roughly the process described by Chu et al (2010).
#' At the moment, twist is estimated from a single land - but the twist should be the same for the whole barrel. Therefore all lands of the same barrel should
#' have the same twist.
#' A note on timing: at the moment calculating the twist rate for a bullet land takes several minutes.
#' XXX TODO XXX make the different methods a parameter. Also, accept other input than the path - if we start with the flattened bulletland we get results much faster.
#' @param path to a file in x3p format
#' @param bullet data in x3p format as returned by function read_x3p
#' @param twistlimit Constraint the possible twist value
#' @param cutoff Use this for the quantile cutoff
#' @return numeric value estimating the twist
#' @export
#' @importFrom robustbase lmrob
#' @importFrom stats lm coef median 
#' @importFrom utils head tail
#' @examples
#' \dontrun{
#' # execution takes several minutes
#' load("data/b1.rda")
#' twist <- getTwist(path="barrel 1 bullet 1", bullet = b1, twistlimit=c(-2,0)*1.5625)
#' }
getTwist <- function(path, bullet = NULL, twistlimit = NULL, cutoff = .75) {
    x <- NULL
    r.squared <- NULL
    r.squared.robust <- NULL
    
  if (is.null(bullet)) bullet <- read_x3p(path)
  cat(path)
  cat("\n")
  
  gg115 <- processBullets(
    bullet, 
    x=bullet$header.info$profile_inc*c(0,bullet$header.info$num_profiles), 
    name=path)
  # qplot(data=gg115, x=y, y=x, fill=resid, geom="tile")+scale_fill_gradient2()
  
  xs <- unique(gg115$x)
  twist <- NULL
  ccf <- NULL
  aligned <- list()
  
  for (i in seq_along(xs)[-1]) {
    profiles <- subset(gg115, x %in% xs[c(i,i-1)])  
    profiles$bullet <- sprintf("%s-%s", path, profiles$x)
    aligned[[i]] <- bulletAlign(profiles, value="resid")
    twist <- c(twist, aligned[[i]]$lag)
    ccf <- c(ccf, aligned[[i]]$ccf)
  }
  #qplot(xs, c(0, twist)) + ylim(c(-10,10))
  #qplot(xs, c(1, ccf)) + geom_hline(yintercept=0.95, colour="red")
  #qplot(xs, cumsum(c(0, twist))) +ylim(c(700,1100))

#  if (!is.null(twistlimit)) {
    # is twistlimit a vector with two numbers?
    twistConstraint <- pmin(twist, max(twistlimit))
    twistConstraint <- pmax(twistConstraint, min(twistlimit))
#  } else twistConstraint = twist
  dframe <- data.frame(x = xs, twist=cumsum(c(0, twist)), 
                       twistConstraint = cumsum(c(0, twistConstraint)), 
                       ccf = c(0,ccf))
  #qplot(x, twist, data=subset(dframe, between(x, 220, 600))) +geom_smooth(method="lm")
  
  Rs <- data.frame(zoo::rollapply(data=dframe$twist, width=200, FUN=function(twist) {
    x <- 1:length(twist)
    m <- lm(twist~x)
    m2 <- try(robustbase::lmrob(twist~x, na.action=na.omit), silent=TRUE)
    if (class(m2) == "try-error") {
      cat(sprintf("NAs in robust estimation of twist in land %s\n", path))
 #     browser()
      r.squared.robust=NA
      twistRobust=NA
    } else {
      r.squared.robust=summary(m2)$r.squared
      twistRobust=coef(m2)[2]
    }
    
    data.frame(r.squared=summary(m)$r.squared, twist=coef(m)[2],
               r.squared.robust=r.squared.robust, 
               twistRobust=twistRobust)
  }, by=1))
#  browser()
  RConstraint <- data.frame(zoo::rollapply(data=dframe$twistConstraint, width=200, FUN=function(twist) {
    x <- 1:length(twist)
    m <- lm(twist~x)
    m2 <- try(robustbase::lmrob(twist~x, na.action=na.omit), silent=TRUE)
    if (class(m2) == "try-error") {
      cat(sprintf("NAs in robust estimation of twist in land %s\n", path))
 #     browser()
      r.squared.robust=NA
      twistRobust=NA
    } else {
      r.squared.robust=summary(m2)$r.squared
      twistRobust=coef(m2)[2]
    }
    data.frame(r.squared=summary(m)$r.squared, twist=coef(m)[2],
               r.squared.robust=r.squared.robust, 
               twistRobust=twistRobust)
  }, by=1))
  
    
  q75 <- quantile(Rs$r.squared, probs=cutoff)
  twist <- median(subset(Rs, r.squared > q75)$twist)
  q75r <- quantile(Rs$r.squared.robust, probs=cutoff, na.rm=TRUE)
  twistRobust <- median(subset(Rs, r.squared.robust > q75r)$twistRobust, na.rm=TRUE)
  
  q75C <- quantile(RConstraint$r.squared, probs=cutoff)
  twistC <- median(subset(RConstraint, r.squared > q75)$twist)
  q75rC <- quantile(RConstraint$r.squared.robust, probs=cutoff, na.rm=TRUE)
  twistRobustC <- median(subset(RConstraint, r.squared.robust > q75r)$twistRobust, na.rm=TRUE)
  
  
    
  data.frame(twist=twist, min.r.squared=q75, twistRobust=twistRobust, min.r.squared.robust=q75r,
             twistC=twistC, min.r.squaredC=q75C, twistRobustC=twistRobustC, min.r.squared.robustC=q75rC)
}

#' Plot a bullet land using plotly
#' 
#' @param path The path to the x3p file
#' @param bullet If not null, use this pre-loaded bullet
#' 
#' @importFrom plotly plot_ly
#' @export
plot_3d_land <- function(path, bullet = NULL) {
    if (is.null(bullet)) bullet <- read_x3p(path)
    surfmat <- bullet$surface.matrix
    
    plot_ly(z = surfmat, type = "surface")
}
