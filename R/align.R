#' Cross correlation function between two vectors
#' 
#' 
#' @param x vector 
#' @param y vector 
#' @param min.overlap integer value: what is the minimal number of values between x and y that should be considered?
#' @return list with ccf values and lags
#' @export
#' @examples 
#' library(dplyr)
#' x <- runif(20)
#' my_ccf(x, lead(x, 5))
#' my_ccf(x, lag(x, 5), min.overlap=3)
#' x <- runif(100)
#' my_ccf(x[45:50], x, min.overlap=6)
my_ccf <-  function(x, y, min.overlap = 0.1*max(length(x),length(y))) {
  x <- as.vector(unlist(x))
  y <- as.vector(unlist(y))
  # assume x is the long vector, y is the short vector:
  nx <- length(x)
  ny <- length(y)
  
  xx <- c(rep(NA, ny-min.overlap), x, rep(NA, ny-min.overlap))
  yy <- c(y, rep(NA, length(xx)-ny))
  
  lag.max <- length(yy) - length(y)
  lags <- 0:lag.max
  
  cors <- sapply(lags, function(lag) {
    cor(xx, lag(yy,lag), use="pairwise.complete")
  }) 
  ns <- sapply(lags, function(lag) {
    dim(na.omit(cbind(xx, lag(yy,lag))))[1]
  }) 
  cors[ns < min.overlap] <- NA
  return(list(lag = lags-(ny-min.overlap), ccf = cors))
}

#' Align two surface cross cuts using cross correlation
#' 
#' The first vector serves as a reference, the second vector is shifted, such that it aligns best with the first and has the same length as the first vector.
#' @param y1 vector of striation marks (assuming equidistance between values)
#' @param y2 vector of striation marks
#' @param min.overlap integer value: what is the minimal number of values between y1 and y2 that should be considered?
#' @return list consisting of a) the maximal cross correlation, b) the lag resulting in the highest cross correlation, 
#' and c) a vector of length y1 with aligned values of y2.
#' @export
#' @importFrom stats cor
#' @examples 
#' library(dplyr)
#' x <- runif(20)
#' do_align(x, lead(x, 5))
#' do_align(x, lag(x, 5), min.overlap=2)
#' do_align(x, lag(x, 5), min.overlap=3)
#' do_align(x, x[-(1:5)], min.overlap=3)
#' do_align(x[-(1:5)], x, min.overlap=3)
do_align <- function (y1, y2, min.overlap = 0.1*max(length(y1),length(y2)))  {
  if (min.overlap < 3) warning("min.overlap should not be below 3 values")
  
  cors <- my_ccf(y1, y2, min.overlap=min.overlap)
  lag <- list(lag = cors$lag[which.max(cors$ccf)], ccf = max(cors$ccf, na.rm=TRUE))
  
  if (lag$lag > 0) y2 <- lag(y2, lag$lag)
  if (lag$lag < 0) y2 <- lead(y2, -lag$lag)

  list(ccf = lag$ccf, lag = lag$lag, y2 = y2[1:length(y1)])
}

#' Align two surface cross cuts according to maximal correlation
#' 
#' The bullet with the first name serves as a reference, the second bullet is shifted.
#' @param data data frame consisting of at least two surface crosscuts as given by function \code{bulletSmooth}.
#' @param value string of the variable to match. Defaults to l30, the variable returned from function \code{bulletSmooth}.
#' @return list consisting of a) the maximal cross correlation, b) the lag resulting in the highest cross correlation, and c) same data frame as input, but y vectors are aligned for maximal correlation
#' @export
#' @importFrom stats na.omit
#' @importFrom stats cor
bulletAlign <- function (data, value = "l30")  {
  name <- NULL
  b12 <- unique(data$name)
  if (length(b12) != 2) 
    stop("Two surfaces should be compared\n\n")
  data$val <- data.frame(data)[, value]
  
  subLOFx1 <- subset(data, name == b12[1])
  subLOFx2 <- subset(data, name == b12[2])
  subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
  subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
  
  whichmin <- which.min(c(length(subLOFx1$y), length(subLOFx2$y)))
  shorter <- list(subLOFx1$val, subLOFx2$val)[[whichmin]]
  longer <- list(subLOFx1$val, subLOFx2$val)[[3 - whichmin]]
  
  mylagmax <- 250
  
  longer_na <- c(rep(NA, mylagmax), longer, rep(NA, mylagmax))
  
  mycors <- NULL
  for (i in 1:(length(longer_na) - length(shorter))) {
    longersub <- longer_na[i:(i + length(shorter) - 1)]
    
    corval <- cor(shorter, longersub, use = "pairwise.complete.obs")
    
    mycors <- c(mycors, corval)
  }
  
  lag <- which.max(mycors) - mylagmax
  incr <- min(diff(sort(unique(subLOFx1$y))))
  
  mydat <- if (whichmin == 1) subLOFx1 else subLOFx2
  mydat2 <- if (whichmin == 1) subLOFx2 else subLOFx1
  
  mydat$y <- mydat$y + lag * incr
  
  bullets <- rbind(data.frame(mydat), data.frame(mydat2))
  
  # ggplot(data, aes(x = y, y = l30, colour = factor(bullet))) + geom_line()
  # ggplot(bullets, aes(x = y, y = l30, colour = factor(bullet))) + geom_line()
  
  list(ccf = max(mycors, na.rm = TRUE), lag = lag * incr, bullets = bullets)
}

#' @importFrom stats cor
bulletAlign_nist <- function (data, value = "l30", mincor = .8)  {
  name <- NULL
  b12 <- unique(data$name)
  if (length(b12) != 2) 
    stop("Two surfaces should be compared\n\n")
  data$val <- data.frame(data)[, value]
  
  subLOFx1 <- subset(data, name == b12[1])
  subLOFx2 <- subset(data, name == b12[2])
  subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
  subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
  
  whichmin <- which.min(c(length(subLOFx1$y), length(subLOFx2$y)))
  shorter <- list(subLOFx1$val, subLOFx2$val)[[whichmin]]
  longer <- list(subLOFx1$val, subLOFx2$val)[[3 - whichmin]]
  
  longer_na <- c(rep(0, length(shorter)), longer, rep(0, (length(shorter))))
  
  mycors <- NULL
  for (i in 1:(length(longer_na) - length(shorter))) {
    longersub <- longer_na[i:(i + length(shorter) - 1)]
    
    corval <- cor(shorter, longersub, use = "pairwise.complete.obs")
    
    mycors <- c(mycors, corval)
  }
  
  lag <- which.max(mycors) - length(shorter)
  if (max(mycors, na.rm = TRUE) < mincor) lag <- 0
  
  incr <- min(diff(sort(unique(subLOFx1$y))))
  
  mydat <- if (whichmin == 1) subLOFx1 else subLOFx2
  mydat2 <- if (whichmin == 1) subLOFx2 else subLOFx1
  
  if (lag < 0) {
    mydat2$y <- mydat2$y + lag * incr
  } else {
    mydat$y <- mydat$y + lag * incr
  }
  
  bullets <- rbind(data.frame(mydat), data.frame(mydat2))
  list(ccf = max(mycors, na.rm = TRUE), lag = lag * incr, bullets = bullets)
}
