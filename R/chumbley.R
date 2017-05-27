#' Get R Statistic for Chumbley matching
#' 
#' See Chumbley et al (2010)
#' @param b1 dataframe
#' @param b2 dataframe
#' @param window width of the window (in indices) to consider for matching
#' @param b1.left left index of the matching window
get_lag_max_R <- function(b1, b2, window, b1.left) {
  dplccf <- function(x, y, lag.max = 50) {
    x <- as.vector(unlist(x))
    y <- as.vector(unlist(y))
    # assume x is the long vector, y is the short vector:
    nx <- length(x)
    ny <- length(y)
    stopifnot(nx >= ny) # we need a better error message here
    
    y <- c(y, rep(NA, nx-ny))
    
    cors <- sapply(1:lag.max, function(lag) {
      cor(x, lag(y,lag), use="pairwise.complete")
    }) 
    list(lag = which.max(cors), ccf = max(cors))
  }
  
  # find the window of measurements around the b1.center
  left.idx <- b1.left #which.min(abs(b1$y-b1.left))
  b1.resid <- b1$resid[seq(left.idx, min(nrow(b1), left.idx+window))]
  lag <- dplccf(b2$resid, b1.resid, lag.max = nrow(b2)-window)
  data <- data.frame(x=1:length(b1.resid), 
                     y1=b1.resid, 
                     y2=b2$resid[lag$lag-1+1:length(b1.resid)])
  plot <- b2 %>% ggplot(aes(y=resid)) + 
    geom_vline(xintercept=left.idx, colour="grey50") +
    geom_point(aes(x=1:nrow(b2)), size=.5, colour="steelblue") +
    geom_point(aes(x=x, y=y), data=data.frame(x = 1:length(b1.resid)+lag$lag, y=b1.resid), size=.5, colour="orange") 
    
  list(lag = lag$lag-left.idx, 
       cor = lag$ccf,
       plot = plot)
}


#' Get correlation between two signatures
#' 
#' @param b1 dataframe
#' @param b2 dataframe
#' @param window width of the window (in indices) to consider for matching
#' @param b1.left left location of the matching window
#' @param lag integer lag for the second window
#' @export
get_cor <- function(b1, b2, window, b1.left, lag) {
  # find the window of measurements around the b1.center
  left.idx <- which.min(abs(b1$y-b1.left))
  indices <- seq(left.idx, min(nrow(b1), left.idx+window))
  idx <- which((indices + lag > 0) & (indices + lag <= nrow(b1)))
  if (length(idx) == 0) return(NA)
  indices <- indices[idx]
  
  b1.resid <- b1$resid[indices]
  b2.resid <- b2$resid[indices+lag]

  b1$window <- FALSE
  b1$window[indices] <- TRUE
  
  b1 %>% ggplot(aes( x = y,  y = resid)) + 
    geom_point(aes( colour=window), size = .5) + scale_colour_manual(values=c("steelblue", "orange")) + 
    geom_point(size = .5, data =b2, colour="black")  + 
    geom_point(aes(x=x,y=y), 
               size=0.5,
               data=data.frame(x=b2$y[indices+lag], y = b2.resid), 
               colour="red")
  
  data = data.frame(x = indices,
                    y1 = b1.resid,
                    y2 = b2.resid)
  plot <- data %>% ggplot(aes(x=indices)) + 
    geom_vline(xintercept=left.idx, colour="grey50") +
    geom_point(aes(y=y1), size=.5, colour="steelblue") +
    geom_point(aes(y=y2), size=.5, colour="orange") 
  #print(plot)
  cor(b1.resid, b2.resid, use="pairwise.complete")  
}

#' Chumbley test score
#' 
#' 
#' @param b1 dataframe
#' @param b2 dataframe
#' @param b1.left left location of the matching window
#' @param window width of the window (in indices) to consider for matching
#' @param reps number of replicates to use in the evaluation
#' @export
#' @examples 
#' data(br411)
#' b1 <- get_crosscut(x = 250, bullet=br411)
#' b2 <- get_crosscut(x = 150, bullet = br411)
#' b3 <- get_crosscut(x = 10, bullet=br411)
#' b1.gr <- b1 %>% get_grooves(smoothfactor=30)
#' b2.gr <- b2 %>% get_grooves()
#' b3.gr <- b3 %>% get_grooves()
#' # check that the grooves are actually found:
#' b1.gr$plot
#' b2.gr$plot
#' # get signatures
#' b1 <- fit_loess(b1, b1.gr)$data
#' b2 <- fit_loess(b2, b2.gr)$data
#' b3 <- fit_loess(b3, b3.gr)$data
#' get_lag_max_R(b1, b2, window = 200, b1.left = 1000)
#' get_lag_max_R(b1, b2, window = 100, b1.left = 800)
#' match <- get_lag_max_R(b1, b2, window = 100, b1.left = 450)
#' # matched correlations
#' get_cor(b1, b2, window = 100, b1.left = 800, lag = match$lag)
#' get_cor(b1, b2, window = 100, b1.left = 1000, lag = match$lag)
#' get_cor(b1, b2, window = 100, b1.left = 1200, lag = match$lag)
#' # random correlations
#' get_cor(b1, b2, window = 100, b1.left = 800, lag = 100)
#' get_cor(b1, b2, window = 100, b1.left = 1000, lag = 300)
#' get_cor(b1, b2, window = 100, b1.left = 1200, lag = 500)
#' 
#' chumbley(b1, b2, window=150, reps=5)
#' 
#' match13 <- get_lag_max_R(b1, b3, window = 100, b1.left = 450)
#' # matched correlations
#' get_cor(b1, b3, window = 100, b1.left = 800, lag = match13$lag)
#' get_cor(b1, b3, window = 100, b1.left = 1000, lag = match13$lag)
#' get_cor(b1, b3, window = 100, b1.left = 1200, lag = match13$lag)
#' # random correlations
#' get_cor(b1, b3, window = 100, b1.left = 800, lag = 100)
#' get_cor(b1, b3, window = 100, b1.left = 1000, lag = 300)
#' get_cor(b1, b3, window = 100, b1.left = 1200, lag = 500)
#' 
#' chumbley(b1, b3, window=100, reps=5)
chumbley <- function(b1, b2, b1.left, window, reps = 3) {
  # get reps+1 many non-overlapping intervals of length window in the first scan
  nx <-  nrow(b1)
  if (nx/window < reps+1) stop("Can't find enough non-overlapping intervals. Reduce window size or number of repetitions.")
  parts <- round(seq.int(from = 1, to=nx, length.out=reps+2))
  parts_diff <- diff(parts)
  parts <- parts[-(reps+2)]
  
  lefts <- parts+sapply(parts_diff, function(x) sample(1:(x-window), 1))
  
  # identify one of the intervals as the special one
  align_by <- sample(reps+1, 1)
  
  # find the best match
  match <- get_lag_max_R(b1, b2, window = window, b1.left = lefts[align_by])

  # matched correlations
  cor_matched <- sapply(lefts[-align_by], function(left) 
    get_cor(b1, b2, window = window, b1.left = left, lag = match$lag))
  addons <- lapply(lefts[-align_by], function(left) {
    xs <-  1:window + left + match$lag
    geom_point(aes(x=x, y=y), data=data.frame(x = xs, 
                                              y=b2$resid[xs]), colour = "red", size = .2)
  })
  match$plot <- match$plot + addons
  
  # random correlations
  random_lags <- sample(1:(nx-window), size=reps, replace=FALSE) - lefts[align_by]
  indices <- lefts[align_by] -1 + 1:window
  cor_random <- sapply(random_lags, function(lag) 
    cor(b1$resid[indices], b2$resid[indices+lag], use="pairwise.complete"))  
  #  get_cor(b1, b2, window = window, b1.left = lefts[align_by], lag = lag))
  list(alignment = list(window = c(lefts[align_by], lefts[align_by]+ window), lag=match$lag, cor=match$cor, plot=match$plot),
       test=wilcox.test(cor_matched, cor_random, alternative="greater"), cor_matched, cor_random)
}
