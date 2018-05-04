#' Find the grooves of a bullet land
#' 
#' @param topo_df data frame with topological data in x-y-z format
#' @param method method to use for identifying grooves. Defaults to "rollapply"
#' @param smoothfactor The smoothing window to use - XXX the smoothing window seems to depend on the resolution at which the data has been collected. 
#' @param adjust positive number to adjust the grooves - XXX should be expressed in microns rather than an index
#' @param groove_cutoff The index at which a groove cannot exist past - XXX this parameter should be expressed in microns rather than as an index to be able to properly deal with different resolutions
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @param second_smooth Whether or not to smooth a second time
#' @export
#' @import ggplot2

get_grooves <- function(topo_df, method = "rollapply", smoothfactor = 15, adjust = 10, groove_cutoff = 400, mean_left = NULL, mean_right = NULL, mean_window = 100) {
  if (method == "quadratic") {
    grooves <- get_grooves_quadratic(topo_df, adjust = adjust)
  }
  if (method == "rollapply") {
    # make sure there is only one x 
    if (length(unique(topo_df$x)) > 1) {
      message(sprintf("summarizing %d profiles by averaging across values\n", length(unique(topo_df$x))))
      topo_df <- topo_df %>% group_by(y) %>% summarize(
        x = mean(x, na.rm = TRUE),
        value = mean(value, na.rm = TRUE)
      )
    }
    grooves <- get_grooves_rollapply(
      topo_df,
      smoothfactor = smoothfactor,
      adjust = adjust,
      groove_cutoff = groove_cutoff, 
      mean_left = mean_left, 
      mean_right = mean_right, 
      mean_window = mean_window,
      second_smooth = TRUE
    )
  }  
  if (method == "middle") {
    grooves <- get_grooves_middle(topo_df, middle = 75)
  }
  
  return(grooves)
}


#' Use the center of a crosscut
#' 
#' @param topo_df data frame with topological data in x-y-z format
#' @param middle middle percent to use for the identification
get_grooves_middle <- function(topo_df, middle = 75) {
  groove <- quantile(topo_df$y, probs=c((100-middle)/200, (100+middle)/200))
  plot <- topo_df %>% ggplot2::ggplot(aggplot2::es(x = y, y = value)) + 
    ggplot2::geom_line(size = .5) + 
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept=groove[1], colour = "blue") +
    ggplot2::geom_vline(xintercept=groove[2], colour = "blue") 
  
  return(list(groove = groove, plot = plot))
}

#' Quadratic fit to find groove locations
#' 
#' Use a robust fit of a quadratic curve to find groove locations
#' @param topo_df data frame with topological data in x-y-z format
#' @param adjust positive number to adjust the grooves
get_grooves_quadratic <- function(topo_df, adjust) {
  
  lm0 <- MASS::rlm(value~poly(y,2), data=topo_df, maxit=100)
  topo_df$pred <- predict(lm0, newdata=topo_df)
  
  topo_df$absresid <- with(topo_df, abs(value-pred))
  topo_df$absresid90 <- with(
    topo_df, absresid>4*median(topo_df$absresid, na.rm=TRUE))
  
  groove <- range(filter(topo_df, !absresid90)$y) + c(adjust, -adjust)
  
  plot <- topo_df %>% ggplot2::ggplot(ggplot2::aes(x = y, y = value)) + 
    ggplot2::geom_line(size = .5) + 
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept=groove[1], colour = "blue") +
    ggplot2::geom_vline(xintercept=groove[2], colour = "blue") 
  
  return(list(groove = groove, plot = plot))
}

#' Using rollapply to find grooves in a crosscut
#' 
#' @param topo_df data frame with topological data in x-y-z format
#' @param smoothfactor The smoothing window to use
#' @param adjust positive number to adjust the grooves
#' @param groove_cutoff The index at which a groove cannot exist past
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @param second_smooth Whether or not to smooth a second time
#' @param which_fun Which function to use in the rollapply statement
#' @export
#' @import ggplot2
#' @importFrom zoo rollapply
#' @importFrom zoo na.fill
get_grooves_rollapply <- function(topo_df, smoothfactor = 15, adjust = 10, groove_cutoff = 400, mean_left = NULL, mean_right = NULL, mean_window = 100, second_smooth = T, which_fun = mean) {
  
  original_topo_df <- topo_df
  if (!is.null(mean_left) && !is.null(mean_right)) {
    mean.left.ind <- which.min(abs(topo_df$y - mean_left))
    mean.right.ind <- which.min(abs(topo_df$y - mean_right))
    
    window.left.left <- max(1, mean.left.ind - mean_window)
    window.left.right <- mean.left.ind + mean_window
    
    window.right.left <- mean.right.ind - mean_window
    window.right.right <- min(length(topo_df$y), mean.right.ind + mean_window)
    
    topo_df <- topo_df[c(window.left.left:window.left.right, window.right.left:window.right.right), ]
    
    groove_cutoff <- Inf
  }
  
  value_filled <- na.fill(topo_df$value, "extend")
  smoothed <- rollapply(value_filled, smoothfactor, function(x) which_fun(x))
  # Add in an if statement, to only do the first smoothing if the second_smooth parameter is equal to FALSE
  if (second_smooth == T){
    smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) which_fun(x), partial = TRUE)
  }
  else {smoothed_truefalse <- smoothed}
  
  lengthdiff <- length(topo_df$value) - length(smoothed_truefalse)
  
  peak_ind_smoothed <- head(which(rollapply(smoothed_truefalse, 3, function(x) which.max(x) == 2)), n = 1)
  peak_ind <- peak_ind_smoothed + floor(lengthdiff / 2)
  if (length(peak_ind) == 0) groove_ind <- peak_ind else groove_ind <- head(which(rollapply(tail(smoothed_truefalse, n = -peak_ind_smoothed), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind
  
  peak_ind2_smoothed_temp <- head(which(rollapply(rev(smoothed_truefalse), 3, function(x) which.max(x) == 2)), n = 1)
  peak_ind2_temp <- peak_ind2_smoothed_temp + floor(lengthdiff / 2)
  if (length(peak_ind2_temp) == 0) groove_ind2_temp <- peak_ind2_temp else groove_ind2_temp <- head(which(rollapply(tail(rev(smoothed_truefalse), n = -peak_ind2_smoothed_temp), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind2_temp
  
  peak_ind2 <- length(topo_df$value) - peak_ind2_temp + 1
  groove_ind2 <- length(topo_df$value) - groove_ind2_temp + 1
  
  ## Check that it actually FOUND a groove...
  if (length(groove_ind) == 0 || groove_ind > groove_cutoff) groove_ind <- 1
  if (length(groove_ind2) == 0 || groove_ind2 < length(topo_df$value) - groove_cutoff) groove_ind2 <- length(topo_df$value)
  
  xvals <- original_topo_df$y
  yvals <- original_topo_df$value
  
  plot_peak_ind <- which(original_topo_df$y == topo_df$y[peak_ind])
  plot_groove_ind <- which(original_topo_df$y == topo_df$y[groove_ind])
  plot_peak_ind2 <- which(original_topo_df$y == topo_df$y[peak_ind2])
  plot_groove_ind2 <- which(original_topo_df$y == topo_df$y[groove_ind2])
  
  center <- which.min(abs(xvals - mean(xvals)))
  
  if (plot_groove_ind > center) {
    plot_groove_ind2 <- plot_groove_ind
    plot_groove_ind <- 0
  }
  
  if (plot_groove_ind2 < center) {
    plot_groove_ind <- plot_groove_ind2
    plot_groove_ind2 <- length(xvals)
  }
  
  smoothed_diff <- floor(lengthdiff/2)
  p <- ggplot2::ggplot() + 
    ggplot2::geom_point(ggplot2::aes(xvals, yvals), size = .3) + 
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = xvals[plot_groove_ind], colour = "blue") +
    ggplot2::geom_vline(xintercept = xvals[plot_groove_ind2], colour = "blue") + 
    ggplot2::ggtitle(topo_df$id_name[1])
  
  return(list(groove = c(original_topo_df$y[plot_groove_ind + adjust], 
                         original_topo_df$y[plot_groove_ind2 - adjust]), plot = p))
}
