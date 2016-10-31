#' Find the grooves of a bullet land
#' 
#' @param bullet data frame with topological data
#' @param smoothfactor The smoothing window to use
#' @param adjust positive number to adjust the grooves
#' @param groove_cutoff The index at which a groove cannot exist past
#' @param mean_left If provided, the location of the average left groove
#' @param mean_right If provided, the location of the average right groove
#' @param mean_window The window around the means to use
#' @export
#' @import ggplot2
#' @importFrom zoo rollapply
#' @importFrom zoo na.fill
get_grooves <- function(bullet, smoothfactor = 15, adjust = 10, groove_cutoff = 400, mean_left = NULL, mean_right = NULL, mean_window = 100) {
    original_bullet <- bullet
    if (!is.null(mean_left) && !is.null(mean_right)) {
        mean.left.ind <- which.min(abs(bullet$y - mean_left))
        mean.right.ind <- which.min(abs(bullet$y - mean_right))
        
        window.left.left <- max(1, mean.left.ind - mean_window)
        window.left.right <- mean.left.ind + mean_window
        
        window.right.left <- mean.right.ind - mean_window
        window.right.right <- min(length(bullet$y), mean.right.ind + mean_window)
        
        bullet <- bullet[c(window.left.left:window.left.right, window.right.left:window.right.right), ]
        
        groove_cutoff <- Inf
    }
    
    value_filled <- na.fill(bullet$value, "extend")
    smoothed <- rollapply(value_filled, smoothfactor, function(x) mean(x))
    smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) mean(x))
    
    lengthdiff <- length(bullet$value) - length(smoothed_truefalse)
    
    peak_ind_smoothed <- head(which(rollapply(smoothed_truefalse, 3, function(x) which.max(x) == 2)), n = 1)
    peak_ind <- peak_ind_smoothed + floor(lengthdiff / 2)
    if (length(peak_ind) == 0) groove_ind <- peak_ind else groove_ind <- head(which(rollapply(tail(smoothed_truefalse, n = -peak_ind_smoothed), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind
    
    peak_ind2_smoothed_temp <- head(which(rollapply(rev(smoothed_truefalse), 3, function(x) which.max(x) == 2)), n = 1)
    peak_ind2_temp <- peak_ind2_smoothed_temp + floor(lengthdiff / 2)
    if (length(peak_ind2_temp) == 0) groove_ind2_temp <- peak_ind2_temp else groove_ind2_temp <- head(which(rollapply(tail(rev(smoothed_truefalse), n = -peak_ind2_smoothed_temp), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind2_temp
    
    peak_ind2 <- length(bullet$value) - peak_ind2_temp + 1
    groove_ind2 <- length(bullet$value) - groove_ind2_temp + 1
    
    ## Check that it actually FOUND a groove...
    if (length(groove_ind) == 0 || groove_ind > groove_cutoff) groove_ind <- 1
    if (length(groove_ind2) == 0 || groove_ind2 < length(bullet$value) - groove_cutoff) groove_ind2 <- length(bullet$value)
    
    xvals <- original_bullet$y
    yvals <- original_bullet$value
    
    plot_peak_ind <- which(original_bullet$y == bullet$y[peak_ind])
    plot_groove_ind <- which(original_bullet$y == bullet$y[groove_ind])
    plot_peak_ind2 <- which(original_bullet$y == bullet$y[peak_ind2])
    plot_groove_ind2 <- which(original_bullet$y == bullet$y[groove_ind2])
    
    center <- which.min(abs(xvals - mean(xvals)))
    
    if (plot_groove_ind > center) {
        plot_groove_ind2 <- plot_groove_ind
        plot_groove_ind <- 0
    }
    
    if (plot_groove_ind2 < center) {
        plot_groove_ind <- plot_groove_ind2
        plot_groove_ind2 <- length(xvals)
    }
    
    p <- qplot(xvals, yvals, geom = "line") +
        theme_bw() +
        # geom_vline(xintercept = xvals[plot_peak_ind], colour = "red") +
        geom_vline(xintercept = xvals[plot_groove_ind], colour = "blue") +
        #geom_vline(xintercept = xvals[plot_peak_ind2], colour = "red") +
        geom_vline(xintercept = xvals[plot_groove_ind2], colour = "blue")
    
    return(list(groove = c(original_bullet$y[plot_groove_ind + adjust], 
                           original_bullet$y[plot_groove_ind2 - adjust]), plot = p))
}
