#' Identify the location and the depth of peaks and heights at a crosscut
#' 
#' @param loessdata export from rollapply 
#' @param column The column which should be smoothed
#' @param smoothfactor set to default of 35. Smaller values will pick up on smaller changes in the crosscut.
#' @param striae If TRUE, show the detected striae on the plot
#' @param window If TRUE, show the window of the striae on the plot
#' @return list of several objects: 
#' @importFrom zoo rollapply
#' @import ggplot2
#' @export
get_peaks <- function(loessdata, column = "resid", smoothfactor = 35, striae = TRUE, window = TRUE) {
    y <- NULL
    xmin <- NULL
    xmax <- NULL
    
    smoothed <- rollapply(loessdata[,column], smoothfactor, function(x) mean(x))
    smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) mean(x))
    
    test <- rollapply(smoothed_truefalse, 3, function(x) which.max(x)==2)
    test2 <- rollapply(smoothed_truefalse, 3, function(x) which.min(x)==2)
    
    
    peaks <- loessdata$y[which(test) + smoothfactor]
    valleys <- loessdata$y[which(test2) + smoothfactor]
    peaks.heights <- smoothed_truefalse[which(test) + 1]
    valleys.heights <- loessdata[,column][which(test2) + smoothfactor]
    
    # adding on some extra stats
    extrema <- c(peaks, valleys)
    heights <- c(peaks.heights, valleys.heights)
    type <- c(rep(1, length(peaks)), rep(-1, length(valleys)))
    idx <- order(extrema)
    extrema <- extrema[idx]
    heights <- heights[idx]
    type <- type[idx]
    diffs <- diff(extrema)
    
    firstval <- diffs[1]
    if (is.na(firstval)) firstval <- 0
    lastval <- diffs[length(diffs)]
    if (length(lastval) == 0) lastval <- 0
    
    lines <- data.frame(xmin = extrema-c(firstval,diffs)/3,
                        xmax = extrema+c(diffs,lastval)/3, 
                        type = type, extrema = extrema, heights = heights)    
    dframe <- data.frame(y=loessdata$y[smoothfactor:(length(loessdata$y) - smoothfactor + 1)], smoothed=smoothed_truefalse)
    p <- qplot(data=dframe, x=y, y=smoothed, geom = "line") +
        theme_bw()
    if (window) p <- p + geom_rect(aes(xmin=xmin, xmax=xmax), ymin=-6, ymax=6, data=lines, colour="grey60", alpha=0.2, inherit.aes = FALSE)
    if (striae) p <- p + geom_vline(xintercept = loessdata$y[which(test) + smoothfactor], colour = "red")
    if (striae) p <- p + geom_vline(xintercept = loessdata$y[which(test2) + smoothfactor], colour = "blue") 
    
    return(list(peaks = peaks, valleys = valleys, extrema = extrema, 
                peaks.heights = peaks.heights, valleys.heights = valleys.heights, 
                lines=lines, plot = p, dframe = dframe))
}

#' Identify the location and the depth of peaks and heights at a crosscut
#' 
#' @param loessdata export from rollapply 
#' @param column The column which should be smoothed
#' @param smoothfactor set to default of 35. Smaller values will pick up on smaller changes in the crosscut.
#' @param striae If TRUE, show the detected striae on the plot
#' @param window If TRUE, show the window of the striae on the plot
#' @return list of several objects: 
#' @importFrom zoo rollapply
#' @importFrom smoother smth.gaussian
#' @import ggplot2
#' @export
get_peaks_nist <- function(loessdata, column = "resid", smoothfactor = 35, striae = TRUE, window = TRUE) {
    y <- NULL
    xmin <- NULL
    xmax <- NULL
    
    smoothed_first <- smth.gaussian(loessdata[,column], window = 16)
    smoothed <- smoothed_first - smth.gaussian(loessdata[,column], window = 160)
    
    test <- which(diff(sign(diff(smoothed))) == -2) + 1
    test2 <- which(diff(sign(diff(smoothed))) == 2) + 1
    
    peaks <- loessdata$y[test]
    valleys <- loessdata$y[test2]
    peaks.heights <- loessdata[,column][test]
    valleys.heights <- loessdata[,column][test2]
    
    # adding on some extra stats
    extrema <- c(peaks, valleys)
    heights <- c(peaks.heights, valleys.heights)
    type <- c(rep(1, length(peaks)), rep(-1, length(valleys)))
    idx <- order(extrema)
    extrema <- extrema[idx]
    heights <- heights[idx]
    type <- type[idx]
    diffs <- diff(extrema)
    
    firstval <- diffs[1]
    if (is.na(firstval)) firstval <- 0
    lastval <- diffs[length(diffs)]
    if (length(lastval) == 0) lastval <- 0
    
    lines <- data.frame(xmin = extrema-c(firstval,diffs)/3,
                        xmax = extrema+c(diffs,lastval)/3, 
                        type = type, extrema = extrema, heights = heights)    
    dframe <- data.frame(y=loessdata$y, smoothed=smoothed)
    p <- qplot(data=dframe, x=y, y=smoothed, geom = "line") +
        theme_bw()
    if (window) p <- p + geom_rect(aes(xmin=xmin, xmax=xmax), ymin=-6, ymax=6, data=lines, colour="grey60", alpha=0.2, inherit.aes = FALSE)
    if (striae) p <- p + geom_vline(xintercept = loessdata$y[test], colour = "red")
    if (striae) p <- p + geom_vline(xintercept = loessdata$y[test2], colour = "blue")
    
    return(list(peaks = peaks, valleys = valleys, extrema = extrema, 
                peaks.heights = peaks.heights, valleys.heights = valleys.heights, 
                lines=lines, plot = p, dframe = dframe))
}

#' Match striation marks across two cross sections based on previously identified peaks and valleys
#' @param lines1 data frame as returned from get_peaks function. data frames are expected to have 
#' the following variables: xmin, xmax, group, type, bullet, heights
#' @param lines2 data frame as returned from get_peaks function. data frames are expected to have 
#' the following variables: xmin, xmax, group, type, bullet, heights
#' @return data frame of the same form as lines1 and lines2, but consisting of an additional variable of whether the striation marks are matches
#' @importFrom dplyr group_by %>% summarise
#' @importFrom reshape2 melt
#' @importFrom stats sd
#' @export
striation_identify <- function(lines1, lines2) {
    group <- NULL
    type <- NULL
    bullet <- NULL
    heights <- NULL
    n <- NULL
    
    lines <- rbind(lines1, lines2)
    lines <- lines[order(lines$xmin),]
    
    ml <- melt(lines, measure.vars=c("xmin", "xmax"))
    ml <- ml[order(ml$value),]
    ml$overlap <- c(1,-1)[as.numeric(ml$variable)]
    ml$gap <- cumsum(ml$overlap)
    
    idx <- which(ml$gap == 0)
    lines <- data.frame(xmin = ml$value[c(1,idx[-length(idx)]+1)], 
                        xmax = ml$value[idx])   
    ml$group <- 0
    ml$group[c(1, idx[-length(idx)]+1)] <- 1
    ml$group <- cumsum(ml$group)
    isMatch <- function(type, bullet) {
        if (length(unique(bullet)) != 2) return(FALSE)
        return (length(unique(type)) == 1) 
    }
    groups <- ml %>% group_by(group) %>% summarise(
        match = isMatch(type, bullet),
        size = n(),
        type = type[1],
        sdheights = sd(heights),
        heights = mean(heights))
    lines$match <- as.vector(groups$match)
    lines$type <- as.vector(groups$type)
    lines$type[!lines$match] <- NA
    lines$meany <- with(lines, (xmin+xmax)/2)
    lines$heights <- as.vector(groups$heights)
    lines$sdheights <- as.vector(groups$sdheights)
    lines
}
