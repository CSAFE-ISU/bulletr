#' Identify the number of maximum CMS between two bullet lands
#' 
#' @param lof1 dataframe of smoothed first signature
#' @param lof2 dataframe of smoothed second signature
#' @param column The column which to smooth
#' @param span positive number  for the smoothfactor to use for assessing peaks. 
#' @return list of matching parameters, data set of the identified striae, and the aligned data sets.
#' @export
bulletGetMaxCMS <- function(lof1, lof2, column = "resid", span = 35) {
    bullet <- NULL
    
    lof <- rbind(lof1, lof2)
    bAlign = bulletAlign(lof, value = column)
    lofX <- bAlign$bullet  
    
    b12 <- unique(lof$bullet)
    peaks1 <- get_peaks(subset(lofX, bullet==b12[1]), column = column, smoothfactor = span)
    peaks2 <- get_peaks(subset(lofX, bullet == b12[2]), column = column, smoothfactor = span)
    
    #qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
    #    theme_bw() +
    #    geom_rect(data=peaks1$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25)) +
    #    geom_rect(data=peaks2$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25))
    
    peaks1$lines$bullet <- b12[1]
    peaks2$lines$bullet <- b12[2]
    
    lines <- striation_identify(peaks1$lines, peaks2$lines)
    
    #   p <- qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
    #     theme_bw() +
    #     geom_rect(data=lines, aes(xmin=xmin, xmax=xmax, fill = factor(type)),  ymin=-6, ymax=6, inherit.aes = FALSE, alpha=I(0.25)) +
    #     ylim(c(-6,6)) +
    #     geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match), inherit.aes = FALSE) +
    #     geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match), inherit.aes = FALSE) 
    
    maxCMS <- maxCMS(lines$match==TRUE)
    list(maxCMS = maxCMS, ccf = bAlign$ccf, lag=bAlign$lag, lines=lines, bullets=lofX)
}  

#' Identify the number of maximum CMS between two bullet lands
#' 
#' @param lof1 dataframe of smoothed first signature
#' @param lof2 dataframe of smoothed second signature
#' @param column The column which to smooth
#' @param span positive number  for the smoothfactor to use for assessing peaks. 
#' @return list of matching parameters, data set of the identified striae, and the aligned data sets.
#' @export
bulletGetMaxCMS_nist <- function(lof1, lof2, column = "resid", span = 35) {
    bullet <- NULL
    
    lof <- rbind(lof1, lof2)
    bAlign = bulletAlign_nist(lof, value = column)
    lofX <- bAlign$bullet  
    
    b12 <- unique(lof$bullet)
    peaks1 <- get_peaks_nist(subset(lofX, bullet==b12[1]), column = column, smoothfactor = span)
    peaks2 <- get_peaks_nist(subset(lofX, bullet == b12[2]), column = column, smoothfactor = span)
    
    #qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
    #    theme_bw() +
    #    geom_rect(data=peaks1$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25)) +
    #    geom_rect(data=peaks2$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25))
    
    peaks1$lines$bullet <- b12[1]
    peaks2$lines$bullet <- b12[2]
    
    lines <- striation_identify(peaks1$lines, peaks2$lines)
    
    #   p <- qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
    #     theme_bw() +
    #     geom_rect(data=lines, aes(xmin=xmin, xmax=xmax, fill = factor(type)),  ymin=-6, ymax=6, inherit.aes = FALSE, alpha=I(0.25)) +
    #     ylim(c(-6,6)) +
    #     geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match), inherit.aes = FALSE) +
    #     geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match), inherit.aes = FALSE) 
    
    maxCMS <- maxCMS(lines$match==TRUE)
    list(maxCMS = maxCMS, ccf = bAlign$ccf, lag=bAlign$lag, lines=lines, bullets=lofX)
}

#' Number of maximum consecutively matching striae
#' 
#' @param match is a Boolean vector of matches/non-matches
#' @return an integer value of the maximum number of consecutive matches
#' @export
#' @examples 
#' x <- rbinom(100, size = 1, prob = 1/3) 
#' CMS(x == 1) # expected value for longest match is 3
#' maxCMS(x==1)
maxCMS <- function(match) {
    cmsTable <- CMS(match)
    as.numeric(rev(names(cmsTable)))[1]
}

#' Table of the number of consecutive matches
#' 
#' @param match is a Boolean vector of matches/non-matches
#' @return a table of the number of the CMS and their frequencies
#' @export
#' @examples 
#' x <- rbinom(100, size = 1, prob = 1/3) 
#' CMS(x == 1) # expected value for longest match is 3
CMS <- function(match) {
    # number of consecutive matching striae
    
    y <- diff(match)
    # y is -1 if change from 1 to 0, 
    #       0 if unchanged
    #       1 if change from 0 to 1
    w <- c(0, y)[match][-1]
    
    z <- which(w == 1)
    z <- c(0,z,length(match[match]))
    
    return(table(diff(z)))
}
