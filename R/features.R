#' Compute the Euclidean distance between toolmarks
#' 
#' Compute the Euclidean distance between two toolmark patterns. 
#' @param y1 vector of equi-distant toolmark values
#' @param y2 vector of equi-distant toolmark values
#' @param normalize should the result be normalized to 1000 microns (1 millimeter)? Defaults to TRUE.
#' @param resolution microns per pixel. Only used for normalization. 
#' @export
get_D <- function(y1, y2, normalize=TRUE, resolution = 0.645) {
  distr.dist <- sqrt(mean((y1 - y2)^2, na.rm=TRUE))
  distr.sd <- sd(y1, na.rm=TRUE) + sd(y2, na.rm=TRUE)
  
  if (normalize) {
    distr.dist <- distr.dist *  resolution / 1000
    distr.sd <- distr.sd * resolution/1000
  }
  c(D = distr.dist, sd_D = distr.sd)
}

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

#' Get a feature vector for a pair of lands
#' 
#' @param res list of two aligned lands resulting from bulletGetMaxCMS
#' @export
get_features <- function(res) {
  lofX <- res$bullets
  lofX$bullet <- lofX$src
  b12 <- unique(lofX$bullet)
  
  subLOFx1 <- subset(lofX, bullet==b12[1])
  subLOFx2 <- subset(lofX, bullet==b12[2]) 
  
  ys <- dplyr::intersect(round(subLOFx1$y, digits = 3), round(subLOFx2$y, digits = 3))
  
  idx1 <- which(round(subLOFx1$y, digits = 3) %in% ys)
  idx2 <- which(round(subLOFx2$y, digits = 3) %in% ys)
  
  g1_inc_x <- 0.645
  
  distr.dist <- sqrt(mean(((subLOFx1$val[idx1] - subLOFx2$val[idx2]) * g1_inc_x / 1000)^2, na.rm=TRUE))
  distr.sd <- sd(subLOFx1$val * g1_inc_x / 1000, na.rm=TRUE) + sd(subLOFx2$val * g1_inc_x / 1000, na.rm=TRUE)
  
  km <- which(res$lines$match)
  knm <- which(!res$lines$match)
  if (length(km) == 0) km <- c(length(knm)+1,0)
  if (length(knm) == 0) knm <- c(length(km)+1,0)
  
  signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))
  
  doublesmoothed <- lofX %>%
    group_by(y) %>%
    mutate(avgl30 = mean(l30, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(smoothavgl30 = smoothloess(x = y, y = avgl30, span = 0.3),
           l50 = l30 - smoothavgl30)
  
  final_doublesmoothed <- doublesmoothed %>%
    filter(round(y, digits = 3) %in% ys)
  
  rough_cor <- cor(na.omit(cbind(final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[1]],
                                 final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[2]])),
                   use = "pairwise.complete.obs")[1,2]
  
  data.frame(ccf=res$ccf, rough_cor = rough_cor, lag=res$lag / 1000, 
             D=distr.dist, 
             sd_D = distr.sd,
             b1=b12[1], b2=b12[2],
             signature_length = signature.length * g1_inc_x / 1000,
             overlap = length(ys) / signature.length,
             matches = sum(res$lines$match) * (1000 / g1_inc_x) / length(ys),
             mismatches = sum(!res$lines$match) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
             cms = res$maxCMS * (1000 / g1_inc_x) / length(ys),
             cms2 = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match) * (1000 / g1_inc_x) / length(ys),
             non_cms = bulletr::maxCMS(!res$lines$match) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
             left_cms = max(knm[1] - km[1], 0) * (1000 / g1_inc_x) / length(ys),
             right_cms = max(km[length(km)] - knm[length(knm)],0) * (1000 / g1_inc_x) / length(ys),
             left_noncms = max(km[1] - knm[1], 0) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
             right_noncms = max(knm[length(knm)]-km[length(km)],0) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
             sum_peaks = sum(abs(res$lines$heights[res$lines$match])) * (1000 / g1_inc_x) / length(ys)
  )
}