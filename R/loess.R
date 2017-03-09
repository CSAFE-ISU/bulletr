#' Fit a LOESS model with bootstrap samples
#' 
#' @param bullet Bullet as returned from fortify_x3p
#' @param groove Groove as returned from get_grooves
#' @param B number of Bootstrap samples
#' @param alpha The significance level
#' @export
#' @importFrom plyr rdply
#' @importFrom dplyr summarize
#' @importFrom stats loess fitted resid quantile predict
boot_fit_loess <- function(bullet, groove, B=1000, alpha=0.95) {
    value <- NULL
    y <- NULL
    
    bullet_filter <- subset(bullet, !is.na(value) & y > groove$groove[1] & y < groove$groove[2])
    my.loess <- loess(value ~ y, data = bullet_filter)
    bullet_filter$fitted <- fitted(my.loess)
    bullet_filter$resid <- resid(my.loess)
    
    N <- nrow(bullet_filter)
    resids <- plyr::rdply(B, function(n) {
        bf <- bullet_filter[sample(N,N, replace=TRUE),]
        my.loess <- loess(value ~ y, data = bf)
        
        dframe <- data.frame(y=bullet_filter$y, fitted=predict(my.loess, newdata=bullet_filter))
        dframe$resid <- bullet_filter$value-dframe$fitted
        dframe
    })
    
    quantiles <- resids %>% group_by(y) %>% summarize(
        nas = sum(is.na(resid)),
        low = quantile(resid, probs=(1-alpha)/2, na.rm=TRUE),
        high = quantile(resid, probs=1 - (1-alpha)/2, na.rm=TRUE)
    )
    quantiles
}

#' Fit a loess curve to a bullet data frame
#' 
#' First, the surface measurements of the bullet land is trimmed to be within left and right groove as specified by vector \code{groove}.
#' A loess regression is fit to the remaining surface measurements and residuals are calculated.
#' The most extreme 0.25% of residuals are filtered from further consideration.
#' The result is called the signature of the bullet land.
#' @param bullet The bullet object as returned from fortify_x3p
#' @param groove vector of two numeric values indicating the location of the left and right groove. 
#' @param span The span to use for the loess regression
#' @return a list of a data frame of the original bullet measurements extended by loess fit, residuals, and standard errors and two plots: a plot of the fit, and a plot of the bullet's land signature. 
#' @export
fit_loess <- function(bullet, groove, span = 0.75) {
    value <- NULL
    y <- NULL
    chop <- NULL
    
    bullet_filter <- subset(bullet, !is.na(value) & y > groove$groove[1] & y < groove$groove[2])
    my.loess <- loess(value ~ y, data = bullet_filter, span = span)
    bullet_filter$fitted <- fitted(my.loess)
    bullet_filter$resid <- resid(my.loess)
    bullet_filter$se <- predict(my.loess, se=TRUE)$se.fit
    
    # filter out most extreme residuals
    bullet_filter$abs_resid <-  abs(bullet_filter$resid)
    cutoff <- quantile(bullet_filter$abs_resid, probs = c(0.9975))
    bullet_filter$chop <- bullet_filter$abs_resid > cutoff
    
    bullet_filter <- subset(bullet_filter, !chop)
    
    poly <- with(bullet_filter, 
                 data.frame(x=c(y, rev(y)), 
                            y=c(resid-1.96*se, rev(resid+1.96*se))))
    p2 <- ggplot(aes(x=y, y=resid), data=bullet_filter) + 
        #     geom_polygon(aes(x=x,y=y), fill="#0066cc", data=poly) +
        #      geom_line(size=0.1) +
        geom_line()+ 
        theme_bw() 
    
    p1 <- qplot(data = bullet_filter, y, value) +
        theme_bw() +
        geom_smooth()
    
    
    #p2 <- qplot(data = bullet_filter, y, resid, geom="line") +
    #    theme_bw()
    
    return(list(data = bullet_filter, fitted = p1, resid = p2))
}

#' Estimate predictions and residuals for a smooth of x and y
#' 
#' Fit a smooth line throught x and y, find predictive values and resiudals. 
#' @param x vector of numeric values
#' @param y vector of numeric values
#' @return data frame with predictions and residuals
#' @export
predSmooth <- function(x, y) {
    dframe <- data.frame(x, y)
    
    # return NA if more than two thirds of the values are missing
    if (sum(is.na(y)) > 2*length(y)/3) {
        dframe$smPred <- NA
        dframe$smResid <- NA
        
        return(dframe)
    }
    data.lo <- loess(y~x)
    
    dframe$smPred <- predict(data.lo, newdata=dframe)
    dframe$smResid <- with(dframe, y - smPred)
    dframe
}

#' Predict smooth from a fit
#' 
#' @param x X values to use
#' @param y Y values to use
#' @param span The span of the loess fit
#' @param sub Subsample factor
#' @export
smoothloess <- function(x, y, span, sub = 2) {
    dat <- data.frame(x, y)
    indx <- sub *(1: (nrow(dat) %/% sub))
    subdat <- dat[indx, ]
    lwp <- with(subdat, loess(y~x,span=span))
    predict(lwp, newdata = dat)
}

#' Smooth the surface of a bullet 
#' 
#' @param data data frame as returned by the function \code{processBullets}
#' @param span width of the smoother, defaults to 0.03
#' @param limits vector of the form c(min, max). Results will be limited to be between these values.
#' @return data frame of the same form as the input extended by the vector l30 for the smooth.
#' @importFrom dplyr mutate
#' @export
bulletSmooth <- function(data, span = 0.03, limits = c(-5,5)) {
    bullet <- NULL
    y <- NULL
    myspan <- NULL

    lof <- data %>% group_by(bullet) %>% mutate(
        myspan = ifelse(span > 1, span / diff(range(y)), span),
        l30 = smoothloess(y, resid, span = myspan[1])
    ) %>% select(-myspan)
    lof$l30 <- pmin(max(limits), lof$l30)
    lof$l30 <- pmax(min(limits), lof$l30)
    lof
}
