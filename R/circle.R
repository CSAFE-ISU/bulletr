#' Estimate center and radius
#' 
#' Assuming the variables x and y are describing points located on a circle, the function uses a likelihood approach to estimate center and radius of the circle.
#' @param x numeric vector of values
#' @param y numeric vector of values
#' @return three dimensional vector of the circle center (x0, y0) and the radius
#' @export
getCircle <- function(x, y) {
    nas <- which(is.na(y))
    x <- x[-nas]
    y <- y[-nas]
    
    mx <- mean(x)
    my <- mean(y)
    
    x <- x - mx
    y <- y - my
    
    c1 <- sum(x^3) + sum(x*y^2)
    c2 <- sum(x^2*y) + sum(y^3)
    
    syy <- sum(y^2)
    sxy <- sum(x*y)
    sxx <- sum(x^2)
    
    D <- sxx*syy - sxy^2
    
    a <- (c1*syy - c2*sxy)/(2*D)
    b <- (c2*sxx - c1*sxy)/(2*D)
    r <- mean((x-a)^2 + (y-b)^2)
    cbind(x0=a+mx, y0=b+my, radius=sqrt(r))
}

#' Estimate predictions and residuals for a circle fit of x and y
#' 
#' estimate a circle, find predictive values and resiudals. depending on specification, vertical (regular) residuals or orthogonal residuals are computed.
#' @param x vector of numeric values
#' @param y vector of numeric values
#' @param resid.method character, one of "response" or "ortho"(gonal)
#' @return data frame with predictions and residuals
#' @export
predCircle <- function(x, y, resid.method="response") {
    pars <- data.frame(getCircle(x, y))
    theta <- acos((x-pars$x0)/pars$radius)/pi*180
    ypred <- pars$y0+pars$radius*sin(theta/180*pi)
    
    dframe <- data.frame(ciry=ypred)
    if ("response" %in% resid.method) {
        dframe$resid = y-ypred
    }  
    if ("ortho" %in% resid.method) {
        dframe$oresid = sqrt( (y-pars$y0)^2 + (x - pars$x0)^2) - pars$radius
    }
    
    dframe
}
