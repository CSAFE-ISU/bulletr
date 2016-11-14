#' Identifying a reliable cross section 
#' 
#' Should be changed: x should just indicate lower and upper limit. That is cleaner and should speed things up as well.
#' @param path path to an x3p file
#' @param bullet If passed in, the actual bullet already loaded
#' @param distance positive numeric value indicating the distance between cross sections to use for a comparison
#' @param xlimits vector of values between which to check for cross sections in a stable region
#' @param minccf minimal value of cross correlation to indicate a stable region
#' @param span The span for the loess smooth function
#' @export
bulletCheckCrossCut <- function(path, bullet = NULL, distance = 25, xlimits = c(50, 500), minccf = 0.9, span = 0.03) {
    get_cc <- function(x, mybullet) {
        pickx <- mybullet$x[which.min(abs(x - unique(mybullet$x)))]
        
        br111 <- mybullet[mybullet$x == pickx,]
        br111.groove <- get_grooves(br111)
        #    br111.groove$plot
        #    browser()
        dframe <- fit_loess(br111, br111.groove)$resid$data
        
        path <- gsub(".*//", "", as.character(path))
        dframe$bullet <- paste(gsub(".x3p", "", path), x)
        dframe
    }
    if (is.null(bullet)) bullet <- read_x3p(path)
    dbr111 <- fortify_x3p(bullet)
    
    done <- FALSE
    x <- min(xlimits)
    first_cc <- get_cc(x, mybullet = dbr111)
    
    while(!done) {
        x <- x + distance
        second_cc <- get_cc(x, mybullet = dbr111)
        b2 <- rbind(first_cc, second_cc)
        lofX <- bulletSmooth(b2, span = span)
        ccf <- bulletAlign(lofX)$ccf
        if (ccf > minccf) { 
            done <- TRUE
            return (x - distance)
        } 
        first_cc <- second_cc
        if (x + distance > max(xlimits)) done <- TRUE
    } 
    return (NA)
}

#' Read a crosscut from a 3d surface file
#' 
#' @param path path to an x3p file. The path will only be considered, if bullet is not specified.
#' @param x level of the crosscut to be taken. If this level does not exist, the crosscut with the closest level is returned.
#' @param bullet alternative access to the surface measurements. 
#' @return data frame 
#' @importFrom zoo na.trim
#' @export
get_crosscut <- function(path = NULL, x = 243.75, bullet = NULL) {
    if (is.null(bullet)) bullet <- read_x3p(path)
    dbr111 <- na.trim(fortify_x3p(bullet))
    
    pickx <- dbr111$x[which.min(abs(x - unique(dbr111$x)))]
    
    dbr111.fixx <- dbr111[dbr111$x == pickx,]
    
    return(dbr111.fixx)
}

#' Deprecated function use get_crosscut
#' 
#' @param path The path to the x3p file
#' @param x The crosscut value
#' @export
get_bullet <- function(path, x = 243.75) {
    cat("Use function get_crosscut instead of get_bullet\n\n")
    get_crosscut(path, x=x)
}
