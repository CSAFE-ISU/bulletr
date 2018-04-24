#' Identifying a reliable cross section 
#' 
#' Identifies a "representative" cross section for a bullet land engraved area.  Striation marks on a bullet land are the
#' best expressed at the heel (bottom) of a bullet where break-off is still problematic. Using cross-correlation we identify a cross section that is 
#' the closest to the bottom of the bullet but does not suffer from break-off. 
#' If the resulting cross section is equal to the maximum of the search area (defined in xlimits), there should be some investigation, whether this cross section is usable. There is the risk of tank rash. 
#' XXX still to do: are missing values only on the right hand side (leading shoulder)?
#' @param path path to an x3p file. Ignored in case bullet is not NULL.
#' @param bullet if not NULL, the bullet land engraved area (in x3p format).
#' @param distance positive numeric value indicating the distance between cross sections to use for a comparison
#' @param xlimits vector of values between which to check for cross sections in a stable region
#' @param minccf minimal value of cross correlation to indicate a stable region
#' @param span The span for the loess smooth function
#' @param percent_missing maximum percent missing values on the crosscut to be picked
#' @importFrom x3ptools read_x3p
#' @importFrom x3ptools x3p_to_df
#' @export
bulletCheckCrossCut <- function(path, bullet = NULL, distance = 25, xlimits = c(50, 500), minccf = 0.9, span = 0.03, percent_missing = 50) {
    get_cc <- function(x, mybullet) {
      # get cross cut, and smooth it
        pickx <- mybullet$x[which.min(abs(x - unique(mybullet$x)))]
        br111 <- mybullet[mybullet$x == pickx,]
        inc <- bullet$header.info$incrementY
        br111.groove <- get_grooves(br111, groove_cutoff = 400, smoothfactor = 15, adjust = 10)
        #    br111.groove$plot
        #    browser()
        dframe <- fit_loess(br111, br111.groove)$resid$data
        
        path <- gsub(".*//", "", as.character(path))
        dframe$bullet <- paste(gsub(".x3p", "", path), x)
        dframe
    }
    if (is.null(bullet)) bullet <- read_x3p(path)
    dbr111 <- x3p_to_df(bullet)
    
    done <- FALSE
    x <- min(xlimits)
    first_cc <- get_cc(x, mybullet = dbr111)

    while(dim(first_cc)[1] < bullet$header.info$sizeX*percent_missing/100) {
      x <- x + distance
      first_cc <- get_cc(x, mybullet = dbr111)
    }
    
    while(!done) {
        x <- x + distance
        second_cc <- get_cc(x, mybullet = dbr111) # need to check that we have enough data
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
#' @importFrom x3ptools read_x3p
#' @importFrom x3ptools x3p_to_df
#' @export
get_crosscut <- function(path = NULL, x = 243.75, bullet = NULL) {
    if (is.null(bullet)) bullet <- read_x3p(path)
    dbr111 <- na.trim(x3p_to_df(bullet))
    
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
