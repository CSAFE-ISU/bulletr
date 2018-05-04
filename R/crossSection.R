#' Get crosscut at a specific level
#' 
#' @param x level to use
#' @param obj x3p object (bullet)
#' @param obj_name name of x3p object (from file). Defaults to "".
single_cc <- function(x, obj, obj_name="") {
  # get cross cut, and smooth it
  pickx <- obj$x[which.min(abs(x - unique(obj$x)))]
  xlevel_data <- obj[obj$x == pickx,]
  inc <- obj$header.info$incrementY
  xlevel_groove <- get_grooves(xlevel_data, groove_cutoff = 400, smoothfactor = 15, adjust = 10)
  dframe <- fit_loess(xlevel_data, xlevel_groove)$resid$data
  
  # FIXME: Should path references even be in here?
  path <- gsub(".*//", "", as.character(obj_name))
  dframe$name <- paste(gsub(".x3p", "", obj_name), x)
  dframe
}



#' Identify a reliable cross section
#' 
#' Identifies a "representative" cross section for a bullet land engraved area.  Striation marks on a bullet land are the
#' best expressed at the heel (bottom) of a bullet where break-off is still problematic. Using cross-correlation we identify a cross section that is 
#' the closest to the bottom of the bullet but does not suffer from break-off. 
#' If the resulting cross section is equal to the maximum of the search area (defined in xlimits), there should be some investigation, whether this cross section is usable. There is the risk of tank rash. 
#' TODO: are missing values only on the right hand side (leading shoulder)?
#' @param path path to an x3p file. Ignored in case bullet is not NULL.
#' @param x3p_obj if not NULL, the bullet land engraved area (in x3p format).
#' @param distance positive numeric value indicating the distance between cross sections to use for a comparison
#' @param xlimits vector of values between which to check for cross sections in a stable region
#' @param minccf minimal value of cross correlation to indicate a stable region
#' @param span The span for the loess smooth function
#' @param percent_missing maximum percent missing values on the crosscut to be picked
#' @importFrom x3ptools read_x3p
#' @importFrom x3ptools x3p_to_df
#' @export
bulletCheckCrossCut <- function(path = NULL, x3p_obj = NULL, distance = 25, xlimits = c(50, 500), minccf = 0.9, span = 0.03, percent_missing = 50) {
  # Bullet and path cannot both be NULL
  stopifnot(!(is.null(path) & is.null(x3p_obj)))
  
  if (is.null(x3p_obj)) {
    x3p_obj <- read_x3p(path)
  }
  
  x3p_df <- x3p_to_df(x3p_obj)
  
  # NOTE: Stopped optimizing/commenting/fixing here
  ### Susan playing from here
  # xlevels <- seq.int(xlimits[1], xlimits[2], by = distance)
  # ccs <- purrr::map(xlevels, ~single_cc(x = .x, obj = x3p_df))
  # 
  # missing_pct <- sapply(ccfs, function(x) dim(x)[1] < x3p_obj$header.info$sizeX*percent_missing/100)
  # 
  # idx <- min(which(!missing_pct))
  # 
  # ccfs <- purrr::map_df(.x = idx:(length(ccs) - 1), .f = ~{
  #   z <- bulletSmooth(rbind(ccfs[[.x]], ccfs[[.x+1]]), span = span) %>%
  #     bulletAlign %>%
  #     magrittr::extract2("ccf")
  #   
  #   data_frame(x = xlevels[.x], x1 = xlevels[.x+1], ccf = z[1])
  # })
  ### End susan playing
  
  done <- FALSE
  x <- min(xlimits)
  first_cc <- single_cc(x, obj = x3p_df)
  
  while(dim(first_cc)[1] < x3p_obj$header.info$sizeX*percent_missing/100) {
    x <- x + distance
    first_cc <- single_cc(x, obj = x3p_df)
  }
  
  while(!done) {
    x <- x + distance
    second_cc <- single_cc(x, obj = x3p_df) # TODO: need to check that we have enough data
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
