

#' Convert an x3p file into a data frame
#' 
#' old function - for previous ISO standard. x3p format consists of a list with header info and a 2d matrix of scan depths. 
#' fortify_x3p turn the matrix into a variable within a data frame, using the parameters of the header as necessary.
#' @param x3p a file in x3p format as return by function read_x3p
#' @return data frame with variables x, y, and value
#' @export
#' @examples 
#' data(br411)
#' br411_fort <- fortify_x3p(br411)
#' head(br411_fort)
fortify_x3p <- function(x3p) {
  .Deprecated("x3p_to_df", msg="use x3ptools::x3p_to_df instead")
    info <- x3p$header.info
    
    df <- data.frame(expand.grid(x=1:info$num_profiles, y=1:info$num_obs_per_profile), 
                     value=as.vector(t(x3p$surface.matrix)))
    df$x <- (df$x-1) * info$profile_inc
    df$y <- (df$y-1) * info$obs_inc
    
    attr(df, "info") <- info
    
    df
}

#' Convert a data frame into an x3p file
#' 
#' @param df A data frame produced by fortify_x3p
#' @return An x3p object
#' @export
#' @examples 
#' data(br411)
#' br411_fort <- fortify_x3p(br411)
#' br411_unfort <- unfortify_x3p(br411_fort)
#' identical(br411_unfort, br411)
unfortify_x3p <- function(df) {
  .Deprecated("df_to_x3p", msg="use x3ptools::df_to_x3p instead")
  my.info <- attr(df, "info")
    my.lst <- list(header.info = my.info, 
                   surface.matrix = matrix(df$value, 
                                           nrow = length(unique(df$y)), 
                                           ncol = length(unique(df$x)),
                                           byrow = TRUE))
    
    class(my.lst) <- "x3p"
    return(my.lst)
}


#' Process x3p file 
#' 
#' x3p file of a 3d topological bullet surface is processed at surface crosscut x, 
#' the bullet grooves in the crosscuts are identified and removed, and a loess smooth 
#' is used (see \code{?loess} for details) to remove the big structure. 
#' @param bullet file as returned from read_x3p
#' @param name name of the bullet
#' @param x (vector) of surface crosscuts to process. 
#' @param grooves The grooves to use as a two element vector, if desired
#' @param span The span for the loess fit
#' @param window The mean window around the ideal crosscut
#' @param ... Additional arguments, passed to the get_grooves function
#' @return data frame
#' @import dplyr
#' @importFrom zoo na.trim
#' @export
#' @examples
#' data(br411)
#' br411_processed <- processBullets(br411, name = "br411")
processBullets <- function(bullet, name = "", x = 100, grooves = NULL, span = 0.75, window = 0, ...) {
    y <- value <- NULL
    
    if (!is.data.frame(bullet)) {
      crosscuts <- unique(fortify_x3p(bullet)$x)
      crosscuts <- crosscuts[crosscuts >= min(x)]
      crosscuts <- crosscuts[crosscuts <= max(x)]
    } else {
      crosscuts <- x
    }
  
    if (length(x) > 2) crosscuts <- crosscuts[crosscuts %in% x]
    
    list_of_fits <- lapply(crosscuts, function(myx) {
        if (!is.data.frame(bullet)) bullet <- fortify_x3p(bullet)
        
        br111 <- bullet %>%
            na.trim %>%
            filter(x >= myx - window, x <= myx + window) %>%
            group_by(y) %>%
            summarise(x = myx, value = mean(value, na.rm = TRUE)) %>%
            dplyr::select(x, y, value) %>%
            as.data.frame
        if (is.null(grooves)) {
            br111.groove <- get_grooves(br111, ...)
        } else {
            br111.groove <- list(groove = grooves)
        }
        
        myspan <- span
        if (myspan > 1) {
            ## Use the nist method
            myspan <- myspan / diff(br111.groove$groove)
        }
        fit_loess(br111, br111.groove, span = myspan)$resid$data
    })
    lof <- list_of_fits %>% bind_rows
    
    data.frame(lof, bullet = name, stringsAsFactors = FALSE)
}



  