#' Read an x3p file as an R Data Frame
#' 
#' @param path The file path to the x3p file
#' @param transpose If TRUE, transpose the resulting matrix
#' 
#' @export
#' @import xml2 
#' @importFrom utils unzip
read_x3p <- function(path, transpose = FALSE) {
    ## Create a temp directory to unzip x3p file
    mydir <- tempdir()
    result <- unzip(path, exdir = mydir)
    
    ## Should contain data.bin and valid.bin
    bullet_data_dir <- file.path(mydir, "bindata", dir(file.path(mydir, "bindata")))
    bullet_data <- result[1]
    
    ## Get the information on the bullet
    bullet_info <- read_xml(result[2])
    bullet_children <- xml_children(bullet_info)
    bullet_childinfo <- xml_children(bullet_children)
    
    ## Convert to a list
    bullet_info_list <- lapply(bullet_childinfo, as_list)
    bullet_info_unlist <- unlist(bullet_info_list, recursive = FALSE)
    
    ## Get the data types
    data_types <- sapply(bullet_info_list[[3]], `[[`, 2)
    
    ## Read the binary matrix
    sizes <- as.numeric(c(bullet_info_unlist$SizeX[[1]], bullet_info_unlist$SizeY[[1]], bullet_info_unlist$SizeZ[[1]]))
    increments <- as.numeric(c(bullet_info_unlist$CX$Increment[[1]], bullet_info_unlist$CY$Increment[[1]], bullet_info_unlist$CZ$Increment[[1]]))
    datamat <- matrix(readBin(bullet_data, what = numeric(), n = prod(sizes)),
                      nrow = sizes[as.numeric(!transpose) + 1],
                      ncol = sizes[2 - as.numeric(!transpose)])
    
    ## Store some metadata
    bullet_metadata <- list(num.pts.line = sizes[as.numeric(!transpose) + 1],
                            num.lines = sizes[2 - as.numeric(!transpose)],
                            x.inc = increments[1],
                            y.inc = increments[2])
    
    #plot_ly(z = ~datamat) %>% add_surface()
    
    return(list(header.info = bullet_metadata,
                surface.matrix = datamat))
}

#' Convert a list of x3p file into a data frame
#' 
#' x3p format consists of a list with header info and a 2d matrix of scan depths. 
#' fortify_x3p turn the matrix into a variable within a data frame, using the parameters of the header as necessary.
#' @param x3p a file in x3p format as return by function read_x3p
#' @return data frame with variables x, y, and value
#' @export
fortify_x3p <- function(x3p) {
    info <- x3p[[1]]
    
    df <- data.frame(expand.grid(x=1:info$num.pts.line, y=1:info$num.lines), 
                     value=as.vector(t(x3p[[2]])))
    df$x <- (df$x-1) * info$x.inc
    df$y <- (df$y-1) * info$y.inc
    
    attr(df, "info") <- info
    
    df
}

#' Convert a data frame into an x3p file
#' 
#' @param df A data frame produced by fortify_x3p
#' @return An x3p object
#' @export
unfortify_x3p <- function(df) {
    my.info <- attr(df, "info")
    my.lst <- list(header.info = my.info, 
                   surface.matrix = matrix(df$value, 
                                           nrow = length(unique(df$y)), 
                                           ncol = length(unique(df$x)),
                                           byrow = TRUE))
    
    return(my.lst)
}

#' Sample every X element of a data frame
#' 
#' Sample every X element of a data frame in x and y direction
#' @param dframe data frame with x and y variable
#' @param byxy (vector) of numeric value indicating the sapling resolution. If a single number, the same resolution is used for x and y.
#' @return subset of the input variable
#' @export
sample_x3p <- function(dframe, byxy = c(2,2)) {
    x <- NULL
    y <- NULL
    # use fortified data set
    # use only every byxy sample in x and y direction 
    if(length(byxy)==1) byxy <- rep(byxy, length=2)
    xn <- sort(as.numeric(unique(dframe$x)))
    yn <- sort(as.numeric(unique(dframe$y)))
    
    xseq <- xn[seq(1, length(xn), by=byxy[1])] 
    yseq <- yn[seq(1, length(yn), by=byxy[2])] 
    subset(dframe, (x %in% xseq) & (y %in% yseq))
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
#' @param ... Additional arguments, passed to the get_grooves function
#' @return data frame
#' @importFrom dplyr bind_rows %>%
#' @export
processBullets <- function(bullet, name = "", x = 100, grooves = NULL, span = 0.75, ...) {
    crosscuts <- unique(fortify_x3p(bullet)$x)
    crosscuts <- crosscuts[crosscuts >= min(x)]
    crosscuts <- crosscuts[crosscuts <= max(x)]
    if (length(x) > 2) crosscuts <- crosscuts[crosscuts %in% x]
    
    list_of_fits <- lapply(crosscuts, function(x) {
        br111 <- get_crosscut(path = NULL, x = x, bullet = bullet)
        if (is.null(grooves)) {
            br111.groove <- get_grooves(br111, ...)
        } else {
            br111.groove <- list(groove = grooves)
        }
        fit_loess(br111, br111.groove, span = span)$resid$data
    })
    lof <- list_of_fits %>% bind_rows
    
    data.frame(lof, bullet = name, stringsAsFactors = FALSE)
}
