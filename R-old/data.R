#' Read a dat file and create an x3p file
#' 
#' @param path The file path to the dat file
#' @param profiley are profiles on y?
#' @param sample 1 in sample lines will be taken 
#' @format list with header information and surface matrix
#' @export
#' @importFrom readr read_delim
#' @importFrom x3ptools df_to_x3p
#' @examples
#' \dontrun{
#' d1 <- read_dat("Br4 Bullet 4-1.dat")
#' d2 <- read_dat("L1.dat", profiley = FALSE)
#' }
#'
read_dat <- function(path, profiley = TRUE, sample = 1) {
    g1 <- read_delim(path, 
                     delim = " ", 
                     col_names = c("x", "y", "value"))
    
    if (!profiley) {
        names(g1) <- c("y", "x", "value")
        g1$x <- -g1$x
    }
    
    if (profiley) {
        g1$x <- -g1$x
        g1$y <- -g1$y
    }
    
    g1_clean <- g1 %>% 
        dplyr::select(x, y, value) %>% 
        arrange(y, x) %>%
        mutate(value = as.numeric(ifelse(value == "1.#QNAN0" | value == "-1.#IND00", NaN, value))) %>%
        mutate(value = value - min(value, na.rm = TRUE)) 
    
    xs <- sort(unique(g1_clean$x))
    ys <- sort(unique(g1_clean$y))
    
    # downsample if necessary
    if (sample > 1) {
        idx <- (1:length(xs)) %% sample == 0
        xs <- xs[idx]
        idx <- (1:length(ys)) %% sample == 0
        ys <- ys[idx]
        
        g1_clean <- g1_clean %>% filter(x %in% xs, y %in% ys) 
    }
    
    myx3p <- df_to_x3p(g1_clean)
    
    return(myx3p)
}

#' 3d topological surface measurements for one land of a bullet from the Hamby study
#' 
#' Some more info - not sure at the moment which bullet this is. Describe structure.
#' @format a list
"br411"

#' randomforest
#' 
#' this randomforest was fitted to predict known matches and non-matches from the scans of land engraved areas of the Hamby study.
#' @format a random forest object fitted by the randomforest function from the package of the same name
"rtrees"
