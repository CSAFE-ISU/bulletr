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
  # Check that file exists and has the correct extension
  stopifnot(file.exists(path))
  stopifnot(grepl("\\.dat$", path))
  
  
  g1 <- readr::read_delim(path, 
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
    idx_x <- seq.int(1, length(xs), by = sample)
    idx_y <- seq.int(1, length(ys), by = sample)
    g1_clean <- g1_clean %>% filter(x %in% xs[idx_x], y %in% ys[idx_y]) 
  }
  
  myx3p <- df_to_x3p(g1_clean)
  
  return(myx3p)
}
