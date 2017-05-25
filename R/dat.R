#' Read a dat file and create an x3p file
#' 
#' @param path The file path to the dat file
#' @param profiley are profiles on y?
#' @format list with header information and surface matrix
#' @export
#' @importFrom readr read_delim
#' @examples
#' \dontrun{
#' d1 <- read_dat("Br4 Bullet 4-1.dat")
#' d2 <- read_dat("L1.dat", profiley = FALSE)
#' }
#'
read_dat <- function(path, profiley = TRUE) {
  g1 <- read_delim(path, 
                       delim = " ", 
                       col_names = c("y", "x", "value"))
  if (!profiley) {
    names(g1) <- c("x", "y", "value")
    g1$x <- -g1$x
  }
  
  g1_clean <- g1 %>% 
    dplyr::select(x, y, value) %>% 
    arrange(y, x) %>%
    mutate(value = as.numeric(ifelse(value == "1.#QNAN0" | value == "-1.#IND00", NaN, value))) %>%
    mutate(value = value - min(value, na.rm = TRUE))
  
  g1_inc_x <- diff(unique(g1_clean$x)[1:2])
  g1_inc_y <- diff(unique(g1_clean$y)[1:2])
  
  g1_num_profiles <- length(unique(g1_clean$x))
  g1_num_obs_per_profile <- length(unique(g1_clean$y))
  
  g1_header.info <- list(num_profiles = g1_num_profiles,
                         num_obs_per_profile = g1_num_obs_per_profile,
                         profile_inc = g1_inc_x,
                         obs_inc = g1_inc_y)
  
  g1_mat <- matrix(g1_clean$value, 
                   nrow = g1_num_obs_per_profile, 
                   ncol = g1_num_profiles, byrow = TRUE)
  
  list(header.info = g1_header.info, surface.matrix = g1_mat)
}

