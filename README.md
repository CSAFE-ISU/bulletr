# bulletr
Analyze bullet striations using nonparametric methods

## HOW-TO

1. Load Libraries
    
    ```
    library(dplyr)
    library(readr)
    library(bulletr)
    library(randomForest)
    ```
  
2. Read in the first bullet file data, and convert to the appropriate x3p format (if necessary):

    ```
    h44_g1 <- read_delim("~/Downloads/H44-G-1.dat", 
                       delim = " ", 
                       col_names = c("y", "x", "value"))

    h44_g1_clean <- h44_g1 %>% 
      dplyr::select(x, y, value) %>% 
      arrange(y, x) %>%
      mutate(value = as.numeric(ifelse(value == "1.#QNAN0" | value == "-1.#IND00", NaN, value))) %>%
      mutate(value = value - min(value, na.rm = TRUE))

    g1_inc_x <- diff(unique(h44_g1_clean$x)[1:2])
    g1_inc_y <- diff(unique(h44_g1_clean$y)[1:2])

    g1_num_profiles <- length(unique(h44_g1_clean$x))
    g1_num_obs_per_profile <- length(unique(h44_g1_clean$y))

    g1_header.info <- list(num_profiles = g1_num_profiles,
                        num_obs_per_profile = g1_num_obs_per_profile,
                        profile_inc = g1_inc_x,
                        obs_inc = g1_inc_y)

    g1_mat <- matrix(h44_g1_clean$value, nrow = g1_num_obs_per_profile, ncol = g1_num_profiles, byrow = TRUE)

    h44_g1_x3p <- list(header.info = g1_header.info, surface.matrix = g1_mat)
    ```
