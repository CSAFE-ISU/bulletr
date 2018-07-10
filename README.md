---
title: "bulletr"
author: "Eric Hare, Heike Hofmann, Ganesh Krishnan"
date: "July 09, 2018"
output: 
  html_document:
    keep_md: true
---

[![CRAN Status](http://www.r-pkg.org/badges/version/bulletr)](https://cran.r-project.org/package=bulletr) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/bulletr)](http://www.r-pkg.org/pkg/bulletr) 
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/heike/bulletr.svg?branch=master)](https://travis-ci.org/isu-csafe/bulletr)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--07--09-yellowgreen.svg)](/commits/master)


Analyze bullet striations using nonparametric methods

# Comparing two lands

## HOW-TO

1. Load Libraries
    

```r
    library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
    library(bulletr)
    library(randomForest)
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```
  
2. Read in the bullet file data, and convert to the appropriate x3p format (if necessary):


```r
    h44_g1 <- read_dat("~/Downloads/H44-G-1.dat", profiley=FALSE)
```

```
## Parsed with column specification:
## cols(
##   x = col_double(),
##   y = col_double(),
##   value = col_character()
## )
```

```r
    h44_gx1 <- read_dat("~/Downloads/H44-G-2.dat", profiley = FALSE)
```

```
## Parsed with column specification:
## cols(
##   x = col_double(),
##   y = col_double(),
##   value = col_character()
## )
```
    
3. Get the ideal cross sections



```r
    cc_g1 <- bulletCheckCrossCut(path = "~/Downloads/H44-G-1.dat", bullet = h44_g1)
    cc_gx1 <- bulletCheckCrossCut(path = "~/Downloads/H44-GX-1.dat", bullet = h44_gx1)

    ccdata_g1 <- get_crosscut(bullet = h44_g1, x = cc_g1) 
    ccdata_gx1 <- get_crosscut(bullet = h44_gx1, x = cc_gx1)
```
    
4. Get the groove locations


```r
    grooves_g1 <- get_grooves(bullet = ccdata_g1)
    grooves_gx1 <- get_grooves(bullet = ccdata_gx1)
```
    
6. Process the bullets to extract LOESS residuals


```r
    g1_processed <- processBullets(bullet = ccdata_g1,
                               name = "g1",
                               x = ccdata_g1$x[1],
                               span = 0.75,
                               grooves = grooves_g1$groove)

    gx1_processed <- processBullets(bullet = ccdata_gx1,
                                   name = "gx1",
                                   x = ccdata_gx1$x[1],
                                   span = 0.75,
                                   grooves = grooves_gx1$groove)
```
    
7. Smooth the processed bullet profiles


```r
    all_smoothed <- g1_processed %>% 
        rbind(gx1_processed) %>%
        bulletSmooth(span = 0.03) %>%
        filter(!is.na(l30))
```
   
8. Detect peaks and valleys in the aligned signatures


```r
    res <- bulletGetMaxCMS(filter(all_smoothed, bullet == "g1"), 
                           filter(all_smoothed, bullet == "gx1"), 
                           column = "l30", 
                           span = 25)
```
    
9. Extract Features


```r
    lofX <- res$bullets
    b12 <- unique(lofX$bullet)

    subLOFx1 <- subset(lofX, bullet==b12[1])
    subLOFx2 <- subset(lofX, bullet==b12[2]) 

    ys <- dplyr::intersect(round(subLOFx1$y, digits = 3), round(subLOFx2$y, digits = 3))

    idx1 <- which(round(subLOFx1$y, digits = 3) %in% ys)
    idx2 <- which(round(subLOFx2$y, digits = 3) %in% ys)

    g1_inc_x <- h44_g1$header.info$incrementX
    
    distr.dist <- sqrt(mean(((subLOFx1$val[idx1] - subLOFx2$val[idx2]) * g1_inc_x / 1000)^2, na.rm=TRUE))
    distr.sd <- sd(subLOFx1$val * g1_inc_x / 1000, na.rm=TRUE) + sd(subLOFx2$val * g1_inc_x / 1000, na.rm=TRUE)

    km <- which(res$lines$match)
    knm <- which(!res$lines$match)
    if (length(km) == 0) km <- c(length(knm)+1,0)
    if (length(knm) == 0) knm <- c(length(km)+1,0)

    signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))

    doublesmoothed <- lofX %>%
      group_by(y) %>%
      mutate(avgl30 = mean(l30, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(smoothavgl30 = smoothloess(x = y, y = avgl30, span = 0.3),
             l50 = l30 - smoothavgl30)

    final_doublesmoothed <- doublesmoothed %>%
      filter(round(y, digits = 3) %in% ys)

    rough_cor <- cor(na.omit(final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[1]]), 
                     na.omit(final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[2]]),
                     use = "pairwise.complete.obs")

    ccf_temp <- c(
      ccf=res$ccf, 
      rough_cor = rough_cor, 
      lag=res$lag / 1000, 
      D=distr.dist, 
      sd_D = distr.sd,
      b1=b12[1], b2=b12[2],
      signature_length = signature.length * g1_inc_x / 1000,
      overlap = length(ys) / signature.length,
      matches = sum(res$lines$match) * (1000 / g1_inc_x) / length(ys),
      mismatches = sum(!res$lines$match) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
      cms = res$maxCMS * (1000 / g1_inc_x) / length(ys),
      cms2 = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match) * (1000 / g1_inc_x) / length(ys),
      non_cms = bulletr::maxCMS(!res$lines$match) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
      left_cms = max(knm[1] - km[1], 0) * (1000 / g1_inc_x) / length(ys),
      right_cms = max(km[length(km)] - knm[length(knm)],0) * (1000 / g1_inc_x) / length(ys),
      left_noncms = max(km[1] - knm[1], 0) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
      right_noncms = max(knm[length(knm)]-km[length(km)],0) * 1000 / abs(diff(range(c(subLOFx1$y, subLOFx2$y)))),
      sum_peaks = sum(abs(res$lines$heights[res$lines$match])) * (1000 / g1_inc_x) / length(ys)
    )

    ccf <- t(as.data.frame(ccf_temp)) %>%
      as.data.frame() %>%
      dplyr::select(profile1_id = b1, profile2_id = b2, ccf, rough_cor, lag, D, sd_D, signature_length, overlap,
                    matches, mismatches, cms, non_cms, sum_peaks)
    ccf[,-(1:2)] <- lapply(ccf[,-(1:2)], function(x) { as.numeric(as.character(x)) })
```
    
10. Get Predicted Probability of Match


```r
    ccf$forest <- predict(rtrees, newdata = ccf, type = "prob")[,2]
ccf$forest
```

```
## [1] 0.16
```
    

An interactive interface for doing comparisons is available https://oaiti.org/apps/bulletmatcher/
