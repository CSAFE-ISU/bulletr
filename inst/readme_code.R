library(dplyr)
library(x3ptools)
library(bulletr)
library(randomForest)



b1 <- read_dat("Houston Set Rescan/Group 1/KA/Bullet 1/HTX - Group 1 - Kit CU - KA Bullet 1 Land 1 - Sneox1 - 20x - auto light left image +20 perc. - threshold 2 - resolution 4 - Allison Mark.dat", profiley = F)
b2 <- read_dat("Houston Set Rescan/Group 1/KA/Bullet 1/HTX - Group 1 - Kit CU - KA Bullet 1 Land 2 - Sneox1 - 20x - auto light left image +20 perc. - threshold 2 - resolution 4 - Allison Mark.dat", profiley = F)


b1.ccl <- bulletCheckCrossCut(path = NULL, bullet = b1)
b2.ccl <- bulletCheckCrossCut(path = NULL, bullet = b2)
# Pull out level for cross cut


b1.cc <- get_crosscut(bullet = b1, x = b1.ccl)
b2.cc <- get_crosscut(bullet = b2, x = b2.ccl)

b1.grooves <- get_grooves(bullet = b1.cc)
b1.grooves$groove <- c(450, 1850)
b2.grooves <- get_grooves(bullet = b2.cc)
b2.grooves$groove <- c(400, 1900)

b1.process <- processBullets(bullet = b1.cc, name = "b1", x = b1.cc$x[1], span = 0.75, grooves = b1.grooves$groove)
b2.process <- processBullets(bullet = b2.cc, name = "b2", x = b2.cc$x[1], span = 0.75, grooves = b2.grooves$groove)


all.smoothed <- rbind(b1.process, b2.process) %>%
  bulletSmooth(span = 0.03)

res <- bulletGetMaxCMS(filter(all.smoothed, bullet == "b1"), filter(all.smoothed, bullet == "b2"), column = "l30", span = 25)


lofX <- res$bullets
b12 <- unique(lofX$bullet)

subLOFx1 <- subset(lofX, bullet==b12[1])
subLOFx2 <- subset(lofX, bullet==b12[2]) 

ys <- dplyr::intersect(subLOFx1$y, subLOFx2$y)

idx1 <- which(subLOFx1$y %in% ys)
idx2 <- which(subLOFx2$y %in% ys)

g1_inc_x <- .645#h44_g1$header.info$incrementY

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
  filter(y %in% ys)

rough_cor <- cor(final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[1]], 
                 final_doublesmoothed$l50[final_doublesmoothed$bullet == b12[2]],
                 use = "pairwise.complete.obs")

ccf_temp <- c(ccf=res$ccf, rough_cor = rough_cor, lag=res$lag / 1000, 
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
ccf[,-c(1, 2)] <- lapply(ccf[,-(1:2)], function(x) { as.numeric(as.character(x)) })

ccf$forest <- predict(rtrees, newdata = ccf, type = "prob")[,2]
ccf$forest
