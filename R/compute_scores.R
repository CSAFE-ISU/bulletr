#' Get average scores
#' 
#' @param land1 (numeric) vector with land ids of bullet 1
#' @param land2 (numeric) vector with land ids of bullet 2
#' @param score numeric vector of scores to be summarized
#' @export
#' @return numeric vector of average scores. Length is the same as the number of land engraved areas on the bullets.
compute_average_scores <- function(land1, land2, score) {
  land1 <- parse_number(land1)
  land2 <- parse_number(land2)
  
  maxland <- max(land1, land2)
  fullframe <- data.frame(expand.grid(land1=1:maxland, land2=1:maxland))
  bcompare <- data.frame(land1, land2, score)
  
  fullframe <- fullframe %>% left_join(bcompare, by=c("land1", "land2"))
  
  fullframe <- fullframe %>% mutate(
    land1=factor(land1, levels=1:maxland),
    land2=factor(land2, levels=1:maxland)
  )
  # get averages, just in case
  matrix <- xtabs(score~land1+land2, data=fullframe)/xtabs(~land1+land2, data=fullframe)
  
  matrix <- cbind(matrix, matrix)
  
  scores <- 1:maxland %>% sapply(FUN = function(i) {
    if (i == 1) mean(diag(matrix), na.rm=TRUE)
    else {
      i <- i-1
      mean(diag(matrix[,-(1:i)]), na.rm=TRUE)
    }
  })
  scores
}
