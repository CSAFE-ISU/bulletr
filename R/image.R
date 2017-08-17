#' Plot x3p object as an image
#' 
#' @param x x3p object
#' @param file file name for saving, if file is NULL the opengl device stays open
#' @param col color specification
#' @param ... not used
#' @export
#' @import rgl
#' @importFrom rgl snapshot3d
#' @examples 
#' data(br411)
#' class(br411) <- "x3p"
#' image(br411, file = "br411.png")
image.x3p <- function(x, file, col = "#cd7f32", ...) {
#  browser()
  surface <- x$surface.matrix
  z <- 2*surface # Exaggerate the relief
  y <- x$header.info$profile_inc * (1:ncol(z)) # 
  x <- x$header.info$obs_inc * (1:nrow(z)) # 
  
  zlim <- range(z, na.rm=TRUE)
  zlen <- zlim[2] - zlim[1] + 1
  #col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point
#  col <- "#cd7f32" # bronze
#  col <- "darkred"
#  col <- "#C0C0C0" #silver
  
#  col <- "#b87333" #copper
  
  
  params <- rgl::r3dDefaults
  params$userMatrix <- diag(c(1,1,1,1))
  
  open3d(params=params)
  rgl.pop("lights")
  surface3d(x, y, z, color = col, back = "lines")
  
  xyz <- matrix(c(-2000, mean(y), max(z, na.rm=TRUE)), ncol = 3)
  light3d(x = xyz, diffuse = "gray40", 
          specular = "gray40", ambient="grey10", viewpoint.rel = TRUE) 
  light3d(diffuse = "gray20", specular = "gray20")

  save <- par3d()
  save$viewport[3:4] <- c(750, 250)
  save$windowRect <- c(40, 125, 790, 375)
suppressWarnings({
    par3d(save)
})
  save$zoom <- 0.35
  #bg3d("white")
  suppressWarnings({
    par3d(save)
  })
  
  Sys.sleep(2)
  if (!is.null(file)) {
  rgl.snapshot(file=file)
  rgl.close()
  }
}
