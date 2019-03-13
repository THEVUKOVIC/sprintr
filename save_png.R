# FUNCTION save_png ------------------------------------------------------------

require(ggplot2)

save_png <- function(name, toprint, width, height) {
  imagewidth <- width
  imageheight <- height
  imageres <- 300

  png(filename = paste(name, ".png", sep = ""),
    type     = "cairo",
    units    = "px",
    width    = imagewidth*imageres,
    height   = imageheight*imageres,
    res      = imageres
  )

  print(toprint)
  
  dev.off()
}