library(ggplot2)

graphFun <- function(x, a, b, d, g)
{
  l5p <- function(x, a, b, d, g) d + (a - d) / ((1 + x^(-b)) ^ g)
  
  if (min(b) != max(b))
  {
    breaks <- limits <- range(b)
    l <- length(b)
    group <- rep(b, each = length(x))
    title <- "b"
    lines <- TRUE
  }
  else if (min(g) != max(g))
  {
    breaks <- limits <- range(g)
    l <- length(g)
    group <- rep(g, each = length(x))
    title <- "g"
    lines <- TRUE
  }
  else
  {
    breaks <- 1
    limits <- 1
    group <- 1
    lines <- FALSE
  }

  data <- expand.grid(x = x, a = a, b = b, d = d, g = g)
  data$group <- group
  data$y <- with(data, l5p(x = x, a = a, b = b, d = d, g = g))
  
  p <- ggplot(data = data) + theme_bw()
  p <- p + coord_cartesian(xlim = range(x), ylim = c(d, a), expand = TRUE)
  
  if (lines)
  {
    p <- p + geom_line(aes(x = x, y = y, group = group, color = group), size = 2)
    cols <- colorRampPalette(
      c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF",
        "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
    )(l)
    
    guide <- guide_colorbar(
      direction = "horizontal", title = title,
      title.position = "top", label = TRUE, barwidth = unit(6, "lines"),
      barheight = unit(1.6, "lines"), draw.llim = TRUE, draw.ulim = TRUE
    )
    p <- p + scale_colour_gradientn(space = "Lab", colours = cols, 
                                    breaks = breaks, limits = limits, 
                                    expand = c(0,0), guide = guide)
  }
  else p <- p + geom_line(aes(x = x, y = y), size = 2)
  p 
}

r <- raster()

min <- minValue(r)
max <- maxValue(r)
x <-  seq(min, max, length.out = 100)
graphFun(x = x, a = .5, b = seq(0.05, 10, length.out = 10), d = .05, g = 1)
graphFun(x = x, a = .5, b = 1, d = .05, g = seq(.01, 5, length.out = 10))