library(imgpalr)
library(png)
library(raster)
library(dplyr)
library(babynames)
library(Cairo)
library(showtext)
library(sysfonts)
library(tiler)

# Color correction and quantization
pal0 <- c("#FFFFFF", "#2B9CD9", "#F51E47", "#222222")
pal <- c("#FFFFFF", "#2B9CD9", "#F51E47", "#F51E47")
img <- image_quantmap("bernie.png", pal0, pal, plot = FALSE)
writePNG(img, "bernie2.png")
pal <- pal[-4]

# Pixels to points and sample first names
r <- calc(brick("bernie2.png"), mean)
m <- matrix(c(0, 116, 3, 138, 139, 2, 254, 255, 1))
r <- reclassify(r, m)
p <- rasterToPoints(r)

d <- filter(babynames,  year >= 1935 & year <= 2002) %>%
  mutate(n = n * 2 * (year - 1935) / 67 + 1) %>%
  group_by(name) %>% summarize(n = sum(n))

set.seed(1)
x <- sample(d$name, round(nrow(p) / 5), TRUE, d$n)
nc <- c(0, cumsum(nchar(x) + 1) + 1)
nc <- nc[-length(nc)]
x <- x[nc <= nrow(p)]
nc <- nc[nc <= nrow(p)]

# Save final image
font_add_google("Rubik Mono One", "rmo")
showtext_auto()
n <- 8

CairoPNG("notmeus.png", width = n * ncol(r), height = n * nrow(r), res = 300, bg = "#CCCCCC")
op <- par(mar = rep(0, 4))
plot(0, 0, type = "n", axes = FALSE, xlim = c(xmin(r), xmax(r)), ylim = c(ymin(r), ymax(r)), xaxs = "i", yaxs = "i")
text(p[nc, 1], p[nc, 2], x, col = pal[p[nc, 3]], family = "rmo", cex = 0.75)
par(op)
dev.off()

# Generate map tiles
tile("notmeus.png", "tiles", zoom = "3-8", georef = FALSE)
