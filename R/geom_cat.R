##' key drawing function
##'
##'
##' @name draw_key
##' @param data A single row data frame containing the scaled aesthetics to display in this key
##' @param params A list of additional parameters supplied to the geom.
##' @param size Width and height of key in mm
##' @return A grid grob
NULL


ggname <- getFromNamespace("ggname", "ggplot2")

##' @rdname draw_key
##' @importFrom grid rectGrob
##' @importFrom grid pointsGrob
##' @importFrom grid gpar
##' @export
draw_key_cat <- function(data, params, size) {

  filename <- system.file(paste0(data$cat, ".png"), package = "ggcats")
  img <- as.raster(png::readPNG(filename))
  aspect <- dim(img)[1]/dim(img)[2]
  # rasterGrob
  grid::rasterGrob(image = img,
                   width = ggplot2::unit(data$size / size, 'snpc'),
                   height = ggplot2::unit(data$size / size * aspect, 'snpc'))
}


##' geom layer adding cats
##'
##'
##' @title geom_cat
##' @param mapping aes mapping
##' @param data data
##' @param stat stat
##' @param position position
##' @param inherit.aes logical, whether inherit aes from ggplot()
##' @param na.rm logical, whether remove NA values
##' @param by one of 'width' or 'height'
##' @param nudge_x horizontal adjustment to nudge cats
##' @param ... additional parameters
##' @return geom layer
##' @importFrom ggplot2 layer
##' @export
##' @examples
##' library("ggplot2")
##' ggplot(mtcars) +
##' geom_cat(aes(mpg, wt), cat = "nyancat", size = 5)
##'
##' set.seed(1)
##' df <- data.frame(x = rnorm(10),
##'                  y = rnorm(10),
##'                  image = sample(c("nyancat",
##'                                   "pusheen",
##'                                   "colonel",
##'                                   "venus",
##'                                   "toast"),
##'                                  size = 10, replace = TRUE))
##'  ggplot(df) +
##' geom_cat(aes(x, y, cat = image), size = 5)
##'
geom_cat <- function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", inherit.aes = TRUE,
                     na.rm = FALSE, by = "width", nudge_x = 0, ...) {

  by <- match.arg(by, c("width", "height"))

  layer(
    data = data,
    mapping = mapping,
    geom = GeomCat,
    stat = stat,
    position = position,
    show.legend = NA,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      by = by,
      nudge_x = nudge_x,
      ##angle = angle,
      ...),
    check.aes = FALSE
  )
}


##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 draw_key_blank
##' @importFrom grid gTree
##' @importFrom grid gList
GeomCat <- ggplot2::ggproto("GeomCat", ggplot2::Geom,
                     setup_data = function(data, params) {
                       if (is.null(data$subset))
                         return(data)
                       data[which(data$subset),]
                     },

                     default_aes = ggplot2::aes(cat = "nyancat", size = 1,
                                       colour = NULL, angle = 0, alpha = 1),

                     draw_panel = function(data, panel_params, coord, by, na.rm=FALSE,
                                           .fun = NULL, height, image_fun = NULL,
                                           hjust = 0.5, nudge_x = 0, nudge_y = 0, asp = 1) {
                       data$x <- data$x + nudge_x
                       data$y <- data$y + nudge_y
                       data <- coord$transform(data, panel_params)

                       if (!is.null(.fun) && is.function(.fun)) {
                         data$ca <- .fun(data$cat)
                       }
                       if (is.null(data$cat)) return(NULL)

                       groups <- split(data, factor(data$cat))
                       imgs <- names(groups)
                       grobs <- lapply(seq_along(groups), function(i) {
                         d <- groups[[i]]
                         if (is.na(imgs[i])) return(zeroGrob())

                         imageGrob(d$x, d$y, d$size/5, imgs[i], by, hjust,
                                   d$colour, d$alpha, image_fun, d$angle, asp)
                       })
                       grobs <- do.call("c", grobs)
                       class(grobs) <- "gList"

                       ggplot2:::ggname("geom_cat",
                              gTree(children = grobs, cl = "fixasp_raster"))
                     },
                     non_missing_aes = c("size", "cat"),
                     required_aes = c("x", "y"),
                     draw_key = draw_key_cat ## draw_key_blank ## need to write the `draw_key_cat` function.
)



##' @importFrom grid rasterGrob
##' @importFrom grid viewport
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @importFrom tools file_ext
imageGrob <- function(x, y, size, img, by, hjust, colour, alpha, image_fun, angle, asp = 1) {
  if (!methods::is(img, "magick-image")) {
      filename <- system.file(paste0(img, ".png"), package = "ggcats")
      img <- magick::image_read(filename)

    asp <- getAR2(img)/asp
  }

  unit <- "native"
  if (any(size == Inf)) {
    x <- 0.5
    y <- 0.5
    width <- 1
    height <- 1
    unit <- "npc"
  } else if (by == "width") {
    width <- size/5
    height <- (size / asp)/5
  } else {
    width <- (size * asp)/5
    height <- size/5
  }

  if (hjust == 0 || hjust == "left") {
    x <- x + width/2
  } else if (hjust == 1 || hjust == "right") {
    x <- x - width/2
  }

  if (!is.null(image_fun)) {
    img <- image_fun(img)
  }


  if (is.null(colour)) {
    grobs <- list()
    grobs[[1]] <- rasterGrob(x = x,
                             y = y,
                             image = img,
                             default.units = unit,
                             height = height,
                             width = width,
                             interpolate = FALSE)
  } else {
    cimg <- lapply(seq_along(colour), function(i) {
      color_image(img, NULL, alpha[i])
    })

    grobs <- lapply(seq_along(x), function(i) {
      img <- cimg[[i]]
      if (angle[i] != 0) {
        img <- magick::image_rotate(img, angle[i])
        img <- magick::image_transparent(img, "white")
      }
      rasterGrob(x = x[i],
                 y = y[i],
                 image = img,
                 default.units = unit,
                 height = height,
                 width = width,
                 interpolate = FALSE
                 ## gp = gpar(rot = angle[i])
                 ## vp = viewport(angle=angle[i])
      )
    })
  }
  return(grobs)
}



getAR2 <- function(magick_image) {
  info <- magick::image_info(magick_image)
  info$width/info$height
}


compute_just <- getFromNamespace("compute_just", "ggplot2")

color_image <- function(img, color, alpha = NULL) {
  if (is.null(color))
    return(img)

  if (length(color) > 1) {
    stop("color should be a vector of length 1")
  }

  bitmap <- img[[1]]
  col <- col2rgb(color)
  bitmap[1, , ] <- as.raw(col[1])
  bitmap[2, , ] <- as.raw(col[2])
  bitmap[3, , ] <- as.raw(col[3])

  if (!is.null(alpha) && alpha != 1)
    bitmap[4, , ] <- as.raw(as.integer(bitmap[4, , ]) * alpha)

  magick::image_read(bitmap)
}


# library(ggplot2)
# set.seed(1)
#
# grid <- expand.grid(1:5, 3:1)
#
# df <- data.frame(x = grid[, 1],
#                  y = grid[, 2],
#                  image = c("nyancat", "bongo", "colonel", "grumpy", "hipster", "lil_bub", "maru",
#                            "mouth", "pop", "pop_close", "pusheen", "pusheen_pc", "toast", "venus", "shironeko"))
#  ggplot(df) +
# geom_cat(aes(x, y, cat =image), size = 5) +
#    xlim(c(0.25, 5.5)) +
#    ylim(c(0.25, 3.5))



library('Ecdat')
 data(incomeInequality)

 library('tidyverse')
 library('ggcats')
 library('gganimate')


 dat <-
   incomeInequality %>%
   select(Year, P99, median) %>%
   rename(income_median = median,
          income_99percent = P99) %>%
   pivot_longer(cols = starts_with("income"),
                names_to = "income",
                names_prefix = "income_")


dat99 <- dat[dat$income == "99percent", ]
datmedian <- dat[dat$income == "median", ]

dat$cat <- rep(NA, 132)

dat$cat[which(dat$income == "median")] <- "nyancat"
dat$cat[which(dat$income == "99percent")] <- rep(c("pop_close", "pop"), 33)

p <- ggplot(dat, aes(x = Year, y = value, group = income, color = income)) +
   geom_line(size = 2) +
   ggtitle("ggcats, a core package of the memeverse") +
   geom_cat(aes(cat = cat), size = 5) +
   # geom_point(size = 5, colour = as.numeric(as.factor(dat$cat))) +
   xlab("Cats") +
   ylab("Cats") +
   scale_x_discrete(labels = NULL) +
   scale_y_discrete(labels = NULL) +
   theme(legend.position = "none",
         plot.title = element_text(size = 20)) +
   transition_reveal(Year)
#
#  animate(p, height = 200, width =200)
#  anim_save("cats.gif")
#
#
#

 ggplot(mtcars) +
        geom_cat(aes(mpg, wt, size = cyl), cat = "toast")
