#' geom_trail
#' @source modified from post on https://stackoverflow.com/a/55857158/7941188
#' @author Teun van den Brand / Tjebo Heeren
#' @rdname geom_trail
#' @format NULL
#' @usage NULL
#' @import grid
#' @import ggplot2
#' @export

GeomTrail <- ggplot2::ggproto(
  "GeomTrail", ggplot2::GeomPoint,
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Default geom point behaviour
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    coords <- coord$transform(data, panel_params)

    if (unique(coords$size == 0)) {
      my_points <- NULL
    } else {
      my_points <- grid::pointsGrob(
        coords$x,
        coords$y,
        pch = coords$shape,
        gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
          lwd = coords$stroke * .stroke / 2
        )
      )
    }

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) {
      return(zeroGrob())
    }

    # New behaviour
    ## Convert x and y to units
    x <- unit(coords$x, "npc")
    y <- unit(coords$y, "npc")

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)

    ## Make custom grob class
    my_path <- grid::grob(
      x = x, y = y,
      mult = munched$gap * .pt,
      name = "trail",
      gp = grid::gpar(
        col = alpha(munched$colour, munched$alpha)[!end],
        fill = alpha(munched$colour, munched$alpha)[!end],
        lwd = munched$linesize * .pt,
        lty = munched$linetype,
        lineend = "butt",
        linejoin = "round",
        linemitre = 10
      ),
      vp = NULL,
      ### Now this is the important bit:
      cl = "trail"
    )

    ## Combine grobs
    ggplot2:::ggname(
      "geom_trail",
      grid::grobTree(my_path, my_points)
    )
  },
  # Adding some defaults for lines and gap
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", size = 1.5, fill = NA, alpha = NA, stroke = 0.5,
    linesize = 0.5, linetype = 1, gap = .9,
  )
)

#' makeContent.trail
#' @description underlying drawing method for paths in geom_trail
#' @rdname geom_trail
#' @author Teun van den Brand
#' @import grid
#' @param x grob object passed to method
#' @export

makeContent.trail <- function(x){
  # Make hook for drawing
  # Convert npcs to absolute units
  x_new <- convertX(x$x, "mm", TRUE)
  y_new <- convertY(x$y, "mm", TRUE)

  # Do trigonometry stuff
  hyp <- sqrt(diff(x_new)^2 + diff(y_new)^2)
  sin_plot <- diff(y_new) / hyp
  cos_plot <- diff(x_new) / hyp

  diff_x0_seg <- head(x$mult, -1) * cos_plot
  diff_x1_seg <- (hyp - head(x$mult, -1)) * cos_plot
  diff_y0_seg <- head(x$mult, -1) * sin_plot
  diff_y1_seg <- (hyp - head(x$mult, -1)) * sin_plot

  x0 = head(x_new, -1) + diff_x0_seg
  x1 = head(x_new, -1) + diff_x1_seg
  y0 = head(y_new, -1) + diff_y0_seg
  y1 = head(y_new, -1) + diff_y1_seg
  keep <- unclass(x0) < unclass(x1)

  # Remove old xy coordinates
  x$x <- NULL
  x$y <- NULL

  # Supply new xy coordinates
  x$x0 <- unit(x0, "mm")[keep]
  x$x1 <- unit(x1, "mm")[keep]
  x$y0 <- unit(y0, "mm")[keep]
  x$y1 <- unit(y1, "mm")[keep]

  # Set to segments class
  class(x)[1] <- 'segments'
  x
}

#' geom_trail
#' @description Mark your trail with a plot just like the good old base plot type = b
#' modified from post on https://stackoverflow.com/a/55857158/7941188
#' @rdname geom_trail
#' @import ggplot2
#' @inheritParams ggplot2::geom_point
#' @param gap gap between points and lines
#' @section Aesthetics:
#' \code{geom_trail} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item **`x`**
#'   \item **`y`**
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size} size of points -
#'   also used for calculation of empty space
#'   \item \code{linesize}
#' }
#' @examples
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_ribbon(aes(ymin = pressure - 50, ymax = pressure + 50), alpha = 0.2) +
#'   geom_trail()
#' @export

geom_trail <-
function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomTrail,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

