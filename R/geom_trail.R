#' geom_trail
#' @description Mark a trail with a plot just like the base plot type = "b".
#' You can also leave the dots blank, thus allowing use of text instead of
#' points (see examples).
#' @name geom_trail
#' @import ggplot2
#' @inheritParams ggplot2::geom_point
#' @section Additional arguments:
#' **`gap`** gap between points and lines
#' @section Aesthetics:
#' `geom_trail` understands the following aesthetics (required aesthetics
#' are in bold):
#'
#'   - **`x`**
#'   - **`y`**
#'   - `alpha`
#'   - `color`
#'   - `linetype`
#'   - `size` size of points - 0 for empty space
#'   - `linesize`
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_ribbon(aes(ymin = pressure - 50, ymax = pressure + 50), alpha = 0.2) +
#'   geom_trail()
#'
#' amd_aggr <-
#' amd %>%
#'   group_by(
#'     age_cut10 = cut_width(BaselineAge, 10),
#'     days_cut90 = cut_width(FollowupDays, 90, labels = seq(0, 810, 90))
#'   ) %>%
#'   summarise(mean_va = mean(VA_ETDRS_Letters))
#'
#' p <- ggplot(amd_aggr, aes(days_cut90, mean_va, color = age_cut10)) +
#'        theme_classic() +
#'        labs(
#'          x = "Follow up time [Days]", y = "Mean VA [ETDRS letters]",
#'          color = "Age strata"
#'        )
#'
#' p + geom_trail(aes(group = age_cut10))
#'
#' p + geom_trail(aes(group = age_cut10), size = 0) +
#'   geom_text(aes(label = round(mean_va, 0)), show.legend = FALSE)
#' @seealso
#' The geom was modified from the suggestion by user teunbrand on
#' Stackoverflow in
#' [this thread](https://stackoverflow.com/a/55857158/7941188)
#' @export

geom_trail <-
  function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomTrail,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...))
  }

#' @rdname geom_trail
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


#' grid draw method geom trail
#' @description underlying drawing method for paths in geom_trail
#' @rdname makeContent_trail
#' @author Teun van den Brand
#' @import grid
#' @importFrom utils head
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

