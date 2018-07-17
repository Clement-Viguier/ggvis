#' Dashes
#'
#' `geom_dash` draws a dash around points (x, y).
#' Couyld be useful for custom data visualisation,
#' or distribution or vector fields. For those scenarios it's more convenient than geom-segment.
#'
#'
#'
#' @import ggplot2 grid
#' @importFrom ggplot2 ggproto
#' @eval rd_aesthetics("geom", "dash")
#' @inheritParams layer
#' @inheritParams geom_point
#' @param arrow specification for arrow heads, as created by arrow().
#' @param arrow.fill fill colour to use for the arrow head (if closed). `NULL`
#'        means use `colour` aesthetic.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @export
#'
#' @examples
#'
#' iris %>% ggplot(aes(Species , Petal.Length)) + geom_dash(width = 0.8)
geom_dash <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         arrow = NULL,
                         arrow.fill = NULL,
                         lineend = "butt",
                         linejoin = "round",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDash,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDash <- ggproto("GeomDash", Geom,
                       required_aes = c("x", "y"),
                       non_missing_aes = c("linetype", "size", "shape", "width", "angle", "start"),
                       default_aes = aes( colour = "black", size = 0.5, linetype = 1, alpha = 1, width = 1, angle = 0, start = 0.5),

                       draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                             lineend = "butt", linejoin = "round", na.rm = FALSE) {

                         # print(data)
                         # print(1)
                         data <- remove_missing(data, na.rm = na.rm,
                                                c("x", "y", "linetype", "size", "shape", "width", "angle", "start"),
                                                name = "geom_dash")
                         # print(data)
                         if (empty(data)) return(zeroGrob())

                         # if (coord$is_linear()) {
                         #   coord <- coord$transform(data, panel_params)
                         #   arrow.fill <- arrow.fill %||% coord$colour
                         #   return(segmentsGrob(coord$x, coord$y,
                         #                       default.units = "native",
                         #                       gp = gpar(
                         #                         col = alpha(coord$colour, coord$alpha),
                         #                         fill = alpha(arrow.fill, coord$alpha),
                         #                         lwd = coord$size * .pt,
                         #                         lty = coord$linetype,
                         #                         lineend = lineend,
                         #                         linejoin = linejoin
                         #                       ),
                         #                       arrow = arrow
                         #   ))
                         # }

                         data$group <- 1:nrow(data)
                         # starts <- subset(data, select = c(-xend, -yend))
                         # ends <- plyr::rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"),
                         #                      warn_missing = FALSE)

                         starts <- data %>% mutate(x = x + (- start) * width * cos(angle),
                                                   y = y + (- start)* width * sin(angle))
                         ends <- data %>% mutate(x = x + (1 - start) * width * cos(angle),
                                                 y = y + (1 - start) * width * sin(angle))


                         pieces <- rbind(starts, ends)


                         pieces <- pieces[order(pieces$group),]

                         GeomPath$draw_panel(pieces, panel_params, coord, arrow = arrow,
                                             lineend = lineend)
                       },

                       draw_key = draw_key_path
)
