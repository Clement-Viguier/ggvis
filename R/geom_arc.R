#' Arcs
#'
#' `geom_arc` draws an arc of center (x, y), with a custom radius, angle and width.
#' Couyld be useful for custom data visualisation,
#' or distribution or vector fields. For those scenarios it's more convenient than geom-segment.
#'
#'
#'
#' @import ggplot2 grid
#' @importFrom ggplot2 ggproto
#' @importFrom plyr empty
#' @importFrom plyr mutate
#' @eval rd_aesthetics("geom", "arc")
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
#' iris %>% ggplot(aes(Sepal.Length , Petal.Length, colour = Species)) + geom_arc(width = 0.8, aes(angle = Petal.Width ,radius = 0.1*Sepal.Width)) + coord_fixed()
geom_arc <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      arrow = NULL,
                      arrow.fill = NULL,
                      lineend = "butt",
                      linejoin = "round",
                      res = pi/20,
                      reverse = T,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      res = res,
      reverse = reverse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomArc <- ggproto("GeomArc", Geom,
                    required_aes = c("x"),
                    non_missing_aes = c("radius", "y" ,"linetype", "size", "shape", "width", "angle", "start"),
                    default_aes = aes(radius = 1, y = 0, colour = "black", size = 0.5, linetype = 1, alpha = 1, width = 1, angle = 0, start = 0.5),

                    draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                          lineend = "butt", linejoin = "round", res = pi/20, reverse = T, na.rm = FALSE) {

                      data <- remove_missing(data, na.rm = na.rm,
                                             c("x", "y", "linetype", "size", "shape", "width", "angle", "start"),
                                             name = "geom_dash")
                      # print(data)
                      if (empty(data)) return(zeroGrob())

                      data$group <- 1:nrow(data)
                      # starts <- subset(data, select = c(-xend, -yend))
                      # ends <- plyr::rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"),
                      #                      warn_missing = FALSE)

                      data <- data %>% mutate(width = width * ( -1 * reverse),
                                              angle_start = width  * start / radius + angle,
                                              angle_end = angle - (width * (1 - start) / radius),
                                              nb_pts = 2 + floor(abs(angle_end - angle_start)/res))

                      pieces <- do.call(rbind, lapply(split(data, data$group), function(x) smooth_arc(x)))
                      pieces <- pieces %>% mutate(x = x + radius * cos(angles),
                                                  y = y + radius * sin(angles))

                      # print(pieces)


                      # pieces <- pieces[order(pieces$group),]

                      GeomPath$draw_panel(pieces, panel_params, coord, arrow = arrow,
                                          lineend = lineend)
                    },

                    draw_key = draw_key_path
)



smooth_arc <- function(df){
  angles <- seq(df$angle_start[1], df$angle_end[1], length.out = df$nb_pts[1])
  # print(angles)
  # print(df)
  return(cbind(df[rep(1, length(angles)),], angles))
}



