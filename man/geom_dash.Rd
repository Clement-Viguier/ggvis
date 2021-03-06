% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_dash.R
\name{geom_dash}
\alias{geom_dash}
\title{Dashes}
\usage{
geom_dash(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", ..., arrow = NULL, arrow.fill = NULL,
  lineend = "butt", linejoin = "round", relative = F,
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
The `geom_dash` is characterised by specific aes() that encore different dimensions: `width`: the length of the dash, `angle`: the angle of the dash and `start` [0-1]the relative position of the starting point of the dash (0.5 by default).}

\item{arrow}{specification for arrow heads, as created by arrow().}

\item{arrow.fill}{fill colour to use for the arrow head (if closed). `NULL`
means use `colour` aesthetic.}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{relative}{if TRUE normalize the dash width relative to the maximum value of width to avoid overlapping (default = F).}
}
\description{
`geom_dash` draws a dash around points (x, y).
Couyld be useful for custom data visualisation,
or distribution or vector fields. For those scenarios it's more convenient than geom-segment.
}
\examples{

iris \%>\% ggplot(aes(Species , Petal.Length)) + geom_dash(width = 0.8)
}
