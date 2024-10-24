% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decorate_plot.r
\name{decorate_plot}
\alias{decorate_plot}
\title{Decorate Plot Function}
\usage{
decorate_plot(
  plot,
  title = NULL,
  subtitle = "http://www.weirddatascience.net | @WeirdDataSci",
  footer = NULL,
  title_colour = "#3c3f4a",
  subtitle_colour = title_colour,
  footer_colour = title_colour,
  bg_colour = NULL,
  bg_image = NULL,
  decorate_theme = NULL,
  rel_heights = c(0.1, 1, 0.05)
)
}
\arguments{
\item{plot}{ggplot plot object to centralize.}

\item{title}{Title text for the plot.}

\item{subtitle}{Subtitle text for the plot.}

\item{footer}{Footer text, for references and acknowledgements.}

\item{title_colour}{Colour for the plot title.}

\item{subtitle_colour}{Colour for the plot subtitle. Defaults to the same as the title.}

\item{footer_colour}{Colour for the plot footer. Defaults to the same as the title.}

\item{bg_colour}{Background colour for the plot. If given, this will override the main plot's background by setting it to be transparent, and cause this colour to span the entire plot without borders. Note that this argument will override any \code{plot.background} or \code{panel.background} settings in the \code{decorate_theme} argument.}

\item{bg_image}{Path to a background image for the page. This will span the entire plot without borders.}

\item{rel_heights}{Array of relative heights for the title, plot, and footer sections. Defaults to c(0.1, 1, 0.05).}

\item{theme}{A theme object to use in constructing the titles and footer.}
}
\description{
Decorate a ggplot with title, subtitle, footer reference text and full-page background image.
This makes use of the cowplot package to strip apart the basic ggplot object and recombine it into a presentation-ready image.
}
\examples{
df <- data.frame( x=rnorm(100), y=rnorm(100) )
gp <- ggplot( df, aes(x=x, y=y) ) + geom_point()
dp <- decorate_plot( gp, "Plot Title", "Weird Data Science", "Data: Normal Distribution" )
}
\keyword{plot}
