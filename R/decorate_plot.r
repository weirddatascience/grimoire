#' Decorate Plot Function
#'
#' Decorate a ggplot with title, subtitle, footer reference text and full-page background image.
#' This makes use of the cowplot package to strip apart the basic ggplot object and recombine it into a presentation-ready image.
#' @param title Title text for the plot.
#' @param subtitle Subtitle text for the plot.
#' @param footer Footer text, for references and acknowledgements.
#' @param plot ggplot plot object to centralize.	
#' @param bg_colour Background colour for the plot. If given, this will override the main plot's background by setting it to be transparent, and cause this colour to span the entire plot without borders. Note that this argument will override any \code{plot.background} or \code{panel.background} settings in the \code{decorate_theme} argument.
#' @param bg_image Path to a background image for the page. This will span the entire plot without borders.
#' @param theme A theme object to use in constructing the titles and footer.
#' @param rel_heights Array of relative heights for the title, plot, and footer sections. Defaults to c(0.1, 1, 0.05).
#' @keywords plot
#' @export
#' @examples
#' df <- data.frame( x=rnorm(100), y=rnorm(100) )
#' gp <- ggplot( df, aes(x=x, y=y) ) + geom_point()
#' dp <- decorate_plot( gp, "Plot Title", "Weird Data Science", "Data: Normal Distribution" )
 
decorate_plot <- function( plot, 
								  	title = NULL, 
								  	subtitle="http://www.weirddatascience.net | @WeirdDataSci", 
									footer = NULL,
									bg_colour = NULL,
									bg_image = NULL,
									decorate_theme = NULL,
									rel_heights = c( 0.1, 1, 0.05 ) ) {

	bg_theme <- NULL
	# Add the plot background and panel background if bg_colour is given
	if( !is.null( bg_colour ) ) {

		bg_theme <-
			theme(	plot.background = element_rect( colour="transparent", fill=bg_colour ),
						panel.background = element_rect( colour="transparent", fill=bg_colour ) )

	}

	# Cowplot trick for ggtitle
	plot_title <- 
		cowplot::ggdraw() + 
		cowplot::draw_label( title, 
					  	fontfamily="main", 
						fontface="bold",
						colour = "#3c3f4a", 
						size=20, 
						hjust=0, vjust=1, 
						x=0.02, y=0.88) +
		cowplot::draw_label( subtitle, 
					  	fontfamily="main", 
						fontface="bold",
						colour = "#3c3f4a", 
						size=12, 
						hjust=0, vjust=1, 
						x=0.02, y=0.40) +
		decorate_theme +
		bg_theme

	data_label <- 
		cowplot::ggdraw() +
		cowplot::draw_label( footer, 
					  	fontfamily="main", 
						colour = "#3c3f4a", 
						size=8, hjust=1, x=0.98 ) +
		decorate_theme +
		bg_theme

	# Assert a background colour over the entire main plot, if supplied.
	# (This is particularly useful when decorating maps made with geom_sf
	# that assert their own aspect ratio.)
	if( !is.null( bg_colour ) ) {

		# Make the base plot background elements transparent
		plot <- 
			plot + 
			theme( 	plot.background = element_blank(),
						panel.background = element_blank() )

		# Place the now-transparent plot into a themed ggdraw plot
		plot <-
			cowplot::ggdraw( plot ) +
			bg_theme

	}

	combined_plot <- 
		cowplot::plot_grid( 	plot_title, 
								 plot, 
								 data_label, 
								 ncol=1, 
								 rel_heights=rel_heights ) +
	decorate_theme

	# Add the background image if it has been specified.
	if( !is.null( bg_image ) ) {
		decorated_plot <- 
			cowplot::ggdraw() +
			cowplot::draw_image( bg_image, scale=1.4 ) +
			cowplot::draw_plot( combined_plot )
	} else {
		decorated_plot <-
			combined_plot
	}

	return( decorated_plot )

}
