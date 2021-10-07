#' @importFrom ggplot2 %+replace%

# Weird Data Science palette functions
# Taken from <https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2>
#' @export
weird_colours <- c(
						 `weird blue`		=	"#0b6788",
						 `blood red`		=	"#8a0707", 	# (Pantone "Blood")
						 `ufo green` 		=  "#3cd070", 	# (Pantone "UFO Green")
						 `ink grey` 		=	"#3c3f4a", 	# (Pantone "India Ink")
						 `pumpkin orange`	=	"#ff7518", 	# (Maerz and Paul "A Dictionary of Color". 1930)
						 `midnight blue`	=	"#061229", 	
						 `purpureus`		=	"#9a4eae",	# (Pantone "Purpureus")
						 `carcosa yellow` =	"#ffcc00",
						 `dark grey` 		=	"#222222", 	
						 `light grey` 		=	"#eeeeee" 	
						 )

#' Extract Weird Data Science colours as hex codes
#' 
#' @param ... Character names of weird_palette colours.
#' @export
weird_cols <- function(...) {

	cols <- c(...)

	# Return the entire palette if no arguments given
	if (is.null(cols))
		return (weird_colours)

	# Return the specified colours
	weird_colours[cols]

}

weird_palettes <- list(
							`weird`  = weird_cols( 	"weird blue", 
														 	"blood red", 
															"ufo green", 
															"ink grey", 
															"pumpkin orange", 
															"midnight blue", 
															"purpureus", 
															"carcosa yellow" ) )

#' Interpolate a weird data science color palette
#' 
#' @param palette Character name of palette
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export
#'
weird_pal <- function( palette = "weird", reverse = FALSE, ... ) {

	# Retrieve the palette from the list
	pal <- weird_palettes[[palette]]

	# Reverse it if necessary
	if( reverse ) 
		pal <- rev(pal)

	# Interpolate
	colorRampPalette(pal, ...)

}

#' Color scale constructor for weird data science colors
#'
#' @param palette Character name of palette in weird_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_color_weird <- function( palette = "main", discrete = TRUE, reverse = FALSE, ... ) {

	# Get the palette
	pal <- weird_pal(palette = palette, reverse = reverse)

	if( discrete ) {
		discrete_scale( "colour", paste0("weird_", palette), palette = pal, ... )
	} else {
		scale_color_gradientn( colours = pal(256), ... )
	}
}

#' Fill scale constructor for weird colors
#'
#' @param palette Character name of palette in weird_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_fill_weird <- function( palette = "main", discrete = TRUE, reverse = FALSE, ... ) {

	# Get the palette
	pal <- weird_pal(palette = palette, reverse = reverse)

	if (discrete) {
		discrete_scale( "fill", paste0("weird_", palette), palette = pal, ... )
	} else {
		scale_fill_gradientn( colours = pal(256), ... )
	}

}

