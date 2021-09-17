#' @importFrom ggplot2 %+replace%

#' @export
theme_weird <- 
	function(base_size = 12, base_family = "main") {
		ggplot2::theme_dark(base_size = base_size, base_family = base_family) %+replace%
		ggplot2::theme( 
				panel.background = ggplot2::element_rect(fill = "#222222", colour = "#222222"),
				plot.background = ggplot2::element_rect(fill = "#222222", colour = "#222222"),
				plot.title = ggplot2::element_text( size=16, color="#eeeeee", family="main", face="bold" ),
				plot.subtitle = ggplot2::element_text( size=14, color="#eeeeee", family="main" ),
				legend.key = ggplot2::element_rect(fill = "#222222"),
				legend.background = ggplot2::element_rect(fill = "#222222", colour = "#222222" ),
				legend.title = ggplot2::element_text( size=14, color="#eeeeee", family="main", margin = ggplot2::margin( t = 20 ) ),
				legend.text = ggplot2::element_text( size=12, color="#eeeeee", family="main", margin = ggplot2::margin( t = 20 ) ),
				axis.title.x = ggplot2::element_text( size=14, color="#eeeeee", family="main", margin = ggplot2::margin( t = 20 ) ),
				axis.title.y = ggplot2::element_text( size=14, color="#eeeeee", family="main", margin = ggplot2::margin( r = 20 ) ),
				axis.text = ggplot2::element_text( size=10, color="#eeeeee", family="main" ),
				panel.grid.major.x = ggplot2::element_blank(),
				panel.grid.major.y = ggplot2::element_line(colour = "#444444"),
				panel.grid.minor.x = ggplot2::element_blank(),
				panel.grid.minor.y = ggplot2::element_line(colour = "#444444" )
				)
}

#' @export
theme_weird_pdf <- 
	function(base_size = 12, base_family = "main"){
		ggplot2::theme_dark(base_size = base_size, base_family = base_family) %+replace%
		ggplot2::theme( 
				panel.background = ggplot2::element_rect(fill = "#222222", colour = "#222222"),
				plot.background = ggplot2::element_rect(fill = "#222222", colour = "#222222"),
				legend.key = ggplot2::element_rect(fill = "#222222"),
				legend.background = ggplot2::element_rect(fill = "#222222", colour = "#222222" ),
				legend.title = ggplot2::element_text( size=10, color="#eeeeee", family="main", margin = ggplot2::margin( t = 20 ) ),
				legend.text = ggplot2::element_text( size=8, color="#eeeeee", family="main", margin = ggplot2::margin( t = 20 ) ),
				axis.title.x = ggplot2::element_text( size=10, color="#eeeeee", family="main", margin = ggplot2::margin( t = 20 ) ),
				axis.title.y = ggplot2::element_text( size=10, color="#eeeeee", family="main", margin = ggplot2::margin( r = 20 ) ),
				axis.text = ggplot2::element_text( size=6, color="#eeeeee", family="main" ),
				panel.grid.major = ggplot2::element_line(colour = "#444444"),
				panel.grid.minor = ggplot2::element_line(colour = "#444444"),
				)
}

