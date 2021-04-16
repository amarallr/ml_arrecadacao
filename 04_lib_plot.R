#===================================================================================================
# Template de plot theme.
#===================================================================================================
get_my_theme <- function(p_legend_position="top", p_plot_title_size=24
						 , p_plot_title_position="top"
						 , p_axis_title_x_size =09
						 , p_axis_title_y_size =18
						 , p_axis_text_x_size  =09
						 , p_axis_text_y_size  =24) {
	
	my_theme <- ggplot2::theme_bw() + 
		ggplot2::theme(  panel.border = element_blank(),
				#panel.grid.major = element_blank(),
				#panel.grid.minor = element_blank(),
				panel.grid.major.x = element_blank(),
				panel.grid.minor.x = element_blank(),
				axis.line       = element_line(colour = "darkgrey"),
				plot.title      = element_text(angle =  0, hjust = 0.5, size = p_plot_title_size, face = 'bold'),
				plot.subtitle   = element_text(angle =  0, hjust = 0.5, size = p_plot_title_size*0.60),
				legend.title    = element_text(angle =  0, hjust = 0.5, size = 09),
				axis.title.x    = element_text(angle =  0, hjust = 0.5, size = p_axis_title_x_size, colour = 'black', face = 'bold'),
				axis.title.y    = element_text(angle = 90, hjust = 0.5, size = p_axis_title_y_size, colour = 'black'),
				axis.text.x     = element_text(angle = 90, hjust = 1.0, size = p_axis_text_x_size),
				axis.text.y     = element_text(angle =  0, hjust = 1.0, size = p_axis_text_y_size),
				legend.text     = element_text(angle =  0, hjust = 1.0, size = 09),
				legend.position = p_legend_position,
				plot.caption    = element_text(color = "black", face = 'italic'))
	
	return(my_theme)
}

#===================================================================================================
# Gera grafico modelo [1].
#===================================================================================================
get_my_plot1 <- function(p_t="", p_st="", p_c="", p_ds="", p_obs="", p_break="", p_span=0.15
						 , p_min=0, p_max=0, p_color="Arrecadação", p_my_colors=c("Arrecadação"="black")) {

	#p_ds <- ds_data_forecast
	
	l_min <- p_min
	if(l_min <= 0) {
		l_min       <- min(p_ds$value, na.rm = TRUE) + 0.0
	}
	
		
	l_max <- p_max
	if(l_max <= 0) {
		l_max       <- max(p_ds$value, na.rm = TRUE) + 0.0
	}
	
	l_range     <- l_max - l_min
	
	yUserLimits <- c(l_min, l_max)
	yUserBreaks <- seq(l_min, l_max, by=l_range/4)
	
	p <- ggplot() + 
		geom_line(data=p_ds,  aes(x=as.Date(dt_str), y=value, group=1,  color=p_color),  size=1.0, linetype = "solid") +
		scale_color_manual(values=p_my_colors)	+
		scale_y_continuous(labels = C_POINT, breaks=yUserBreaks, limits=yUserLimits) + 
		scale_x_date(breaks = date_breaks(p_break), date_labels = "%Y-%m") +
		labs(title=p_t, subtitle=p_st, x="", y="", caption=p_c, color="") + 
		theme_bw() +
		get_my_theme ()

	return(p)
}
