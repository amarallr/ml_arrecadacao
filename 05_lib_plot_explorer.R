#========================================================================================
# Gráficos exploratórios.
#========================================================================================
get_plot_explorer <- function(p_ds="") {
	#p_ds=my_data_work$ds_work
	p_explorer <- as.list("")
	my_breaks     <- "6 months"

	t             <- "Receitas Administradas pela Receita Federal - Receita Bruta"
	st            <- paste0("Período: "
							, substring(min(p_ds$dt_str[p_ds$dt_str>=C_DT_INI_PLOT]), 1, 7)
							, " a ", substring(max(p_ds$dt_str), 1, 7), "")
	c             <- "Fonte de dados: Secretaria Especial da Receita Federal"
	c             <- ""
	obs           <- ""
	
	my_color <- c(  "Arrecadação"
				   , "Energia"
				   , "Base Monetária"
				   , "IPCA Acum."
				   , "Dívida Pública"
				   , "IBOVESPA"
				   , "SELIC-A"
				   , "PIB"
				   , "Emprego Formal"
				   , "USD"
				   , "Export - Import")
	
	my_color_values <- c(  
		             "Arrecadação"       ="black"
				   , "Energia"           ="orange"
				   , "Base Monetária"    ="cyan"
				   , "IPCA Acum."        ="blue"
				   , "Dívida Pública"    ="red"
				   , "IBOVESPA"          ="pink"
				   , "SELIC-A"           ="orange"
				   , "PIB"               ="blue"
				   , "Emprego Formal"    ="blue"
				   , "USD"               ="green"
				   , "Export - Import"   ="darkblue")
	
	my_col_num <- length(my_color_values)
	
	#----------------------------------------------------------------------------------------
	p_ds$value       <- p_ds$vl_arrec
	p_explorer$p01   <- get_my_plot1(p_t=t
							     , p_st=st
								 , p_c=c
								 , p_ds=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
								 , p_obs=""
								 , p_break= my_breaks
								 , p_span = 0.15
								 , p_min  = 0
								 , p_max  = 0
								 , p_color=my_color[1]
								 , p_my_colors=my_color_values[1])
	
	#p_explorer$p01

	#----------------------------------------------------------------------------------------
	p_explorer$p02 <- p_explorer$p01 +
		              geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				       ,  aes(x=as.Date(dt_str), y=vl_energ_total, group=1, color=my_color[2])
				       ,  size=1.00, linetype = "solid") +
						scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p02

	#----------------------------------------------------------------------------------------
	p_explorer$p03 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_base_1833, group=1, color=my_color[3])
				  ,  size=1.00, linetype = "solid") + 
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p03
	
	#----------------------------------------------------------------------------------------
	p_explorer$p04 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_icpa_acum_norm, group=1, color=my_color[4])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p04
	
	#----------------------------------------------------------------------------------------
	p_explorer$p05 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_divida_publica_4469, group=1, color=my_color[5])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p05
	
	#----------------------------------------------------------------------------------------
	p_explorer$p06 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_bvsp, group=1, color=my_color[6])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p06
	
	#----------------------------------------------------------------------------------------
	p_explorer$p07 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_selic_4189, group=1, color=my_color[7])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p07
	
	#----------------------------------------------------------------------------------------
	p_explorer$p08 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_pib, group=1, color=my_color[8])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p08
	
	#----------------------------------------------------------------------------------------
	p_explorer$p09 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_empr_form, group=1, color=my_color[9])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p09
		
	#----------------------------------------------------------------------------------------
	p_explorer$p10 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_usd, group=1, color=my_color[10])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p10
	
	#----------------------------------------------------------------------------------------
	p_explorer$p11 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_export_net, group=1, color=my_color[11])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p11
	
	#========================================================================================
	p_explorer$p12 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_energ_total, group=1, color=my_color[2])
				  ,  size=1.00, linetype = "solid") +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_pib, group=1, color=my_color[8])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p12

	#----------------------------------------------------------------------------------------
	p_explorer$p13 <- p_explorer$p01 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_base_1833, group=1, color=my_color[3])
				  ,  size=1.00, linetype = "solid") + 
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_icpa_acum_norm, group=1, color=my_color[4])
				  ,  size=1.00, linetype = "solid") +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_divida_publica_4469, group=1, color=my_color[5])
				  ,  size=1.00, linetype = "solid") +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_selic_4189, group=1, color=my_color[7])
				  ,  size=1.00, linetype = "solid") +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_usd, group=1, color=my_color[10])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num])
	#p_explorer$p13
	
	#----------------------------------------------------------------------------------------
	p_ds$value       <- p_ds$vl_divida_publica_4469
	p_explorer$p14   <- get_my_plot1(p_t="Dívida pública vs. Base monetária vs. IPCA (acum.) vs. SELIC vs. USD"
									 , p_st=st
									 , p_c=c
									 , p_ds=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
									 , p_obs=""
									 , p_break= my_breaks
									 , p_span = 0.15
									 , p_min  = 0
									 , p_max  = 0
									 , p_color=my_color[5]
									 , p_my_colors=my_color_values[5])
	
	p_explorer$p14 <- p_explorer$p14 +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_base_1833, group=1, color=my_color[3])
				  ,  size=1.00, linetype = "solid") + 
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_icpa_acum_norm, group=1, color=my_color[4])
				  ,  size=1.00, linetype = "solid") +
		scale_color_manual(values=my_color_values[1:my_col_num]) +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_selic_4189, group=1, color=my_color[7])
				  ,  size=1.00, linetype = "solid") +
		geom_line(data=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
				  ,  aes(x=as.Date(dt_str), y=vl_usd, group=1, color=my_color[10])
				  ,  size=1.00, linetype = "solid") +
		get_my_theme()
	#p_explorer$p14
	
	#----------------------------------------------------------------------------------------
	return(p_explorer)
}


#========================================================================================
# Gráficos exploratórios
#========================================================================================
get_plot_bovespa <- function(p_ds) {
	
	my_breaks     <- "6 months"
	t             <- "Índice BOVESPA"
	t             <- paste0(t, " (", substring(min(p_ds$dt_str[p_ds$dt_str>=C_DT_INI_PLOT]), 1, 7), " a "
							, substring(max(p_ds$dt_str), 1, 7), ")")
	st            <- ""
	c             <- "Fonte de dados: R Package BatchGetSymbols \n"
	obs           <- ""
	
	p01   <- get_my_plot1(p_t=t, p_st=st, p_c=c
						 , p_ds=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
									 , p_obs=""
									 , p_break= my_breaks
									 , p_span = 0.15
									 , p_min  = 0
									 , p_max  = 0
						 			 , p_color="IBOVESPA"
						 			 , p_my_colors=c("IBOVESPA"="blue"))
	
	return(p01)
}
	
#========================================================================================
get_plot_resíduos <- function(p_t="", p_ds="") {
	
	my_breaks     <- "6 months"
	t             <- paste0("Resíduos FORECAST::", p_t)
	t             <- paste0(t, " (", substring(min(p_ds$dt_str[p_ds$dt_str>=C_DT_INI_PLOT]), 1, 7), " a "
							, substring(max(p_ds$dt_str), 1, 7), ")")
	st            <- ""
	c             <- ""
	obs           <- ""
	
	p01   <- get_my_plot1(p_t=t, p_st=st, p_c=c
						  , p_ds=p_ds[p_ds$dt_str>=C_DT_INI_PLOT, ]
						  , p_obs=""
						  , p_break= my_breaks
						  , p_span = 0.15
						  , p_min  = 0
						  , p_max  = 0
						  , p_color="Resíduos"
						  , p_my_colors=c("Resíduos"="red"))
	
	return(p01)
}
