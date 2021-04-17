#========================================================================================
# Neural Network Time Series Forecasts
# Feed-forward neural networks with a single hidden layer and lagged inputs for 
# forecasting univariate time series.
#========================================================================================
get_model_forecast_nnetar <- function(p_ds) {
	#p_ds <- my_data_work

	# Prepara as variaveis com as series temporais.
	my_ds_orig   <- p_ds$my_ds_orig   # Dados na escala original.
	my_ds_ts     <- p_ds$my_ds_ts     # Dados na escala normalizada (0-100).
	my_ts        <- p_ds$my_ts        # Dados na escala normalizada (0-100) em objeto TS.
	my_ts_train  <- p_ds$my_ts_train  # Dados de treinamento.
	my_h         <- p_ds$my_h         # Horizonte do forecast.
	
	set.seed(111)
	# Treinamento da série temporal.
	my_nnetar          <- forecast::nnetar(y = my_ts_train, lambda="auto", scale.inputs = TRUE, repeats = 100, size=120)

	# Forecast.
	my_nnetar_forecast <- forecast::forecast(object=my_nnetar, h=my_h, robust = TRUE, lambda="auto", biasadj=TRUE, PI=TRUE)
	print(my_nnetar)
	print(forecast::accuracy(my_nnetar_forecast, my_ts))
	
	#----------------------------------------------------------------------------------------
	# Gráfico com o forecast.
	p01 <- ggplot2::autoplot(my_nnetar_forecast
					  , main=paste0("Receitas Admnistradas pela Receita Federal\nNeural Network Time Series Forecasts "
					  			    , my_nnetar_forecast$method)
					  , xlab=""
					  , ylab="") + get_my_theme()
	#p01

	#----------------------------------------------------------------------------------------
	# Resíduos: previsões da rede neural vs. valores reais.
	my_ds_residuos <- data.frame(dt_str=my_ds_ts$dt_str[1:nrow(as.data.frame(my_ts_train))]
							  , value=my_nnetar_forecast$residuals)
	p02         <- get_plot_resíduos(p_t=my_nnetar$method, p_ds=my_ds_residuos) + get_my_theme(p_legend_position = "none")
	#p02
	
	#----------------------------------------------------------------------------------------
	# Valores do forecast.
	my_ds_forecast <- as.data.frame(cbind(seq(DescTools::AddMonths(as.Date(my_ds_ts$dt_str[nrow(as.data.frame(my_ts_train))]), 1)
										   , by = "month"
										   , length.out = my_h)
									   ,  my_nnetar_forecast$mean))
	names(my_ds_forecast) <- c("dt_str", "nnetar_forecast")
	my_ds_forecast$dt_str <- base::as.Date(my_ds_forecast$dt_str, origin = "1970-01-01")
	my_ds_forecast$dt_str <- as.character(my_ds_forecast$dt_str)

	#----------------------------------------------------------------------------------------
	# Resultados
	#----------------------------------------------------------------------------------------
	my_ds_resultado  <- get_data_result (p_ds_orig     = my_ds_orig
									   , p_ds_ts       = my_ds_ts
									   , p_ds_forecast = my_ds_forecast)
	
	my_dt_ini_plot <- C_DT_INI_PLOT
	my_breaks      <- "6 months"
	p03 <- get_plot_result(p_ds=my_ds_resultado, p_dt_ini_plot = my_dt_ini_plot, p_breaks = my_breaks)

	my_dt_ini_plot <- "2017-12-01"
	my_breaks      <- "months"
	p04 <- get_plot_result(p_ds=my_ds_resultado, p_dt_ini_plot = my_dt_ini_plot, p_breaks = my_breaks)
	
	#----------------------------------------------------------------------------------------
	# Salva os resultados numa única variável de retorno.
	ds <- as.list("");
	ds$my_ds_ts                   <- my_ds_ts
	ds$my_ts                      <- my_ts
	ds$my_ts_train                <- my_ts_train
	ds$my_h                       <- my_h
	ds$my_forecat_nnetar          <- my_nnetar
	ds$my_forecat_nnetar_forecast <- my_nnetar_forecast
	ds$p01                        <- p01
	ds$my_ds_residuos             <- my_ds_residuos
	ds$p02                        <- p02
	ds$my_ds_forecast             <- my_ds_forecast
	ds$my_ds_resultado            <- my_ds_resultado
	ds$p03                        <- p03
	ds$p04                        <- p04
	
	return(ds)
	
}

#=========================================================================================
# Formata os dados do forecast em um data set.
#=========================================================================================
get_data_result <- function(p_ds_orig = "", p_ds_ts="", p_ds_forecast="") {
	
	ds <- ""
	ds <- sqldf::sqldf("
	select distinct a.dt_str
	from   ( select dt_str from p_ds_ts
			 union all
			 select dt_str from p_ds_forecast) a")
	
	ds <- sqldf::sqldf("
	select     t1.dt_str
			, t2.value             as value
			, t3.nnetar_forecast   as value_forecast
			, t4.value_orig
			, t4.value_norm
	from       ds            t1 
	left join  p_ds_ts       t2 on t1.dt_str = t2.dt_str
	left join  p_ds_forecast t3 on t1.dt_str = t3.dt_str
	left join  p_ds_orig     t4 on t1.dt_str = t4.dt_str
	order by   t1.dt_str;")
	
	ds$value_orig_forecast   <- NA
	n_min                    <- min(ds$value_orig[!is.na(ds$value)], na.rm = TRUE) 
	n_max                    <- max(ds$value_orig[!is.na(ds$value)], na.rm = TRUE) 
	value_orig_forecast      <- ((ds$value_forecast[!is.na(ds$value_forecast)] * ( n_max - n_min ))/100) + n_min
	ds$value_orig_forecast[  ( !is.na(ds$value_forecast) & !is.na(ds$value_forecast) )
							 | ( !is.na(ds$value_forecast) &  is.na(ds$value_forecast) )
							 ] <- value_orig_forecast
	
	ds$desvio_perc      <- round((ds$value_norm - ds$value_forecast)/ds$value_norm*100,2)
	ds$desvio_perc_orig <- round((ds$value_orig - ds$value_orig_forecast)/ds$value_orig*100,2)
	
	return(ds)
	
}

#=========================================================================================
# Gera o gráfico com os resultados: valores reais vs. forecast.
#=========================================================================================
get_plot_result <- function(p_ds="", p_dt_ini_plot="", p_breaks="") {
	
	t             <- "Receitas Administradas pela Receita Federal - Receita Bruta"
	t             <- paste0(t, " (", substring(min(p_ds$dt_str[p_ds$dt_str>=C_DT_INI_PLOT]), 1, 7), " a "
							, substring(max(p_ds$dt_str), 1, 7), ")")
	st            <- "Previsões utilizando Redes Neurais (vermelho)"
	c             <- "Fonte de dados: Secretaria Especial da Receita Federal\n"
	obs           <- ""
	
	p_ds$value <- p_ds$value_orig
	p01 <- get_my_plot1(p_t=t, p_st=st, p_c=c
						, p_ds=p_ds[p_ds$dt_str>=p_dt_ini_plot, ]
						, p_obs=""
						, p_break=p_breaks
						, p_span=0.15
						, p_min= 0
						, p_max= 175000 ) + 
		geom_rect(data = p_ds[p_ds$dt_str>=p_dt_ini_plot, ]
				  , xmin = as.Date("2020-02-01")
				  , xmax = as.Date("2020-07-01"), ymin = -Inf , ymax =Inf
				  , fill="yellow", alpha=0.005) + 
		get_my_theme(p_legend_position = "none")
	
	p02 <- p01 +
		geom_point(data=p_ds[p_ds$dt_str>"2018-12-01", ]
				   , aes(x=as.Date(dt_str), y=value_orig),  colour="black",  size=2.0) +
		geom_line(data=p_ds[p_ds$dt_str>"2018-12-01", ]
				  ,  aes(x=as.Date(dt_str), y=value_orig_forecast, group=1),  colour="red",  size=1.00, linetype = "solid") +
		geom_point(data=p_ds[p_ds$dt_str>"2018-12-01", ]
				   , aes(x=as.Date(dt_str), y=value_orig_forecast),  colour="red",  size=2.0) + 
		geom_rect(data = p_ds[p_ds$dt_str>=p_dt_ini_plot, ]
				  , xmin = as.Date("2020-02-01")
				  , xmax = as.Date("2020-07-01"), ymin = -Inf , ymax =Inf
				  , fill="yellow", alpha=0.002) +
		get_my_theme(p_legend_position = "none")
	
	return(p02)
}
	
