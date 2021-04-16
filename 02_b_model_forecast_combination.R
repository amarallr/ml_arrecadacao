#========================================================================================
# Forecast combinations.
#========================================================================================
get_model_forecast_combination <- function(p_model="") {
	
	my_ts         <- p_model$my_ts
	my_ts_train   <- p_model$my_ts_train
	my_h          <- p_model$my_h

	my_nnar       <- p_model$my_forecat_nnetar_forecast
	my_ets        <- forecast::forecast(forecast::ets(my_ts_train), h=my_h)
	my_tbats      <- forecast::forecast(forecast::tbats(my_ts_train, biasadj=TRUE), h=my_h)
	my_comb       <- (my_ets[["mean"]] + my_nnar[["mean"]] + my_tbats[["mean"]])/3

	p01 <- autoplot(my_ts) +
		autolayer(my_ets, series="ETS", PI=FALSE) +
		autolayer(my_nnar, series="NNAR", PI=FALSE) +
		autolayer(my_tbats, series="TBATS", PI=FALSE) +
		autolayer(my_comb, series="Combination") +
		xlab("Ano-calendário") + ylab("") +
		ggtitle("") +
		get_my_theme()

	p02 <- autoplot(window(my_ts, start=c(2017,01))) +
		autolayer(my_ets, series="ETS", PI=FALSE) +
		autolayer(my_nnar, series="NNAR", PI=FALSE) +
		autolayer(my_tbats, series="TBATS", PI=FALSE) +
		autolayer(my_comb, series="Combination") +
		xlab("Ano-calendário") + ylab("") +
		ggtitle("") +
		get_my_theme()

	# Seleciona o resulado dos resíduos de cada modelo.
	my_accuracy_comb <- c(ETS = forecast::accuracy(my_ets, my_ts)["Test set","MAE"],
						NNAR = forecast::accuracy(my_nnar, my_ts)["Test set","MAE"],
						TBATS = forecast::accuracy(my_tbats, my_ts)["Test set","MAE"],
						Combination = forecast::accuracy(my_comb, my_ts)["Test set","MAE"])

	# Armazena os resultados numa única variável de retorno.
	ds <- as.list("")
	ds$p01 <- p01
	ds$p02 <- p02	
	ds$my_accurary_comb <- my_accuracy_comb
	
	return(ds)
}
