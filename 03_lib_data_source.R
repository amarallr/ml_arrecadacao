#install.packages("devtools")
#devtools::install_github('wilsonfreitas/rbcb')
#install.packages("rvest")
#install.packages("DescTools")
#install.packages("psych")
#install.packages("GGally")

#=========================================================================================
# Constantes e opções de ambiente.
#=========================================================================================
C_POINT           <- scales::format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)  
C_DT_INI          <- "1995-01-01" 
C_DT_INI_PLOT     <- "1995-01-01" 
#C_DT_INI_PLOT     <- "2005-01-01" 
C_MONTHS_FORECAST <- 3
options(digits=2)

#=========================================================================================
# Coleta dados do IBOVESPA.
#=========================================================================================
get_financial_data <- function(p_code="^BVSP") {
	ds <- ""
	ds <- as.data.frame(BatchGetSymbols::BatchGetSymbols('^BVSP', first.date = as.Date(C_DT_INI),
															 #last.date = as.Date('2021-01-31'),
															 freq.data="monthly",
														     be.quiet=TRUE))
	
	ds$dt_str      <- as.character(ds$df.tickers.ref.date)
	ds$dt_str_mes  <- paste0(substring(as.character(ds$df.tickers.ref.date), 1, 7), "-01")
	ds$value_orig  <- ds$df.tickers.price.close
	ds$value       <- ds$value_orig
	ds$value_norm <- ( ds$value - min(ds$value) ) /( max(ds$value) - min(ds$value) ) * 100
	ds$value       <- ds$value_norm
	#ds$value       <- ds$value_orig
	return(ds)	
}

#=========================================================================================
# Função principal de coleta dados pela API do Banco Central.
#=========================================================================================
get_rbcb_series <- function(p) {
	#?rbcb::get_series()
	ds            <- ""
	ds            <- rbcb::get_series(c(value_orig = p$code)
									  , start_date = C_DT_INI #p$dt_start
									  , end_date   = ""           #p$dt_end
									  , as         = "data.frame")
	
	ds$code       <- p$code
	ds$series     <- p$series
	ds$dt_str     <- as.character(ds$date)
	
	ds            <- ds[ds$dt_str >=C_DT_INI, ] # Filtra apenas os dados a partir de "1995-01-01".
	ds$value_orig <- ds$value
	ds$value_norm <- ( ds$value - min(ds$value) ) /( max(ds$value) - min(ds$value) ) * 100 # Normaliza os dados.
	ds$value      <- ds$value_norm
	#ds$value      <- ds$value_orig
	
	return(ds)
	
}

#=========================================================================================
# Coleta dos dados da arrecadação.
#=========================================================================================
get_ipea_series_arrecadacao <- function() {
	# SRF12_TOTGER12 - Arrecadação das receita federais - receita bruta
	# SRF12_TOTREC12 - Receita administrada pela SRF - receita bruta
	# s <- ipeadatar::available_series(language = "br")
	# s <- as.data.frame(s)
	# View(s)
	# s_trib <- s$source %like% c('%SRF%') & s$status %like% c('%Ativa%')
	# s_trib <- s[s_trib, ]
	# View(s_trib)
	
	#?ipeadatar::ipeadata

	ds            <- ipeadatar::ipeadata(code="SRF12_TOTREC12")
	ds            <- as.data.frame(ds)
	
	ds$dt_str     <- as.character(ds$date)
	ds$value_orig <- ds$value

	ds            <- ds[ds$dt_str >= C_DT_INI, ]
	
	ds$value_norm <- ( ds$value - min(ds$value) ) /( max(ds$value) - min(ds$value) ) * 100
	ds$value      <- ds$value_norm
	#ds$value      <- ds$value_orig
	
	return(ds)
	
}

#=========================================================================================
# Coleta dos dados do PIB.
#=========================================================================================
get_data_pib <- function() {
	ds       <- ""
	p        <- as.list("4380 - PIB mensal - valores correntes")
	p$code   <- 4380
	p$series <- "4380 - PIB mensal - valores correntes" 
	ds       <- get_rbcb_series(p)
	return(ds)
}

#=========================================================================================
# Coleta dos dados do consumo de energia elétrica comercial.
#=========================================================================================
get_data_energia_comercial <- function() {
	ds       <- ""
	p        <- as.list("1402 - Consumo de energia elétrica Brasi (IBGE) - Comercial")
	p$code   <- 1402
	p$series <- "1402 - Consumo de energia elétrica Brasi (IBGE) - Comercial" 
	ds       <- get_rbcb_series(p)
	return(ds)
}    

#=========================================================================================
# Coleta dos dados do consumo de energia elétrica residencial.
#=========================================================================================
get_data_energia_residencial <- function() {
	ds       <- ""
	p        <- as.list("1403 - Consumo de energia elétrica Brasi (IBGE) - Residencial")
	p$code   <- 1403
	p$series <- "1403 - Consumo de energia elétrica Brasi (IBGE) - Residencial" 
	ds       <- get_rbcb_series(p)
	return(ds)
}    

#=========================================================================================
# Coleta dos dados do consumo de energia elétrica industrial.
#=========================================================================================
get_data_energia_industrial <- function() {
	ds       <- ""
	p        <- as.list("1404 - Consumo de energia elétrica Brasi (IBGE) - Industrial")
	p$code   <- 1404
	p$series <- "1404 - Consumo de energia elétrica Brasi (IBGE) - Industrial" 
	ds       <- get_rbcb_series(p)
	return(ds)
}    

#=========================================================================================
# Coleta dos dados do consumo de energia elétrica "outros".
#=========================================================================================
get_data_energia_outros <- function() {
	ds       <- ""
	p        <- as.list("1405 - Consumo de energia elétrica Brasi (IBGE) - Outros")
	p$code   <- 1405
	p$series <- "1405 - Consumo de energia elétrica Brasi (IBGE) - Outros" 
	ds       <- get_rbcb_series(p)
	return(ds)
}    

#=========================================================================================
# Coleta dos dados do consumo de energia elétrica; TOTAL.
#=========================================================================================
get_data_energia_total <- function() {
	ds       <- ""
	p        <- as.list("1406 - Consumo de energia elétrica Brasi (IBGE)")
	p$code   <- 1406
	p$series <- "1406 - Consumo de energia elétrica Brasi (IBGE)" 
	ds       <- get_rbcb_series(p)
	return(ds)
}    

#=========================================================================================
# Coleta dos dados de empregados.
#=========================================================================================
get_data_empregados <- function() {
	ds       <- ""
	p        <- as.list("24371 - Empregados no setor privado e público - PNADC Unidades (mil)")
	p$code   <- 24371
	p$series <- "24371 - Empregados no setor privado e público - PNADC Unidades (mil)" 
	ds       <- get_rbcb_series(p)
	return(ds)
}    

#=========================================================================================
# Coleta dos dados: Taxa de câmbio - Livre - Dólar americano (compra) - Fim de período - 
# mensal Sisbacen PTAX800.
#=========================================================================================
get_data_dolar <- function() {
	ds       <- ""
	p        <- as.list("Sisbacen PTAX800")
	p$code   <- 3695
	p$series <- "Taxa de câmbio - Livre - Dólar americano (compra) - Fim de período - mensal Sisbacen PTAX800" 
	ds_c     <- get_rbcb_series(p)
	
	
	ds       <- ""
	p        <- as.list("Sisbacen PTAX800")
	p$code   <- 3696
	p$series <- "Taxa de câmbio - Livre - Dólar americano (venda) - Fim de período - mensal Sisbacen PTAX800" 
	ds_v     <- get_rbcb_series(p)
	
	ds <- sqldf::sqldf("
	select 		'0' as code
			,	'Taxa de câmbio - Livre - Dólar americano - Fim de período - mensal Sisbacen PTAX800' as series
			,	c.dt_str 
			,	round((c.value_orig + v.value_orig)/2, 2) as value_orig
	from 		ds_c c 
	join 		ds_v v on c.dt_str = v.dt_str
		  ")
	ds$value_norm <- round((ds$value_orig - min(ds$value_orig))/(max(ds$value_orig) - min(ds$value_orig))*100, 2)
	ds$value      <- ds$value_norm
	
	return(ds)
}    

#=========================================================================================
# Coleta dos dados: Empregos formais gerados.
#=========================================================================================
get_data_empregos_formais <- function() {
	
	# 13564	Empregos formais gerados - Sul
	# 13901	Empregos formais gerados - Sudeste
	# 21991	Empregos formais gerados - Região Centro-Oeste
	# 28152	Empregos formais gerados - Norte
	# 13941	Empregos formais gerados - Nordeste
	
	l_codes      <- c(13564, 13901, 21991, 28152, 13941)
	l_first_time <- TRUE
	ds_aux       <- ""
	ds           <- ""
	
	for (c in l_codes) {
		ds_aux   <- ""
		p        <- as.list(paste0("Empregos formais gerados (", c, ")"))
		p$code   <- c
		p$series <- paste0(as.list("Empregos formais gerados"), " (", c, ")")
		ds_aux   <- get_rbcb_series(p)
		if(l_first_time) {
			l_first_time <- FALSE
			ds   <- sqldf::sqldf("select * from ds_aux where date is null;")
		}
		ds <- rbind(ds, ds_aux)
		
		#print(p$series)				   
		
	}
	
	# Calcula o valor total
	ds2 <- sqldf::sqldf("
	select 		date                               as date2
			, 	sum(value_orig)                    as value_orig
			,	0			                       as code2
			,   'Empregos formais gerados (total)' as series2
			,	dt_str                             as dt_str2
	from 		ds
	group by	date2
			,	code2
			,	series2
			,	dt_str2
	order by	date")
	
	ds2$value_norm <- ( ds2$value_orig - min(ds2$value_orig) ) / 
					  ( max(ds2$value_orig) - min(ds2$value_orig) ) * 100
	ds2$value      <- ds2$value_norm
	
	# Concatena os datasets
	ds3             <- sqldf::sqldf("select * from ds union all select * from ds2")

	return(ds3)
}    

#=========================================================================================
# Coleta de dados do IPCA.
#=========================================================================================
get_data_ipca <- function() {
	ds       <- ""
	p        <- as.list("433 - Índice nacional de preços ao consumidor-amplo (IPCA) Var. % mensal")
	p$code   <- 433
	p$series <- "433 - Índice nacional de preços ao consumidor-amplo (IPCA) Var. % mensal" 
	ds       <- get_rbcb_series(p)
	return(ds)
}   

#=========================================================================================
# Coleta de dados da base monetária.
#=========================================================================================
get_data_base_monetaria_1833 <- function() {
	ds       <- ""
	p        <- as.list("1833 - Base Monetária Ampliada (saldo em final de período)")
	p$code   <- 1833
	p$series <- "1833 - Base Monetária Ampliada (saldo em final de período)"
	ds       <- get_rbcb_series(p)
	return(ds)
}   

#=========================================================================================
get_data_divida_publica_4469 <- function() {
	ds       <- ""
	p        <- as.list("4469 - Dívida Líquida do Setor Público - Saldos em R$ milhões - Total - Governo Federal")
	p$code   <- 4469
	p$series <- "4469 - Dívida Líquida do Setor Público - Saldos em R$ milhões - Total - Governo Federal"
	ds       <- get_rbcb_series(p)
	return(ds)
}   


#=========================================================================================
get_data_selic_4390 <- function() {
	ds       <- ""
	p        <- as.list("4390 - Taxa de juros - Selic acumulada no mês")
	p$code   <- 4390
	p$series <- "4390 - Taxa de juros - Selic acumulada no mês"
	ds       <- get_rbcb_series(p)
	return(ds)
}   

#=========================================================================================
get_data_selic_4189 <- function() {
	ds       <- ""
	p        <- as.list("4189 - Taxa de juros - Selic acumulada no mês anualizada base 252")
	p$code   <- 4189
	p$series <- "4189 - Taxa de juros - Selic acumulada no mês anualizada base 252"
	ds       <- get_rbcb_series(p)
	return(ds)
}   

#=========================================================================================
get_data_massa_salarial_22078 <- function() {
	ds       <- ""
	p        <- as.list("22078 - Massa salarial ampliada - PNADC (média móvel trimestral)")
	p$code   <- 22078
	p$series <- "22078 - Massa salarial ampliada - PNADC (média móvel trimestral)"
	ds       <- get_rbcb_series(p)
	return(ds)
}   

#=========================================================================================
get_data_export_22708 <- function() {
	ds       <- ""
	p        <- as.list("22708 - Exportação de bens - Balanço de Pagamentos - mensal")
	p$code   <- 22708
	p$series <- "22708 - Exportação de bens - Balanço de Pagamentos - mensal"
	ds       <- get_rbcb_series(p)
	return(ds)
}   

#=========================================================================================
get_data_import_22709 <- function() {
	ds       <- ""
	p        <- as.list("22709 - Importação de bens - Balanço de Pagamentos - mensal")
	p$code   <- 22709
	p$series <- "22709 - Importação de bens - Balanço de Pagamentos - mensal"
	ds       <- get_rbcb_series(p)
	return(ds)
}   

#=========================================================================================
get_data_export_net <- function(p_export="", p_import="") {
	
	ds1 <- sqldf::sqldf("
	select distinct dt_str 
	from (	select dt_str from p_export
			union all
			select dt_str from p_import)
	order by dt_str;")
	
	ds2 <- sqldf::sqldf("
	select     t1.dt_str 
			,  t2.value_orig                           as vl_export_orig
			,  t3.value_orig                           as vl_import_orig
			,  round(t2.value_orig - t3.value_orig, 2) as value
	from 	   ds1      t1
	left join  p_export t2 on t1.dt_str = t2.dt_str
	left join  p_import t3 on t1.dt_str = t3.dt_str
	order by   t1.dt_str;")
	
	ds2$value_orig <- ds2$value
	ds2$value_norm <- ( ds2$value - min(ds2$value) ) /( max(ds2$value) - min(ds2$value) ) * 100
	ds2$value      <- ds2$value_norm

	return(ds2)
}
	
#=========================================================================================
get_data_work <- function() {
	ds                      <- ""
	print(paste0(Sys.time(), "    ", "Coletando dados da arrecadação (IPEA)..."))
	ds_arrec                <- get_ipea_series_arrecadacao()
	
	print(paste0(Sys.time(), "    ", "Coletando dados do PIB (BCB)..."))
	ds_pib                  <- get_data_pib()
	
	print(paste0(Sys.time(), "    ", "Coletando dados do consumo de energia elétrica (BCB)..."))
	print(paste0(Sys.time(), "    ", "    Consumo total..."))
	ds_energia_total        <- get_data_energia_total()
	
	print(paste0(Sys.time(), "    ", "    Consumo comercial..."))
	ds_energia_comercial    <- get_data_energia_comercial()
	
	print(paste0(Sys.time(), "    ", "    Consumo residencial..."))
	ds_energia_residencial  <- get_data_energia_residencial()
	
	print(paste0(Sys.time(), "    ", "    Consumo industrial..."))
	ds_energia_industrial   <- get_data_energia_industrial()
	
	print(paste0(Sys.time(), "    ", "    Consumo energia 'outros'..."))
	ds_energia_outros       <- get_data_energia_outros()
	
	print(paste0(Sys.time(), "    ", "Coletando dados empregados PNADC (BCB)..."))
	ds_empregados           <- get_data_empregados()
	
	print(paste0(Sys.time(), "    ", "Coletando dados IPCA (BCB)..."))
	ds_ipca                 <- get_data_ipca()
	
	print(paste0(Sys.time(), "    ", "Coletando dados empregos formais (BCB)..."))
	ds_empregos_formais     <- get_data_empregos_formais()
	
	print(paste0(Sys.time(), "    ", "Coletando dados do IBOVESPA..."))
	ds_bvsp <- get_financial_data(p_code="^BVSP")
	
	print(paste0(Sys.time(), "    ", "Coletando dados do Dolar/Real (BCB)..."))
	ds_usd <- get_data_dolar()
	
	print(paste0(Sys.time(), "    ", "Coletando dados da base monetária (BCB)..."))
	ds_base_1833 <- get_data_base_monetaria_1833()
	
	print(paste0(Sys.time(), "    ", "Coletando dados da dívida pública (BCB)..."))
	ds_divida_publica_4469 <- get_data_divida_publica_4469()
	
	print(paste0(Sys.time(), "    ", "Coletando dados da SELIC (BCB)..."))
	ds_selic_4390 <- get_data_selic_4390()
	
	print(paste0(Sys.time(), "    ", "Coletando dados da SELIC anualizada (BCB)..."))
	ds_selic_4189 <- get_data_selic_4189()
	
	print(paste0(Sys.time(), "    ", "Coletando dados da Massa salarial ampliada - PNADC (média móvel trimestral) [22078]..."))
	ds_massa_salarial_22078 <- get_data_massa_salarial_22078()

	print(paste0(Sys.time(), "    ", "Coletando dados de Exportação de bens - Balanço de Pagamentos - mensal [22708]..."))
	ds_export_22708 <- get_data_export_22708()
	
	print(paste0(Sys.time(), "    ", "Coletando dados de Importação de bens - Balanço de Pagamentos - mensal [22709]..."))
	ds_import_22709 <- get_data_import_22709()
	
	print(paste0(Sys.time(), "    ", "Coletando dados de exportações NET..."))
	ds_export_net <- get_data_export_net(p_export=ds_export_22708, p_import=ds_import_22709)
	
	print(paste0(Sys.time(), "    ", "Preparando dados..."))
	
	# Cria uma lista com todas as datas diponíveis.
	ds_dt_str <- sqldf::sqldf("
					   select distinct a.dt_str from (
					   select dt_str from ds_arrec
					   union all
					   select dt_str from ds_pib
					   union all
					   select dt_str from ds_energia_total
					   union all
					   select dt_str from ds_energia_comercial
					   union all
					   select dt_str from ds_energia_residencial
					   union all
					   select dt_str from ds_energia_industrial
					   union all
					   select dt_str from ds_energia_outros
					   union all
					   select dt_str from ds_empregados
					   union all
					   select dt_str from ds_ipca
					   union all
					   select dt_str from ds_empregos_formais
					   union all
					   select dt_str_mes as dt_str from ds_bvsp
					   union all 
					   select dt_str from ds_usd
					   union all 
					   select dt_str from ds_base_1833
					   union all 
					   select dt_str from ds_divida_publica_4469
					   union all 
					   select dt_str from ds_selic_4390
					   union all 
					   select dt_str from ds_selic_4189
					   union all 
					   select dt_str from ds_massa_salarial_22078
					   union all 
					   select dt_str from ds_export_22708
					   union all 
					   select dt_str from ds_import_22709
                       ) a
					   where a.dt_str is not null")

	ds <- sqldf::sqldf("
	select    
				t0.dt_str
			,   t1.value   as vl_arrec
			,   t2.value   as vl_pib
			,   t3.value   as vl_energ_total
			,   t4.value   as vl_energ_comercial
			,   t5.value   as vl_energ_residencial
			,   t6.value   as vl_energ_industrial
			,   t7.value   as vl_energ_outros
			,   t8.value   as vl_empregados
			,   t9.value   as vl_ipca
			,   t1X.value  as vl_empr_form
			,   t10.value  as vl_empr_form_sul
			,   t11.value  as vl_empr_form_sudeste
			,   t12.value  as vl_empr_form_centro_oeste
			,   t13.value  as vl_empr_form_norte
			,   t14.value  as vl_empr_form_nordeste
			,   t15.value  as vl_bvsp
			,	t16.value  as vl_usd
			,	t17.value  as vl_base_1833
			,	t18.value  as vl_divida_publica_4469
			,	t19.value  as vl_selic_4390
			,	t20.value  as vl_selic_4189
			,	t21.value  as vl_massa_salarial_22078
			,	t22.value  as vl_export_22708
			,	t23.value  as vl_import_22709
            ,   t24.value  as vl_export_net
	from          ds_dt_str                 t0
		left join ds_arrec                  t1  on t0.dt_str = t1.dt_str
		left join ds_pib                    t2  on t1.dt_str = t2.dt_str
		left join ds_energia_total          t3  on t1.dt_str = t3.dt_str
		left join ds_energia_comercial      t4  on t1.dt_str = t4.dt_str
		left join ds_energia_residencial    t5  on t1.dt_str = t5.dt_str
		left join ds_energia_industrial     t6  on t1.dt_str = t6.dt_str
		left join ds_energia_outros         t7  on t1.dt_str = t7.dt_str
		left join ds_empregados             t8  on t1.dt_str = t8.dt_str
		left join ds_ipca                   t9  on t1.dt_str = t9.dt_str
		left join ds_empregos_formais       t1X on t1.dt_str = t1X.dt_str and t1X.code = 0
		left join ds_empregos_formais       t10 on t1.dt_str = t10.dt_str and t10.code = 13564
		left join ds_empregos_formais       t11 on t1.dt_str = t11.dt_str and t11.code = 13901
		left join ds_empregos_formais       t12 on t1.dt_str = t12.dt_str and t12.code = 21991
		left join ds_empregos_formais       t13 on t1.dt_str = t13.dt_str and t13.code = 28152
		left join ds_empregos_formais       t14 on t1.dt_str = t14.dt_str and t14.code = 13941
		left join ds_bvsp                   t15 on t1.dt_str = t15.dt_str_mes
		left join ds_usd                    t16 on t1.dt_str = t16.dt_str
		left join ds_base_1833              t17 on t1.dt_str = t17.dt_str
		left join ds_divida_publica_4469    t18 on t1.dt_str = t18.dt_str
		left join ds_selic_4390             t19 on t1.dt_str = t19.dt_str
		left join ds_selic_4189             t20 on t1.dt_str = t20.dt_str
		left join ds_massa_salarial_22078   t21 on t1.dt_str = t21.dt_str
		left join ds_export_22708           t22 on t1.dt_str = t22.dt_str
		left join ds_import_22709           t23 on t1.dt_str = t23.dt_str
		left join ds_export_net             t24 on t1.dt_str = t24.dt_str
	order by t0.dt_str;")
	
	ds$seq  <- seq(1: nrow(ds))
	ds_temp <- ds
	ds_temp <- sqldf::sqldf("
	select		t1.*
			,	(select sum(t2.vl_ipca) from ds_temp t2 where t2.seq <= t1.seq) as vl_icpa_acum
	from 		ds_temp t1;
	")
	ds_temp$vl_icpa_acum_norm <- (ds_temp$vl_icpa_acum - min(ds_temp$vl_icpa_acum) ) / 
		( max(ds_temp$vl_icpa_acum) - min(ds_temp$vl_icpa_acum) ) * 100
	
	ds <- ds_temp		

	#View(ds)
	ds$dt_ano_str   <- substring(ds$dt_str, 1, 4)
	
	my_data_work                         <- as.list("")
	my_data_work$ds_work                 <- ds[!is.na(ds$vl_usd), ]
	my_data_work$ds_arrec                <- ds_arrec
	my_data_work$ds_pib                  <- ds_pib
	my_data_work$ds_energia_total        <- ds_energia_total
	my_data_work$ds_empregados			 <- ds_empregados
	my_data_work$ds_ipca                 <- ds_ipca
	my_data_work$ds_empregos_formais     <- ds_empregos_formais
	my_data_work$ds_bvsp                 <- ds_bvsp
	my_data_work$ds_usd                  <- ds_usd
	my_data_work$ds_base_1833            <- ds_base_1833
	my_data_work$ds_divida_publica_4469  <- ds_divida_publica_4469
	my_data_work$ds_selic_4390			 <- ds_selic_4390
	my_data_work$ds_selic_4189			 <- ds_selic_4189
	my_data_work$ds_massa_salarial_22078 <- ds_massa_salarial_22078
	my_data_work$ds_export_22708		 <- ds_export_22708
	my_data_work$ds_import_22709		 <- ds_import_22709
	my_data_work$ds_export_net		     <- ds_export_net

	my_data_work$ds_cor_pearson          <- get_data_cor_pearson(ds)
	
	return(my_data_work)
	
}

#=========================================================================================
get_data_cor_pearson <- function(p_ds) {
	#p_ds    <- my_data_work$ds_work
	#str(p_ds)
	ds_work <- p_ds
	ds_cor  <- as.data.frame(cbind(ds_work$vl_arrec, ds_work$vl_energ_total, ds_work$vl_pib,
								  ds_work$vl_bvsp, ds_work$vl_ipca, ds_work$vl_empr_form,
								  ds_work$vl_usd, ds_work$vl_base_1833, ds_work$vl_divida_publica_4469,
								  ds_work$vl_selic_4390, ds_work$vl_selic_4189,
								  ds_work$vl_massa_salarial_22078,
								  ds_work$vl_export_22708,
								  ds_work$vl_import_22709,
								  ds_work$vl_export_net))

	l_metricas <- c("   Arrec", "   Energ", "     PIB", "    BVPS", "    IPCA", "    Empr", "     USD"
					, "    Base", "  Dívida", "  SELIC-M", "  SELIC-A", "Massa", "Export", "Import", "Exp-Imp")
	names(ds_cor) <- l_metricas
	
	ds_cor_pearson <- as.data.frame(cor(x=ds_cor,  use="pairwise.complete.obs", method="pearson"))

	names(ds_cor_pearson)    <- l_metricas
	rownames(ds_cor_pearson) <- l_metricas

	l_years <- unique(ds_work$dt_ano_str, 1, 4)
	
	for (y in l_years) {
		#y <- "2000"
		ds_work_aux <- ds_work[ds_work$dt_ano_str == y, ]
		
		ds_cor_aux  <- as.data.frame(cbind(ds_work_aux$vl_arrec, ds_work_aux$vl_energ_total, ds_work_aux$vl_pib,
										   ds_work_aux$vl_bvsp, ds_work_aux$vl_ipca, ds_work_aux$vl_empr_form,
										   ds_work_aux$vl_usd, ds_work_aux$vl_base_1833, ds_work_aux$vl_divida_publica_4469,
										   ds_work_aux$vl_selic_4390, ds_work_aux$vl_selic_4189, ds_work_aux$vl_massa_salarial_22078,
										   ds_work_aux$vl_export_22708,ds_work_aux$vl_import_22709, ds_work_aux$vl_export_net))
		
		ds_cor_pearson_aux <- as.data.frame(round(cor(x=ds_cor_aux,  use="pairwise.complete.obs", method="pearson"),2))

		names(ds_cor_pearson_aux)    <- l_metricas
		rownames(ds_cor_pearson_aux) <- l_metricas
		#ds_cor_pearson_aux           <- ds_cor_pearson_aux[1, ]
		ds_cor_pearson_aux           <- ds_cor_pearson_aux
		rownames(ds_cor_pearson_aux) <- paste0(l_metricas, "-", y)
		
		ds_cor_pearson <- rbind(ds_cor_pearson, ds_cor_pearson_aux)
	}
	
	ds_cor_pearson <- ds_cor_pearson[!is.na(ds_cor_pearson$`   Arrec`), ]
	
	ds <- as.list("")
	ds$ds_cor_pearson      <- ds_cor_pearson
	ds$ds_cor_pearson_data <- ds_cor

	return(ds)
	
}


#=========================================================================================
get_data_labels <- function(p_ds) {
	
	#p_ds      <- temp$data
	#?openxlsx::read.xlsx()
	ds_labels <- openxlsx::read.xlsx(xlsxFile = "00_my_labels.xlsx", encoding="UTF-8")
	
	ds        <- sqldf::sqldf("
	select		t1.rel_imp
			,   t2.value as x_names
			,	t1.x_names as x_names_orig
	from 		p_ds      t1
	left join 	ds_labels t2 on t1.x_names = t2.key
	order by 	rel_imp desc;
	")
	#ds$x_names <- paste0(rownames(ds), ". ", ds$x_names)
		
	return(ds)

}

#=========================================================================================
get_data_ts <- function(p_ds="", p_value=c("vl_arrec")) {
	#p_ds    <- my_data_work$ds_work
	#p_value <- c("vl_arrec")
	
	#----------------------------------------------------------------------------------------
	my_ds_ts        <- data.frame(dt_str=p_ds$dt_str, value=round(p_ds[, p_value],2))
	names(my_ds_ts) <- c("dt_str", "value")
	my_ds_ts$dt_str <- as.character(my_ds_ts$dt_str)
	my_ds_ts$value  <- as.numeric(my_ds_ts$value) # Força a conversão para numeric.
	my_ds_ts        <- my_ds_ts[!is.na(my_ds_ts$value), ]
	
	#----------------------------------------------------------------------------------------
	# Transforma os dados para TS.
	#----------------------------------------------------------------------------------------
	l_min     <- min(my_ds_ts$dt_str)
	l_min_ano <- as.integer(substring(text = l_min, first = 1, last = 4))
	l_min_mes <- as.integer(substring(text = l_min, first = 6, last = 7))
	
	l_max     <- max(my_ds_ts$dt_str)
	l_max_ano <- as.integer(substring(text = l_max, first = 1, last = 4))
	l_max_mes <- as.integer(substring(text = l_max, first = 6, last = 7))
	
	my_ts <- ts(data = my_ds_ts$value
				, start     =c(l_min_ano, l_min_mes)
				, end       =c(l_max_ano, l_max_mes)
				, frequency = 12)
	
	# Define o período da série de dados que será utilizada como treinamento (até 2018).
	my_ts_train <- ts(data = my_ts, start = c(l_min_ano, l_min_mes), end = c(2018,12), frequency = 12)
	
	# Configura a quantidades de períodos para o forecast.
	my_h <- nrow(as.data.frame(my_ts)) - nrow(as.data.frame(my_ts_train)) + C_MONTHS_FORECAST
	
	ds             <- as.list("")
	ds$my_ds_ts    <- my_ds_ts
	ds$my_ts       <- my_ts
	ds$my_ts_train <- my_ts_train
	ds$my_h        <- my_h
	
	return(ds)
}
