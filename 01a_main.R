##########################################################################################
# PONTIFÍCIA UNIVERSIDADE CATÓLICA DE MINAS GERAIS - NÚCLEO DE EDUCAÇÃO A DISTÂNCIA
# Pós-graduação Lato Sensu em Ciência de Dados e Big Data
# Aluno: Lucas Rodrigues Amaral (amaral.lr@gmail.com)
# TCC: TÉCNICAS DE MACHINE LEARNING APLICADAS NA PREVISÃO DA ARRECADAÇÃO FEDERAL 
# Data: 15/04/2021.
##########################################################################################

#=========================================================================================
# Forecasting: Principles and Practice
# Rob J Hyndman and George Athanasopoulos
# Monash University, Australia
# 11.3 Neural network models
# https://otexts.com/fpp2/nnetar.html
#=========================================================================================
rm(list=ls())
C_WD <- "C:\\Users\\01278575677\\Documents\\3. Cursos e pós graduações\\3.4. PUC Minas Ciência de Dados e Big Data\\13. TCC\\R"
setwd(C_WD)
getwd()

#========================================================================================
# Carrega as bibliotecas na memória.
#========================================================================================
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(DescTools)
library(data.table)

#========================================================================================
# Carrega as biblitecas complementares na memória.
#========================================================================================
source(file = "02_a_model_forecast_nnetar.R", encoding = "UTF-8")
source(file = "02_b_model_forecast_combination.R", encoding = "UTF-8")
source(file = "03_lib_data_source.R", encoding = "UTF-8")
source(file = "04_lib_plot.R", encoding = "UTF-8")
source(file = "05_lib_plot_explorer.R", encoding = "UTF-8")

#========================================================================================
# Prepara os dados.
#========================================================================================

# Coleta os dados pelas APIs (rbcb e ipeadatar).
my_data_work             <- get_data_work()

# Transforma os dados para TS.
my_data_work$my_ds_orig  <- my_data_work$ds_arrec # Série de dados da arrecadação federal.
my_ts_data               <- get_data_ts(p_ds=my_data_work$ds_work, p_value=c("vl_arrec"))
my_data_work$my_ds_ts    <- my_ts_data$my_ds_ts
my_data_work$my_ts       <- my_ts_data$my_ts
my_data_work$my_ts_train <- my_ts_data$my_ts_train
my_data_work$my_h        <- my_ts_data$my_h

#-----------------------------------------------------------------------------------------
# Informações sobre a série temporal.
#-----------------------------------------------------------------------------------------
TSstudio::ts_info(ts.obj = my_data_work$my_ts)
TSstudio::ts_plot(ts.obj = my_data_work$my_ts)
my_ts_decomposed <- TSstudio::ts_decompose(ts.obj = my_data_work$my_ts)
my_ts_decomposed

TSstudio::ts_heatmap(ts.obj = my_data_work$my_ts)
TSstudio::ts_lags(ts.obj = my_data_work$my_ts)

stats::acf(my_data_work$my_ts)
stats::pacf(my_data_work$my_ts)

#str(my_data_work$ds_work)
#View(my_data_work$ds_work)
#openxlsx::write.xlsx(file="ds_work.xlsx", x = my_data_work$ds_work, rowNames = TRUE)

#========================================================================================
# Gráficos das séries de dados.
#========================================================================================
my_plot_explorer <- get_plot_explorer(p_ds=my_data_work$ds_work)
my_plot_explorer$p01
my_plot_explorer$p02
my_plot_explorer$p03
my_plot_explorer$p04
my_plot_explorer$p05
my_plot_explorer$p06
my_plot_explorer$p07
my_plot_explorer$p08
my_plot_explorer$p09
my_plot_explorer$p10
my_plot_explorer$p11
my_plot_explorer$p12
my_plot_explorer$p13
my_plot_explorer$p14

#========================================================================================
# Correlações.
#========================================================================================
ds_cor_pearson             <- my_data_work$ds_cor_pearson$ds_cor_pearson
ds_cor_pearson_data        <- my_data_work$ds_cor_pearson$ds_cor_pearson_data
names(ds_cor_pearson_data) <- gsub(pattern = " ", "", names(ds_cor_pearson_data))
#f                          <- rownames(my_data_work$ds_cor_pearson$ds_cor_pearson) %like% c('%Arrec')
f                          <- rownames(my_data_work$ds_cor_pearson$ds_cor_pearson) %like% c('*Arrec*')

#View(ds_cor_pearson[f,])
#openxlsx::write.xlsx(file="ds_work_cor.xlsx", x = ds_cor_pearson[f,], rowNames = TRUE)

#----------------------------------------------------------------------------------------
# Gráficos de correlação.
#----------------------------------------------------------------------------------------
psych::pairs.panels(ds_cor_pearson_data, cex.labels=1.25, cex.cor=5.0, method="pearson")
GGally::ggcorr(ds_cor_pearson_data, nbreaks=8, method = c("pairwise.complete.obs", "pearson")
			   , palette='RdGy', label=TRUE, label_size=5, label_color='white', label_round = 2)

#========================================================================================
# Modelagem de Redes Neurais.
#========================================================================================
#?forecast::nnetar()
t1 <- Sys.time()
my_model_forecast_nnetar <- get_model_forecast_nnetar(p_ds=my_data_work)
t2 <- Sys.time()
print(t2 - t1)

# Exibe as informações da rede neural.
print(my_model_forecast_nnetar$my_forecat_nnetar)

# Exibe as informações de acuracidade do modelo.
forecast::accuracy(my_model_forecast_nnetar$my_forecat_nnetar_forecast, my_model_forecast_nnetar$my_ts)

# Gráfico com os valores reais vs. forecasted.
my_model_forecast_nnetar$p03

# Zoom do gráfico com os valores reais vs. forecasted.
my_model_forecast_nnetar$p04

#========================================================================================
# Combinação de modelos.
#========================================================================================
t1 <- Sys.time()
model_forecast_combination <- get_model_forecast_combination(p_model = my_model_forecast_nnetar)
t2 <- Sys.time()
print(t2 - t1)

# Exibe as informações de acuracidade (MAE) dos modelos.
model_forecast_combination$my_accurary_comb

#========================================================================================
# Gráficos para o TCC
#========================================================================================
my_tcc     <- as.list("01. TCC Ciência de Dados 2019 - Lucas Rodrigues Amaral")
my_tcc$t01 <- "Tabela 1. Dados coletados por API"
my_tcc$p01 <- my_plot_explorer$p12 + get_my_theme(p_legend_position = "top") # Gráfico 1. Arrecadação Federal vs. Consumo de Energia Elétrica (em amarelo) vs. PIB (em azul)
my_tcc$f01 <- "Figura 1. Arrecadação vs. Consumo de Energia, Base Monetária, IPCA, Dívida Pública, IBOVESPA, Empregos Formais, SELIC, PIB e Dólar."
my_tcc$f02 <- psych::pairs.panels(ds_cor_pearson_data, cex.labels=1.25, cex.cor=4.0, method="pearson") # Figura 2. Ilustração gráfica da matriz de correlação de Pearson entre variáveis macroeconômicas.
my_tcc$f03 <- GGally::ggcorr(ds_cor_pearson_data, nbreaks=8, method = c("pairwise.complete.obs", "pearson")
							 , palette='RdGy', label=TRUE, label_size=5, label_color='white', label_round = 2) # Figura 3. Matriz de Correlação de Pearson entre variáveis macroeconômicas.
my_tcc$t02 <- my_data_work$ds_cor_pearson$ds_cor_pearson[f,] # Tabela 2. Correlação de Pearson entre a Arrecadação e variáveis macroeconômicas ano a ano.
my_tcc$f04 <- "Figura 4. Ilustração do código fonte no ambiente R Studio."
my_tcc$f05 <- "Figura 5. Rede Neural obtida a partir da série temporal da arrecadação federal."
my_tcc$p02 <- my_model_forecast_nnetar$p01 # Gráfico 2. Previsão da Arrecadação Federal a partir da aplicação de redes neurais.
my_tcc$f06 <- "Figura 6. Resíduos da rede neural obtida a partir da série temporal da arrecadação federal."
my_tcc$p03 <- my_model_forecast_nnetar$p02 # Gráfico 3. Resíduos da previsão da arrecadação Federal a partir da aplicação de redes neurais.
my_tcc$p04 <- "Gráfico 4. Combinação de modelos ETS, NNAR e TABTS"
model_forecast_combination$p01
model_forecast_combination$p02
my_tcc$f07 <- "Figura 7. Resíduos da rede neural obtida a partir da combinação de modelos."
my_tcc$p05 <- my_model_forecast_nnetar$p03 # Gráfico 5. Arrecadação Federal e previsão até maio/2021.
my_tcc$p06 <- my_model_forecast_nnetar$p04 # Gráfico 6. Arrecadação Federal e previsão até maio/2021 (zoom do gráfico 7).
my_tcc$f08 <- "Figura 8. Erros mensais percentuais da rede neural."


my_tcc$t01
my_tcc$p01
my_tcc$f01
my_tcc$f02
my_tcc$f03
my_tcc$t02
my_tcc$f04
my_tcc$f05
my_tcc$p02
my_tcc$f06
my_tcc$p03
my_tcc$p04
model_forecast_combination$p01 + get_my_theme()
model_forecast_combination$p02
my_tcc$f07
my_tcc$p05
my_tcc$p06
my_tcc$f08


