#Estimando uma NAIRU(Non-Accelerating Inflation Rate of Unemployment) 
#para o Brasil a partir de dados da PNADc Mensal e dados do IPCA
#Dados coletados do SIDRA IBGE, tabelas 1737 e 6381
#-----------------------------------------------------------------------------
#Importando Bibliotecas
library(sidrar)
library(tidyverse)
library(splitTools)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(seasonal)
#Coleta dos dados
ipca_raw <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")
#Limpeza, Tratamento de dados, IPCA de 2010 até o dias atuais
dplyr::glimpse(ipca_raw)
ipca <- ipca_raw |>
      dplyr::select("data"= "Mês (Código)",
                    "ipca"= "Valor") |>
      dplyr::mutate(data = lubridate::ym(data)) |>
      dplyr::filter(data >= "2010-01-01")
      dplyr::as_tibble()
#Analise Exploratória
ggplot2::ggplot(ipca)+ggplot2::aes(x=data,y=ipca)+ggplot2::geom_line()

summary(ipca)
      
ggplot2::ggplot(ipca)+ggplot2::aes(y=ipca)+ggplot2::geom_boxplot()
ggplot2::ggplot(ipca)+ggplot2::aes(x=ipca)+ggplot2::geom_histogram()

#Taxa de Desemprego
#/t/6381/n1/all/v/4099/p/all/d/v4099%201 API do Sidra IBGE
desemprego_raw <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")


desemprego <- desemprego_raw |> 
            dplyr::select('data' = "Trimestre Móvel (Código)", 'desemprego' = "Valor") |>
            dplyr::mutate(data = lubridate::ym(data)) |>
            dplyr::as_tibble()
df_dados <- ipca |> inner_join(desemprego, by = "data")

# Separar amostras
# Particionar os dados em treino e teste
set.seed(123) # Para reprodutibilidade

# Criar partições de treino e teste
amostras <- splitTools::partition(
  y = df_dados$desemprego,
  p = c(treino = 0.8, teste = 0.2),
  type = "basic"
)
dados_treino <- df_dados[amostras$treino, ]
dados_teste <- df_dados[amostras$teste, ]
# Converter dados de treino para tsibble
y_treino <- as_tsibble(dados_treino, index = data) |>
  fill_gaps()|>#Preencher valores ausentes por interpolação
  mutate(desemprego = zoo::na.approx(desemprego))

# Ajustar o modelo ARIMA aos dados de treino
set.seed(1984)# Para reprodutibilidade

modelo <- fabletools::model(
  .data = y_treino, 
  arima = fable::ARIMA(desemprego ~ PDQ(P = 0, D = 0, Q = 0))
)
print(modelo)
#verificar o inicio da serie
start_year <- year(min(y_treino$data))
start_period <- c(start_year, month(min(y_treino$data)))
# Preparar a série temporal para ajuste sazonal com seas
serie_temporal <- ts(y_treino$desemprego, start = start_period, frequency = 12)
#estrutura da serie temporal
str(serie_temporal)
# Verificar se há NAs ou valores inválidos
print(sum(is.na(serie_temporal)))
# Preencher NAs se necessário
if (sum(is.na(serie_temporal)) > 0) {
  serie_temporal <- na.approx(serie_temporal)
}

# Ajustar o modelo sazonal
modelo_sazonal <- seas(serie_temporal, x11 = "", 
                       outlier = "AO")

# Visualizar o resultado do ajuste sazonal
summary(modelo_sazonal)


#-----------------------------------------------------------------------------
#Visualização dos dados IPCA + Desemprego
df_dados |> 
  ggplot2::ggplot()+
  ggplot2::aes(x=data)+
  ggplot2::geom_line(aes(y=desocupacao,color = "Taxa de Desocupação"))+
  ggplot2::geom_line(aes(y=ipca,color = "Inflação"))+
  ggplot2::scale_color_manual(values = c("#800000", "#0000FF"))

modelo_phillips <- lm(ipca ~ desocupacao, data = df_dados)
summary(modelo_phillips)
