# Script R - file: integracao_clima_simplificado

# Explicação do Script de Análise Climática Agrícola do Ceará

Este script em R realiza uma análise climática completa voltada para três culturas específicas (Mandioca, Feijão Caupi e Caju) nas diferentes regiões do Ceará. Vou explicar cada componente, as análises realizadas e os resultados gerados.

## Bibliotecas Utilizadas

```r

library(httr)        # Para requisições HTTP
library(jsonlite)    # Para processamento de JSON
library(dplyr)       # Para manipulação de dados
library(tidyr)       # Para crossing() e outras manipulações
library(ggplot2)     # Para visualizações
library(lubridate)   # Para manipulação de datas
library(gridExtra)   # Para organizar gráficos

```

Estas bibliotecas fornecem as ferramentas necessárias para:

- Fazer requisições à API climática (httr, jsonlite)
- Manipular e transformar os dados (dplyr, tidyr)
- Criar visualizações gráficas (ggplot2, gridExtra)
- Trabalhar com datas (lubridate)

## Funções de Obtenção de Dados

### 1. Obtenção de Dados Históricos

```r

obter_dados_climaticos <- function(latitude, longitude, inicio, fim, variaveis)

```

Esta função:

- Constrói uma URL para a API do Open-Meteo Archive
- Faz uma requisição HTTP GET para obter dados climáticos históricos
- Processa a resposta JSON e transforma em um dataframe R
- Retorna dados diários de temperatura máxima, mínima e precipitação

### 2. Obtenção de Previsões

```r

obter_previsao <- function(latitude, longitude, dias, variaveis)

```

Similar à função anterior, mas obtém previsões futuras em vez de dados históricos:

- Utiliza o endpoint de previsão da API Open-Meteo
- Permite especificar quantos dias de previsão deseja (padrão: 10 dias)
- Retorna os dados futuros em formato estruturado

## Estrutura de Dados

### Locais Analisados

```r

locais_ceara <- data.frame(
  nome = c("Fortaleza", "Aquiraz", "Sobral", "Quixadá", "Juazeiro do Norte"),
  latitude = c(-3.7319, -3.9006, -3.6889, -4.9676, -7.2131),
  longitude = c(-38.5267, -38.3911, -40.3494, -39.0157, -39.3153),
  regiao = c("Litoral", "Litoral", "Sertão", "Sertão", "Cariri")
)

```

Define cinco municípios do Ceará divididos em três regiões:

- **Litoral**: Fortaleza e Aquiraz
- **Sertão**: Sobral e Quixadá
- **Cariri**: Juazeiro do Norte

### Requisitos das Culturas

```r

requisitos_culturas <- data.frame(
  cultura = c("Mandioca", "Feijão Caupi", "Caju"),
  temp_min_ideal = c(20, 18, 22),
  temp_max_ideal = c(30, 34, 36),
  precip_min_mensal = c(50, 40, 30),
  precip_max_mensal = c(200, 150, 100)
)

```

Define os parâmetros climáticos ideais para cada cultura:

- **Mandioca**: 20-30°C, 50-200mm precipitação mensal
- **Feijão Caupi**: 18-34°C, 40-150mm precipitação mensal
- **Caju**: 22-36°C, 30-100mm precipitação mensal

## Fluxo de Coleta de Dados

O script coleta dados para cada município em um loop:

```r

for (i in 1:nrow(locais_ceara)) {
  # Coleta dados históricos
  # Coleta previsões
  # Atribui região e município aos dados
}

```

Após a coleta, os dados são combinados em dois dataframes principais:

- `dados_combinados`: Contém o histórico de um ano para todos os municípios
- `previsao_combinada`: Contém previsões para os próximos 10 dias

## Análises Realizadas

### 1. Agregação Mensal por Região

```r

dados_mensais <- dados_combinados %>%
  mutate(mes = month(time, label = TRUE)) %>%
  group_by(regiao, mes) %>%
  summarise(
    temp_max_media = mean(temperature_2m_max, na.rm = TRUE),
    temp_min_media = mean(temperature_2m_min, na.rm = TRUE),
    precipitacao_media = mean(precipitation_sum, na.rm = TRUE)
  )

```

- Transforma as datas em meses rotulados (Jan, Fev, etc.)
- Agrupa os dados por região e mês
- Calcula médias de temperatura e precipitação para cada grupo

### 2. Cálculo de Adequabilidade Climática

```r

adequabilidade <- dados_mensais %>%
  crossing(requisitos_culturas) %>%
  mutate(
    # Cálculos de adequação
    indice_adequabilidade = (adequacao_temp * 0.5) + (adequacao_precip * 0.5)
  )

```

Esta é uma análise central do script que:

- Combina os dados climáticos com os requisitos de cada cultura
- Calcula o quão adequada é cada região/mês para cada cultura
- Gera um índice de 0 a 1, onde valores mais altos indicam melhor adequação
- Pondera igualmente temperatura (50%) e precipitação (50%)

### 3. Identificação de Períodos Ideais de Plantio

```r

melhores_periodos <- adequabilidade %>%
  filter(indice_adequabilidade >= 0.6) %>%
  group_by(regiao, cultura) %>%
  summarise(
    meses_recomendados = paste(as.character(mes), collapse = ", "),
    indice_medio = mean(indice_adequabilidade, na.rm = TRUE)
  )

```

Esta análise:

- Filtra apenas períodos com adequabilidade boa (>= 0.6)
- Agrupa por região e cultura
- Lista os meses recomendados para cada combinação
- Calcula a adequabilidade média durante esses meses

### 4. Agregação Estatística por Região

```r

estatisticas_regiao <- dados_combinados %>%
  group_by(regiao) %>%
  summarise(
    temp_max_media = mean(temperature_2m_max, na.rm = TRUE),
    temp_min_media = mean(temperature_2m_min, na.rm = TRUE),
    amplitude_termica = mean(temperature_2m_max - temperature_2m_min, na.rm = TRUE),
    precipitacao_anual = sum(precipitation_sum, na.rm = TRUE) / n_distinct(local),
    dias_chuvosos = sum(precipitation_sum > 1, na.rm = TRUE) / n_distinct(local)
  )

```

Gera estatísticas anuais por região:

- Temperaturas médias máxima e mínima
- Amplitude térmica média
- Precipitação anual total
- Número médio de dias chuvosos (>1mm)

## Visualizações Geradas

### 1. Gráfico de Temperatura Mensal

```r

p1 <- ggplot(dados_mensais, aes(x = mes, y = temp_max_media, color = regiao, group = regiao)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ...

```

- **Tipo**: Gráfico de linhas
- **Objetivo**: Visualizar a variação de temperatura ao longo do ano
- **Análise possível**: Identificar padrões sazonais e diferenças entre regiões

### 2. Gráfico de Precipitação Mensal

```r

p2 <- ggplot(dados_mensais, aes(x = mes, y = precipitacao_media, fill = regiao)) +
  geom_bar(stat = "identity", position = "dodge") +
  ...

```

- **Tipo**: Gráfico de barras agrupadas
- **Objetivo**: Mostrar distribuição mensal de chuvas por região
- **Análise possível**: Identificar períodos chuvosos e de estiagem

### 3. Mapa de Calor de Adequabilidade

```r

p3 <- ggplot(adequabilidade_media, aes(x = cultura, y = regiao, fill = indice_medio)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.0f%%", indice_medio * 100)), color = "black") +
  ...

```

- **Tipo**: Mapa de calor (heatmap)
- **Objetivo**: Visualizar quais culturas são mais adequadas para cada região
- **Análise possível**: Identificar combinações ótimas de cultura-região

## Saídas Geradas

### 1. Relatório Textual (TXT)

O script gera um relatório em formato texto com:

- Estatísticas climáticas por região
- Índices de adequabilidade por cultura e região
- Melhores períodos para plantio
- Recomendações específicas para cada cultura
- Previsão para os próximos dias

### 2. Dashboard HTML

Gera um arquivo HTML interativo com:

- Tabelas de estatísticas e adequabilidade
- Gráficos incorporados
- Tabela de melhores períodos de plantio
- Previsão para os próximos dias

### 3. Dados CSV

Exporta vários datasets em formato CSV:

- `dados_mensais.csv`: Estatísticas mensais por região
- `adequabilidade_detalhada.csv`: Índices de adequabilidade detalhados
- `periodos_recomendados.csv`: Melhores períodos para plantio
- `previsao_proximos_dias.csv`: Previsões meteorológicas

### 4. Visualizações

Salva os três gráficos principais em formato PNG:

- `temperatura_mensal.png`
- `precipitacao_mensal.png`
- `adequabilidade_culturas.png`

## Fluxo de Análise e Decisão

O script segue uma lógica que permite responder as seguintes perguntas:

1. **Quais são as condições climáticas típicas de cada região do Ceará?**
    - Respondido pelas estatísticas regionais e gráficos de temperatura e precipitação
2. **Quais culturas são mais adequadas para cada região?**
    - Respondido pelo índice de adequabilidade e mapa de calor
3. **Quando é o melhor período para plantar cada cultura em cada região?**
    - Respondido pela análise de períodos ideais de plantio
4. **Como será o clima nos próximos dias?**
    - Respondido pela previsão de 10 dias obtida da API

## Aplicações Práticas

Este script oferece informações valiosas para:

- **Agricultores**: Planejamento de plantio baseado em dados climáticos
- **Extensionistas rurais**: Recomendações técnicas fundamentadas
- **Gestores públicos**: Políticas de apoio à agricultura regional
- **Pesquisadores**: Base de dados para estudos agroclimáticos

A saída em múltiplos formatos (texto, HTML, CSV) facilita o compartilhamento e uso das informações por diferentes públicos.