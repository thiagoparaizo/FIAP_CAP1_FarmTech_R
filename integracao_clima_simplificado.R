# Script para análise climática agrícola do Ceará
# Foco: Mandioca, Feijão Caupi e Caju

# install.packages(c("httr", "jsonlite", "dplyr", "ggplot2", "lubridate", "gridExtra"))

# Carregando bibliotecas
library(httr)        # Para requisições HTTP
library(jsonlite)    # Para processamento de JSON
library(dplyr)       # Para manipulação de dados
library(tidyr)       # Para crossing() e outras manipulações de dados
library(ggplot2)     # Para visualizações
library(lubridate)   # Para manipulação de datas
library(gridExtra)   # Para organizar gráficos

# Função para obter dados meteorológicos históricos
obter_dados_climaticos <- function(latitude, longitude, inicio, fim, 
                                   variaveis = c("temperature_2m_max", "temperature_2m_min", 
                                                 "precipitation_sum", "rain_sum")) {
  base_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  variaveis_str <- paste(variaveis, collapse = ",")
  
  url <- paste0(
    base_url, 
    "?latitude=", latitude,
    "&longitude=", longitude,
    "&start_date=", inicio,
    "&end_date=", fim,
    "&daily=", variaveis_str,
    "&timezone=auto"
  )
  
  resposta <- GET(url)
  
  if (status_code(resposta) == 200) {
    dados_json <- fromJSON(content(resposta, "text", encoding = "UTF-8"))
    dados_diarios <- as.data.frame(dados_json$daily)
    dados_diarios$time <- as.Date(dados_diarios$time)
    
    return(dados_diarios)
  } else {
    stop(paste("Erro na requisição à API:", status_code(resposta)))
  }
}

# Função para obter previsão do tempo
obter_previsao <- function(latitude, longitude, dias = 10,
                           variaveis = c("temperature_2m_max", "temperature_2m_min", 
                                         "precipitation_sum", "rain_sum")) {
  base_url <- "https://api.open-meteo.com/v1/forecast"
  
  variaveis_str <- paste(variaveis, collapse = ",")
  
  url <- paste0(
    base_url, 
    "?latitude=", latitude,
    "&longitude=", longitude,
    "&daily=", variaveis_str,
    "&forecast_days=", dias,
    "&timezone=auto"
  )
  
  resposta <- GET(url)
  
  if (status_code(resposta) == 200) {
    dados_json <- fromJSON(content(resposta, "text", encoding = "UTF-8"))
    dados_diarios <- as.data.frame(dados_json$daily)
    dados_diarios$time <- as.Date(dados_diarios$time)
    
    return(dados_diarios)
  } else {
    stop(paste("Erro na requisição à API:", status_code(resposta)))
  }
}

# Definir municípios do Ceará
locais_ceara <- data.frame(
  nome = c("Fortaleza", "Aquiraz", "Sobral", "Quixadá", "Juazeiro do Norte"),
  latitude = c(-3.7319, -3.9006, -3.6889, -4.9676, -7.2131),
  longitude = c(-38.5267, -38.3911, -40.3494, -39.0157, -39.3153),
  regiao = c("Litoral", "Litoral", "Sertão", "Sertão", "Cariri")
)

# Obter dados históricos para os últimos 12 meses
data_atual <- Sys.Date()
data_inicio <- data_atual - 365
data_fim <- data_atual

# Lista para armazenar dados coletados
dados_historicos <- list()
dados_previsao <- list()

# Coletar dados para cada local
for (i in 1:nrow(locais_ceara)) {
  local <- locais_ceara[i, ]
  
  cat("Obtendo dados para", local$nome, "...\n")
  
  tryCatch({
    dados <- obter_dados_climaticos(
      local$latitude, 
      local$longitude, 
      format(data_inicio, "%Y-%m-%d"), 
      format(data_fim, "%Y-%m-%d")
    )
    
    dados$local <- local$nome
    dados$regiao <- local$regiao
    
    dados_historicos[[i]] <- dados
    
    previsao <- obter_previsao(local$latitude, local$longitude)
    previsao$local <- local$nome
    previsao$regiao <- local$regiao
    
    dados_previsao[[i]] <- previsao
    
  }, error = function(e) {
    cat("Erro ao obter dados para", local$nome, ":", e$message, "\n")
  })
  
  Sys.sleep(1)
}

# Combinar dados
dados_combinados <- bind_rows(dados_historicos)
previsao_combinada <- bind_rows(dados_previsao)

# Temperatura e precipitação mensal por região
dados_mensais <- dados_combinados %>%
  mutate(mes = month(time, label = TRUE)) %>%
  group_by(regiao, mes) %>%
  summarise(
    temp_max_media = mean(temperature_2m_max, na.rm = TRUE),
    temp_min_media = mean(temperature_2m_min, na.rm = TRUE),
    precipitacao_media = mean(precipitation_sum, na.rm = TRUE)
  )

# Requisitos para as culturas específicas (valores hipotéticos ajustados para Ceará)
requisitos_culturas <- data.frame(
  cultura = c("Mandioca", "Feijão Caupi", "Caju"),
  temp_min_ideal = c(20, 18, 22),
  temp_max_ideal = c(30, 34, 36),
  precip_min_mensal = c(50, 40, 30),
  precip_max_mensal = c(200, 150, 100)
)

# Calcular índice de adequabilidade
adequabilidade <- dados_mensais %>%
  crossing(requisitos_culturas) %>%
  mutate(
    # Adequação de temperatura (0-1)
    adequacao_temp = pmin(
      pmax(0, 1 - abs(temp_max_media - temp_max_ideal) / 10),
      pmax(0, 1 - abs(temp_min_media - temp_min_ideal) / 10)
    ),
    # Adequação de precipitação (0-1)
    adequacao_precip = pmin(
      pmax(0, (precipitacao_media - precip_min_mensal) / precip_min_mensal),
      pmax(0, (precip_max_mensal - precipitacao_media) / precip_max_mensal)
    ),
    # Índice geral (média ponderada)
    indice_adequabilidade = (adequacao_temp * 0.5) + (adequacao_precip * 0.5)
  )

# Calcular balanço hídrico simplificado
balanco_hidrico <- dados_combinados %>%
  mutate(mes = month(time, label = TRUE)) %>%
  group_by(regiao, local, mes) %>%
  summarise(
    precipitacao_total = sum(precipitation_sum, na.rm = TRUE)
  )

# Estatísticas por região
estatisticas_regiao <- dados_combinados %>%
  group_by(regiao) %>%
  summarise(
    temp_max_media = mean(temperature_2m_max, na.rm = TRUE),
    temp_min_media = mean(temperature_2m_min, na.rm = TRUE),
    amplitude_termica = mean(temperature_2m_max - temperature_2m_min, na.rm = TRUE),
    precipitacao_anual = sum(precipitation_sum, na.rm = TRUE) / n_distinct(local),
    dias_chuvosos = sum(precipitation_sum > 1, na.rm = TRUE) / n_distinct(local)
  )

# Previsão para os próximos dias
previsao_regiao <- previsao_combinada %>%
  group_by(regiao, time) %>%
  summarise(
    temp_max = mean(temperature_2m_max, na.rm = TRUE),
    temp_min = mean(temperature_2m_min, na.rm = TRUE),
    precip = mean(precipitation_sum, na.rm = TRUE)
  )

# ----- VISUALIZAÇÕES SIMPLIFICADAS -----

# 1. Temperatura média mensal por região
p1 <- ggplot(dados_mensais, aes(x = mes, y = temp_max_media, color = regiao, group = regiao)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Temperatura Máxima Média Mensal por Região",
       x = "Mês",
       y = "Temperatura (°C)",
       color = "Região") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Precipitação média mensal por região
p2 <- ggplot(dados_mensais, aes(x = mes, y = precipitacao_media, fill = regiao)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Precipitação Média Mensal por Região",
       x = "Mês",
       y = "Precipitação (mm)",
       fill = "Região") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Adequabilidade simplificada por cultura e região
adequabilidade_media <- adequabilidade %>%
  group_by(regiao, cultura) %>%
  summarise(indice_medio = mean(indice_adequabilidade, na.rm = TRUE)) %>%
  ungroup()

p3 <- ggplot(adequabilidade_media, aes(x = cultura, y = regiao, fill = indice_medio)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.0f%%", indice_medio * 100)), color = "black") +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  labs(title = "Índice de Adequabilidade por Cultura e Região",
       x = "Cultura",
       y = "Região",
       fill = "Adequabilidade") +
  theme_minimal()

# Criar diretório para resultados
dir.create("resultados_ceara", showWarnings = FALSE)

# Gráficos
grid.arrange(p1, p2, p3, ncol = 1)

# Salvar gráficos
ggsave("resultados_ceara/temperatura_mensal.png", p1, width = 8, height = 5)
ggsave("resultados_ceara/precipitacao_mensal.png", p2, width = 8, height = 5)
ggsave("resultados_ceara/adequabilidade_culturas.png", p3, width = 8, height = 5)

# Identificar melhores períodos de plantio
melhores_periodos <- adequabilidade %>%
  filter(indice_adequabilidade >= 0.6) %>%
  group_by(regiao, cultura) %>%
  summarise(
    meses_recomendados = paste(as.character(mes), collapse = ", "),
    indice_medio = mean(indice_adequabilidade, na.rm = TRUE)
  ) %>%
  arrange(regiao, cultura)

# Relatório simplificado
relatorio_file <- "resultados_ceara/relatorio_agricola_ceara.txt"
sink(relatorio_file)

cat("================================================================\n")
cat("           ANÁLISE CLIMÁTICA PARA CULTURAS DO CEARÁ             \n")
cat("================================================================\n\n")
cat("Data de geração:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n\n")

cat("ESTATÍSTICAS CLIMÁTICAS POR REGIÃO:\n")
for (r in unique(estatisticas_regiao$regiao)) {
  estat <- estatisticas_regiao %>% filter(regiao == r)
  cat(sprintf("\n* %s:\n", r))
  cat(sprintf("  - Temperatura média máxima: %.1f°C\n", estat$temp_max_media))
  cat(sprintf("  - Temperatura média mínima: %.1f°C\n", estat$temp_min_media))
  cat(sprintf("  - Precipitação anual média: %.0f mm\n", estat$precipitacao_anual))
  cat(sprintf("  - Dias chuvosos (média): %.0f dias\n", estat$dias_chuvosos))
}

cat("\n\nÍNDICE DE ADEQUABILIDADE POR CULTURA E REGIÃO:\n")
for (r in unique(adequabilidade_media$regiao)) {
  cat(sprintf("\n* %s:\n", r))
  for (c in unique(adequabilidade_media$cultura)) {
    adequabilidade_valor <- adequabilidade_media %>% 
      filter(regiao == r, cultura == c) %>% 
      pull(indice_medio)
    
    if (length(adequabilidade_valor) > 0) {
      cat(sprintf("  - %s: %.0f%%\n", c, adequabilidade_valor * 100))
    }
  }
}

cat("\n\nMELHORES PERÍODOS DE PLANTIO:\n")
if (nrow(melhores_periodos) > 0) {
  for (r in unique(melhores_periodos$regiao)) {
    cat(sprintf("\n* %s:\n", r))
    for (c in unique(melhores_periodos$cultura)) {
      periodos <- melhores_periodos %>% filter(regiao == r, cultura == c)
      if (nrow(periodos) > 0) {
        cat(sprintf("  - %s: %s (adequabilidade média: %.0f%%)\n", 
                    c, periodos$meses_recomendados, periodos$indice_medio * 100))
      } else {
        cat(sprintf("  - %s: Sem períodos adequados identificados\n", c))
      }
    }
  }
} else {
  cat("Não foram identificados períodos adequados com base nos critérios definidos.\n")
}

cat("\n\nRECOMENDAÇÕES ESPECÍFICAS:\n")

cat("\n* Mandioca:\n")
cat("  - É uma cultura adaptada a climas quentes e tolera bem períodos de seca.\n")
cat("  - No litoral, o plantio pode ser realizado no início das chuvas (janeiro a março).\n")
cat("  - No sertão, aproveitar períodos com maior disponibilidade hídrica.\n")
cat("  - Ciclo longo (10-14 meses), permite flexibilidade no período de colheita.\n")

cat("\n* Feijão Caupi:\n")
cat("  - Cultura de ciclo curto (60-80 dias), adequada para regiões semiáridas.\n")
cat("  - Plantio recomendado no início da estação chuvosa (janeiro a março).\n")
cat("  - No Sertão, considerar plantio em áreas com acesso à irrigação suplementar.\n")
cat("  - Apresenta boa tolerância ao calor e adapta-se bem às condições do Ceará.\n")

cat("\n* Caju:\n")
cat("  - Cultura perene adaptada às condições do semiárido nordestino.\n")
cat("  - Plantio de mudas preferencialmente no início da estação chuvosa.\n")
cat("  - No Litoral e Cariri, apresenta melhores condições de desenvolvimento.\n")
cat("  - Produção de frutos concentrada entre agosto e dezembro.\n")

cat("\n\nPREVISÃO PARA OS PRÓXIMOS 10 DIAS:\n")
for (r in unique(previsao_regiao$regiao)) {
  cat(sprintf("\n* %s:\n", r))
  previsao <- previsao_regiao %>% 
    filter(regiao == r) %>%
    arrange(time)
  
  for (i in 1:min(5, nrow(previsao))) {
    cat(sprintf("  - %s: %.1f°C a %.1f°C, precipitação: %.1f mm\n", 
                format(previsao$time[i], "%d/%m"), 
                previsao$temp_min[i],
                previsao$temp_max[i],
                previsao$precip[i]))
  }
}

cat("\n================================================================\n")
cat("                        FIM DO RELATÓRIO                         \n")
cat("================================================================\n")

sink()

# Criar um HTML simples com os resultados
html_file <- "resultados_ceara/dashboard_ceara.html"
sink(html_file)

cat('<!DOCTYPE html>
<html lang="pt-br">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Análise Climática Agrícola do Ceará</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background-color: #f5f5f5; }
        .container { max-width: 900px; margin: 0 auto; }
        .header { text-align: center; margin-bottom: 30px; color: #2c5e1a; }
        .card { background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
        .card-title { margin-top: 0; color: #2c5e1a; border-bottom: 1px solid #eee; padding-bottom: 10px; }
        .table { width: 100%; border-collapse: collapse; margin: 15px 0; }
        .table th, .table td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }
        .table th { background-color: #f2f2f2; }
        .footer { text-align: center; margin-top: 30px; color: #666; font-size: 12px; }
        img { max-width: 100%; height: auto; border-radius: 4px; margin-top: 10px; }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Análise Climática para Culturas do Ceará</h1>
            <p>Mandioca, Feijão Caupi e Caju</p>
            <p>Data de geração: ')
cat(format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
cat('</p>
        </div>
        
        <div class="card">
            <h2 class="card-title">Estatísticas Climáticas</h2>
            <table class="table">
                <tr>
                    <th>Região</th>
                    <th>Temp. Máx. Média</th>
                    <th>Temp. Mín. Média</th>
                    <th>Precipitação Anual</th>
                    <th>Dias Chuvosos</th>
                </tr>')

# Estatísticas regionais
for (i in 1:nrow(estatisticas_regiao)) {
  estat <- estatisticas_regiao[i,]
  cat(sprintf('
                <tr>
                    <td>%s</td>
                    <td>%.1f°C</td>
                    <td>%.1f°C</td>
                    <td>%.0f mm</td>
                    <td>%.0f dias</td>
                </tr>', 
              estat$regiao, 
              estat$temp_max_media, 
              estat$temp_min_media,
              estat$precipitacao_anual,
              estat$dias_chuvosos))
}

cat('
            </table>
        </div>
        
        <div class="card">
            <h2 class="card-title">Adequabilidade das Culturas</h2>
            <table class="table">
                <tr>
                    <th>Região</th>
                    <th>Mandioca</th>
                    <th>Feijão Caupi</th>
                    <th>Caju</th>
                </tr>')

# Tabela de adequabilidade
for (r in unique(adequabilidade_media$regiao)) {
  valores <- adequabilidade_media %>% 
    filter(regiao == r) %>%
    arrange(cultura)
  
  if (nrow(valores) == 3) {
    cat(sprintf('
                <tr>
                    <td>%s</td>
                    <td>%.0f%%</td>
                    <td>%.0f%%</td>
                    <td>%.0f%%</td>
                </tr>', 
                r, 
                valores$indice_medio[1] * 100,
                valores$indice_medio[2] * 100,
                valores$indice_medio[3] * 100))
  }
}

cat('
            </table>
        </div>
        
        <div class="card">
            <h2 class="card-title">Gráficos</h2>
            <div style="margin-bottom: 20px;">
                <h3>Temperatura Máxima Média Mensal</h3>
                <img src="temperatura_mensal.png" alt="Temperatura por região">
            </div>
            <div style="margin-bottom: 20px;">
                <h3>Precipitação Média Mensal</h3>
                <img src="precipitacao_mensal.png" alt="Precipitação por região">
            </div>
            <div>
                <h3>Índice de Adequabilidade por Cultura</h3>
                <img src="adequabilidade_culturas.png" alt="Adequabilidade das culturas">
            </div>
        </div>
        
        <div class="card">
            <h2 class="card-title">Melhores Períodos de Plantio</h2>
            <table class="table">
                <tr>
                    <th>Região</th>
                    <th>Cultura</th>
                    <th>Meses Recomendados</th>
                    <th>Adequabilidade Média</th>
                </tr>')

# Periodos de plantio
if (nrow(melhores_periodos) > 0) {
  for (i in 1:nrow(melhores_periodos)) {
    mp <- melhores_periodos[i,]
    cat(sprintf('
                <tr>
                    <td>%s</td>
                    <td>%s</td>
                    <td>%s</td>
                    <td>%.0f%%</td>
                </tr>', 
                mp$regiao, 
                mp$cultura,
                mp$meses_recomendados,
                mp$indice_medio * 100))
  }
} else {
  cat('
                <tr>
                    <td colspan="4">Não foram identificados períodos adequados com os critérios definidos.</td>
                </tr>')
}

cat('
            </table>
        </div>
        
        <div class="card">
            <h2 class="card-title">Previsão para os Próximos Dias</h2>
            <table class="table">
                <tr>
                    <th>Região</th>
                    <th>Data</th>
                    <th>Temperatura Mín.</th>
                    <th>Temperatura Máx.</th>
                    <th>Precipitação</th>
                </tr>')

# Previsões dos próximos 5 dias
previsao_5dias <- previsao_regiao %>%
  filter(time <= Sys.Date() + 5) %>%
  arrange(regiao, time)

for (i in 1:nrow(previsao_5dias)) {
  prev <- previsao_5dias[i,]
  cat(sprintf('
                <tr>
                    <td>%s</td>
                    <td>%s</td>
                    <td>%.1f°C</td>
                    <td>%.1f°C</td>
                    <td>%.1f mm</td>
                </tr>', 
              prev$regiao, 
              format(prev$time, "%d/%m/%Y"),
              prev$temp_min,
              prev$temp_max,
              prev$precip))
}

cat('
            </table>
        </div>
        
        <div class="footer">
            <p>Análise gerada automaticamente com base em dados climáticos da API Open-Meteo.</p>
            <p>Desenvolvido para o monitoramento de culturas adaptadas ao Ceará.</p>
        </div>
    </div>
</body>
</html>')

sink()

# Exportar os dados para CSV
write.csv(dados_mensais, "resultados_ceara/dados_mensais.csv", row.names = FALSE)
write.csv(adequabilidade, "resultados_ceara/adequabilidade_detalhada.csv", row.names = FALSE)
write.csv(melhores_periodos, "resultados_ceara/periodos_recomendados.csv", row.names = FALSE)
write.csv(previsao_regiao, "resultados_ceara/previsao_proximos_dias.csv", row.names = FALSE)

# Mensagem final
cat("\n=== ANÁLISE CLIMÁTICA PARA CULTURAS DO CEARÁ CONCLUÍDA ===\n\n")
cat("Todos os resultados foram salvos no diretório 'resultados_ceara'.\n")
cat("Arquivos gerados:\n")
cat("1. Dashboard HTML: dashboard_ceara.html\n")
cat("2. Relatório de texto: relatorio_agricola_ceara.txt\n")
cat("3. Visualizações e arquivos CSV com dados processados\n\n")
cat("Obrigado por utilizar o sistema de análise agroclimática!\n")