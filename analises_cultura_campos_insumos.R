# Aplicação de análise estatística para dados do FarmTech
# Desenvolvido para análise de culturas, campos e insumos agrícolas

# Carregando as bibliotecas necessárias
library(tidyverse)  # Para manipulação de dados e visualização
library(readr)      # Para leitura de arquivos CSV
library(ggplot2)    # Para gráficos
library(gridExtra)  # Para organizar múltiplos gráficos
library(scales)     # Para formatação de escalas nos gráficos

# Definindo o diretório de trabalho (ajuste conforme necessário)
# setwd("caminho/para/seu/diretorio")

# Função para encontrar o arquivo mais recente com determinado sufixo
encontrar_arquivo <- function(sufixo) {
  arquivos <- list.files(pattern = paste0(".*", sufixo, "$"))
  if (length(arquivos) == 0) {
    stop(paste("Nenhum arquivo encontrado com o sufixo:", sufixo))
  }
  # Ordenar por data de modificação (mais recente primeiro)
  arquivos_info <- file.info(arquivos)
  arquivos_ordenados <- rownames(arquivos_info[order(arquivos_info$mtime, decreasing = TRUE), ])
  return(arquivos_ordenados[1])  # Retorna o arquivo mais recente
}

# Encontrar arquivos mais recentes
arquivo_culturas <- encontrar_arquivo("_culturas.csv")
arquivo_campos <- encontrar_arquivo("_campos.csv")
arquivo_insumos <- encontrar_arquivo("_insumos.csv")

# Informar os arquivos encontrados
cat("Arquivos encontrados para análise:\n")
cat("Culturas:", arquivo_culturas, "\n")
cat("Campos:", arquivo_campos, "\n")
cat("Insumos:", arquivo_insumos, "\n\n")

# Leitura dos arquivos CSV 
culturas <- read_csv(arquivo_culturas)
campos <- read_csv(arquivo_campos)
insumos <- read_csv(arquivo_insumos)

# Exibir estrutura dos dados
cat("\n=== ESTRUTURA DOS DADOS ===\n")
cat("\nDados de Culturas:\n")
str(culturas)
cat("\nDados de Campos:\n")
str(campos)
cat("\nDados de Insumos:\n")
str(insumos)

# Estatísticas descritivas básicas
cat("\n\n=== ESTATÍSTICAS DESCRITIVAS ===\n")

# Estatísticas de Culturas
cat("\nEstatísticas de Culturas:\n")
cat("Número de culturas cadastradas:", nrow(culturas), "\n")
cat("Média de ciclo mínimo (dias):", mean(culturas$ciclo_minimo), "\n")
cat("Média de ciclo máximo (dias):", mean(culturas$ciclo_maximo), "\n")
cat("Média de temperatura mínima (°C):", mean(culturas$temperatura_min), "\n")
cat("Média de temperatura máxima (°C):", mean(culturas$temperatura_max), "\n")
cat("Média de precipitação mínima (mm):", mean(culturas$precipitacao_min), "\n")
cat("Média de precipitação máxima (mm):", mean(culturas$precipitacao_max), "\n")

# Estatísticas de Campos
cat("\nEstatísticas de Campos:\n")
cat("Número de campos cadastrados:", nrow(campos), "\n")
cat("Média de área (m²):", mean(campos$area_m2), "\n")
cat("Desvio padrão de área (m²):", sd(campos$area_m2), "\n")
cat("Média de área (hectares):", mean(campos$area_hectare), "\n")
cat("Desvio padrão de área (hectares):", sd(campos$area_hectare), "\n")
cat("Área total cultivada (hectares):", sum(campos$area_hectare), "\n")

# Distribuição de tipos de geometria
geometria_count <- table(campos$tipo_geometria)
cat("\nDistribuição de tipos de geometria:\n")
print(geometria_count)

# Estatísticas de Insumos
cat("\nEstatísticas de Insumos:\n")
cat("Média de fertilizante por campo (kg):", mean(insumos$fertilizante_total_kg), "\n")
cat("Desvio padrão de fertilizante (kg):", sd(insumos$fertilizante_total_kg), "\n")
cat("Média de fertilizante por hectare (kg/ha):", mean(insumos$fertilizante_total_kg / insumos$area_hectare), "\n")
cat("Média de volume de irrigação (L):", mean(insumos$irrigacao_volume_total), "\n")
cat("Desvio padrão de volume de irrigação (L):", sd(insumos$irrigacao_volume_total), "\n")
cat("Média de volume de irrigação por hectare (L/ha):", mean(insumos$irrigacao_volume_total / insumos$area_hectare), "\n")

# Análise por cultura
cat("\nAnálise por cultura:\n")
cultura_summary <- insumos %>%
  group_by(cultura_plantada) %>%
  summarise(
    num_campos = n(),
    area_total = sum(area_hectare),
    media_fertilizante = mean(fertilizante_total_kg),
    media_irrigacao = mean(irrigacao_volume_total),
    media_fert_por_ha = mean(fertilizante_total_kg / area_hectare),
    media_irrig_por_ha = mean(irrigacao_volume_total / area_hectare)
  )
print(cultura_summary)

# Análise por região
cat("\nAnálise por região:\n")
# Juntar dados de campos e insumos para análise por região
campos_insumos <- merge(campos, insumos, by=c("nome_produtor", "cultura_plantada", "area_hectare"))
regiao_summary <- campos_insumos %>%
  group_by(regiao) %>%
  summarise(
    num_campos = n(),
    area_total = sum(area_hectare),
    media_fertilizante = mean(fertilizante_total_kg),
    media_irrigacao = mean(irrigacao_volume_total)
  )
print(regiao_summary)

# Criar visualizações
cat("\n\n=== GERANDO VISUALIZAÇÕES ===\n")

# 1. Gráfico de barras para áreas por cultura
p1 <- ggplot(insumos, aes(x=cultura_plantada, y=area_hectare, fill=cultura_plantada)) +
  geom_bar(stat="identity") +
  labs(title="Área Cultivada por Cultura",
       x="Cultura",
       y="Área (hectares)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")

# 2. Gráfico de dispersão relacionando área e uso de fertilizantes
p2 <- ggplot(insumos, aes(x=area_hectare, y=fertilizante_total_kg, color=cultura_plantada)) +
  geom_point(size=3, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="darkgrey") +
  labs(title="Relação entre Área e Uso de Fertilizantes",
       x="Área (hectares)",
       y="Fertilizante (kg)",
       color="Cultura") +
  theme_minimal()

# 3. Gráfico de barras para irrigação por cultura
p3 <- ggplot(insumos, aes(x=cultura_plantada, y=irrigacao_volume_total, fill=cultura_plantada)) +
  geom_bar(stat="identity") +
  labs(title="Volume de Irrigação por Cultura",
       x="Cultura",
       y="Volume de Irrigação (L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")

# 4. Gráfico de dispersão relacionando área e volume de irrigação
p4 <- ggplot(insumos, aes(x=area_hectare, y=irrigacao_volume_total, color=cultura_plantada)) +
  geom_point(size=3, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="darkgrey") +
  labs(title="Relação entre Área e Volume de Irrigação",
       x="Área (hectares)",
       y="Volume de Irrigação (L)",
       color="Cultura") +
  theme_minimal()

# Exibir os gráficos
grid.arrange(p1, p2, p3, p4, ncol=2)

# Extrair timestamp do nome do arquivo para o relatório
timestamp <- str_extract(arquivo_culturas, "\\d{8}_\\d{6}")
if (is.na(timestamp)) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
}

# Exportar resultados para um relatório
cat("\nGerando relatório de análise... \n")

# Criar um arquivo de resumo
nome_relatorio <- paste0("analise_farmtech_", timestamp, ".txt")
sink(nome_relatorio)
cat("=== RELATÓRIO DE ANÁLISE FARMTECH ===\n")
cat("Data de geração:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n\n")
cat("Arquivos analisados:\n")
cat("- ", arquivo_culturas, "\n")
cat("- ", arquivo_campos, "\n")
cat("- ", arquivo_insumos, "\n\n")

cat("RESUMO GERAL\n")
cat("Número de culturas cadastradas:", nrow(culturas), "\n")
cat("Número de campos cadastrados:", nrow(campos), "\n")
cat("Área total cultivada:", sum(campos$area_hectare), "hectares\n\n")

cat("ANÁLISE POR CULTURA\n")
print(cultura_summary)
cat("\n")

cat("ANÁLISE POR REGIÃO\n")
print(regiao_summary)
cat("\n")

cat("EFICIÊNCIA DE INSUMOS\n")
cat("Média geral de fertilizante por hectare:", mean(insumos$fertilizante_total_kg / insumos$area_hectare), "kg/ha\n")
cat("Média geral de irrigação por hectare:", mean(insumos$irrigacao_volume_total / insumos$area_hectare), "L/ha\n")

sink()

# Salvar os gráficos com o mesmo timestamp
ggsave(paste0("area_por_cultura_", timestamp, ".png"), p1, width=8, height=6)
ggsave(paste0("area_vs_fertilizantes_", timestamp, ".png"), p2, width=8, height=6)
ggsave(paste0("irrigacao_por_cultura_", timestamp, ".png"), p3, width=8, height=6)
ggsave(paste0("area_vs_irrigacao_", timestamp, ".png"), p4, width=8, height=6)

cat("\nAnálise concluída! Arquivos gerados:\n")
cat("- ", nome_relatorio, " (relatório com estatísticas)\n")
cat("- area_por_cultura_", timestamp, ".png (gráfico)\n", sep="")
cat("- area_vs_fertilizantes_", timestamp, ".png (gráfico)\n", sep="")
cat("- irrigacao_por_cultura_", timestamp, ".png (gráfico)\n", sep="")
cat("- area_vs_irrigacao_", timestamp, ".png (gráfico)\n", sep="")