# Script R - Análise de Dados

# Explicação do Script de Análise de Dados Agrícolas FarmTech

Este script R foi desenvolvido para analisar dados agrícolas exportados do sistema FarmTech. Vou explicar cada componente, desde as bibliotecas utilizadas até as visualizações geradas.

## Bibliotecas Utilizadas

```r

library(tidyverse)  # Para manipulação de dados e visualização
library(readr)      # Para leitura de arquivos CSV
library(ggplot2)    # Para gráficos
library(gridExtra)  # Para organizar múltiplos gráficos
library(scales)     # Para formatação de escalas nos gráficos

```

- **tidyverse**: Um conjunto de pacotes que inclui dplyr para manipulação de dados e outras ferramentas para análise de dados.
- **readr**: Fornece funções para leitura eficiente de arquivos de texto.
- **ggplot2**: Sistema para criação de gráficos baseado na "gramática dos gráficos".
- **gridExtra**: Permite organizar múltiplos gráficos em uma única visualização.
- **scales**: Fornece ferramentas para formatação de escalas em gráficos.

## Carregamento dos Dados

### Função para Encontrar Arquivos

```r

encontrar_arquivo <- function(sufixo) {
  arquivos <- list.files(pattern = paste0(".*", sufixo, "$"))
  if (length(arquivos) == 0) {
    stop(paste("Nenhum arquivo encontrado com o sufixo:", sufixo))
  }
  arquivos_info <- file.info(arquivos)
  arquivos_ordenados <- rownames(arquivos_info[order(arquivos_info$mtime, decreasing = TRUE), ])
  return(arquivos_ordenados[1])
}

```

Esta função:

1. Busca todos os arquivos no diretório atual que terminam com o sufixo especificado
2. Verifica se algum arquivo foi encontrado
3. Ordena os arquivos pela data de modificação (mais recente primeiro)
4. Retorna o arquivo mais recente

Isso permite que o script sempre use os dados mais atualizados sem precisar de nomes fixos.

### Leitura dos Arquivos

```r

# Encontrar arquivos mais recentes
arquivo_culturas <- encontrar_arquivo("_culturas.csv")
arquivo_campos <- encontrar_arquivo("_campos.csv")
arquivo_insumos <- encontrar_arquivo("_insumos.csv")

# Leitura dos arquivos CSV
culturas <- read_csv(arquivo_culturas)
campos <- read_csv(arquivo_campos)
insumos <- read_csv(arquivo_insumos)

```

Aqui, a função encontra os arquivos mais recentes de cada tipo e os carrega em dataframes R, que são estruturas de dados tabular.

## Análises Estatísticas

### Estatísticas de Culturas

```r

cat("\nEstatísticas de Culturas:\n")
cat("Número de culturas cadastradas:", nrow(culturas), "\n")
cat("Média de ciclo mínimo (dias):", mean(culturas$ciclo_minimo), "\n")
# ...outras estatísticas

```

- **nrow(culturas)**: Conta o número de linhas no dataframe, representando a quantidade de culturas cadastradas.
- **mean(culturas$ciclo_minimo)**: Calcula a média do ciclo mínimo de todas as culturas.

Estas estatísticas fornecem uma visão geral das características das culturas, como ciclos de produção e necessidades ambientais (temperatura, precipitação).

### Estatísticas de Campos

```r

cat("\nEstatísticas de Campos:\n")
cat("Número de campos cadastrados:", nrow(campos), "\n")
cat("Média de área (m²):", mean(campos$area_m2), "\n")
cat("Desvio padrão de área (m²):", sd(campos$area_m2), "\n")
# ...outras estatísticas

```

- **mean(campos$area_m2)**: Calcula a área média dos campos em metros quadrados.
- **sd(campos$area_hectare)**: Calcula o desvio padrão da área, indicando a variabilidade do tamanho dos campos.
- **sum(campos$area_hectare)**: Soma total de área cultivada em hectares.

### Distribuição de Tipos de Geometria

```r

geometria_count <- table(campos$tipo_geometria)

```

Esta análise conta quantos campos existem de cada tipo de geometria (retangular, triangular, circular, trapezoidal), permitindo entender a distribuição de formatos dos campos cadastrados.

### Estatísticas de Insumos

```r

cat("Média de fertilizante por campo (kg):", mean(insumos$fertilizante_total_kg), "\n")
cat("Média de fertilizante por hectare (kg/ha):", mean(insumos$fertilizante_total_kg / insumos$area_hectare), "\n")
# ...outras estatísticas

```

Estas estatísticas analisam o uso de insumos, calculando:

- Médias e desvios padrão de uso de fertilizantes
- Médias e desvios padrão de volume de irrigação
- Eficiência de uso por hectare (quantidades normalizadas pela área)

## Agregações de Dados

### Análise por Cultura

```r

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

```

Esta análise:

1. Agrupa os dados por tipo de cultura
2. Para cada cultura, calcula:
    - Número de campos plantados com essa cultura
    - Área total plantada
    - Média de fertilizante utilizado
    - Média de volume de irrigação
    - Médias normalizadas por hectare

Isto permite comparar diferentes culturas em termos de área cultivada e intensidade de uso de insumos.

### Análise por Região

```r
R
campos_insumos <- merge(campos, insumos, by=c("nome_produtor", "cultura_plantada", "area_hectare"))
regiao_summary <- campos_insumos %>%
  group_by(regiao) %>%
  summarise(
    num_campos = n(),
    area_total = sum(area_hectare),
    media_fertilizante = mean(fertilizante_total_kg),
    media_irrigacao = mean(irrigacao_volume_total)
  )

```

Nesta análise:

1. Primeiro, mesclamos as tabelas de campos e insumos usando chaves comuns
2. Depois, agrupamos por região
3. Para cada região, calculamos:
    - Número de campos
    - Área total
    - Médias de uso de fertilizantes e irrigação

Isto permite comparar práticas agrícolas em diferentes regiões geográficas.

## Visualizações

### Gráfico 1: Área Cultivada por Cultura

```r

p1 <- ggplot(insumos, aes(x=cultura_plantada, y=area_hectare, fill=cultura_plantada)) +
  geom_bar(stat="identity") +
  labs(title="Área Cultivada por Cultura",
       x="Cultura",
       y="Área (hectares)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")

```

Este gráfico de barras mostra a área total cultivada para cada tipo de cultura. Cada barra representa uma cultura, com a altura indicando o total de hectares plantados. É útil para visualizar rapidamente qual cultura ocupa mais área.

### Gráfico 2: Relação entre Área e Uso de Fertilizantes

```r

p2 <- ggplot(insumos, aes(x=area_hectare, y=fertilizante_total_kg, color=cultura_plantada)) +
  geom_point(size=3, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="darkgrey") +
  labs(title="Relação entre Área e Uso de Fertilizantes",
       x="Área (hectares)",
       y="Fertilizante (kg)",
       color="Cultura") +
  theme_minimal()

```

Este gráfico de dispersão:

- Mostra cada campo como um ponto, com área no eixo X e quantidade de fertilizante no eixo Y
- Usa cores diferentes para cada cultura
- Inclui uma linha de tendência que mostra a relação geral entre área e uso de fertilizantes
- Ajuda a identificar se o uso de fertilizantes escala linearmente com o tamanho do campo

### Gráfico 3: Volume de Irrigação por Cultura

```r

p3 <- ggplot(insumos, aes(x=cultura_plantada, y=irrigacao_volume_total, fill=cultura_plantada)) +
  geom_bar(stat="identity") +
  labs(title="Volume de Irrigação por Cultura",
       x="Cultura",
       y="Volume de Irrigação (L)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")

```

Similar ao primeiro gráfico, este mostra o volume total de irrigação utilizado para cada cultura. Permite identificar rapidamente quais culturas demandam mais água.

### Gráfico 4: Relação entre Área e Volume de Irrigação

```r

p4 <- ggplot(insumos, aes(x=area_hectare, y=irrigacao_volume_total, color=cultura_plantada)) +
  geom_point(size=3, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="darkgrey") +
  labs(title="Relação entre Área e Volume de Irrigação",
       x="Área (hectares)",
       y="Volume de Irrigação (L)",
       color="Cultura") +
  theme_minimal()

```

Este gráfico de dispersão é similar ao segundo, mas analisa o uso de água em vez de fertilizantes. Mostra como o volume de irrigação se relaciona com o tamanho do campo para diferentes culturas.

## Exportação dos Resultados

```r

# Extrair timestamp do nome do arquivo
timestamp <- str_extract(arquivo_culturas, "\\d{8}_\\d{6}")

# Criar um arquivo de resumo
nome_relatorio <- paste0("analise_farmtech_", timestamp, ".txt")
sink(nome_relatorio)
# ... conteúdo do relatório ...
sink()

# Salvar os gráficos com o mesmo timestamp
ggsave(paste0("area_por_cultura_", timestamp, ".png"), p1, width=8, height=6)
# ... outros gráficos ...

```

Esta parte do script:

1. Extrai o timestamp do nome do arquivo original para manter a consistência
2. Cria um arquivo de texto com um resumo completo das análises
3. Salva cada gráfico como uma imagem PNG separada, usando o mesmo timestamp
4. Informa ao usuário quais arquivos foram gerados