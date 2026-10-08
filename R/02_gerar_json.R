# Title: JSON das escolas para o navegador (ficha, busca, comparacao)
# Author: Victor G Alcantara | victorgalcantara@usp.br

# Roda antes do render (pre-render em _quarto.yml), para que o arquivo exista
# quando o Quarto copia os recursos do site. Se dados/ ainda nao foi preparado,
# funcoes_site.R roda R/01_preparar_dados.R primeiro.

source("R/funcoes_site.R", encoding = "UTF-8")

escolas <- ler_dados("escolas") %>%
  mutate(id = sprintf("%.0f", id))

gerar_json_escolas(escolas, ORDEM_LEGENDA_MAPA)
message("dados/escolas_mapa.json gerado (", nrow(escolas), " escolas).")
