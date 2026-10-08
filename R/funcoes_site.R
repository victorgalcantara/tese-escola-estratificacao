# Title: Funcoes de apoio das paginas do site da tipologia (C4)
# Author: Victor G Alcantara | victorgalcantara@usp.br

# Formatacao numerica em pt-BR, componentes HTML (cartoes, legendas de figura,
# etiquetas de tipo), tema dos graficos plotly e leitura dos dados do site.

# 1. Setup and packages --------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(htmltools)
  library(plotly)
  library(reactable)
  library(leaflet)
  library(crosstalk)
  library(sf)
})

source("R/00_parametros.R", encoding = "UTF-8")

ler_dados <- function(nome) readRDS(file.path("dados", paste0(nome, ".rds")))

# Se os dados do site ainda nao foram preparados, roda o preparo em um processo
# separado (o script limpa o ambiente com rm(list = ls()))
if (!file.exists(file.path("dados", "meta.rds"))) {
  message("dados/ ausente: executando R/01_preparar_dados.R (pode levar alguns minutos)...")
  rscript <- file.path(R.home("bin"), "Rscript")
  Sys.setenv(SITE_TIPOLOGIA_DIR = getwd())
  status <- system2(rscript, shQuote("R/01_preparar_dados.R"))
  if (!identical(status, 0L) || !file.exists(file.path("dados", "meta.rds"))) {
    stop("O preparo dos dados falhou. Rode R/01_preparar_dados.R no RStudio para ver o erro.")
  }
}

meta <- ler_dados("meta")

# 2. Number formatting ---------------------------------------------------------

num_br <- function(x, digitos = 0) {
  ifelse(is.na(x), "n.d.",
         formatC(round(x, digitos), format = "f", digits = digitos,
                 big.mark = ".", decimal.mark = ","))
}

pct_br <- function(x, digitos = 1) ifelse(is.na(x), "n.d.", paste0(num_br(x, digitos), "%"))

reais_br <- function(x) ifelse(is.na(x), "n.d.", paste0("R$ ", num_br(x, 0)))

# Formatos de coluna do reactable
fmt_int <- colFormat(separators = TRUE, locales = "pt-BR")
fmt_dec <- function(d = 1) colFormat(digits = d, separators = TRUE, locales = "pt-BR")
fmt_pct <- function(d = 1) colFormat(digits = d, suffix = "%", locales = "pt-BR")
fmt_brl <- colFormat(prefix = "R$ ", separators = TRUE, digits = 0, locales = "pt-BR")

# 3. HTML components -----------------------------------------------------------

# Etiqueta colorida do tipo escolar
etiqueta_tipo <- function(tipo) {
  cor <- PALETA_TIPOLOGIA[[tipo]]
  if (is.null(cor)) cor <- "#999999"
  span(class = "tipo-pill",
       style = paste0("--tipo:", cor),
       span(class = "tipo-dot"), tipo)
}

cartao_numero <- function(valor, rotulo, detalhe = NULL) {
  div(class = "stat-card",
      div(class = "stat-valor", valor),
      div(class = "stat-rotulo", rotulo),
      if (!is.null(detalhe)) div(class = "stat-detalhe", detalhe))
}

# Legenda de figura ou tabela: titulo, fonte e nota em linhas separadas
legenda <- function(titulo = NULL, fonte = NULL, nota = NULL) {
  div(class = "legenda-fig",
      if (!is.null(titulo)) p(class = "legenda-titulo", titulo),
      if (!is.null(fonte)) p(class = "legenda-fonte", strong("Fonte: "), fonte),
      if (!is.null(nota)) p(class = "legenda-nota", strong("Nota: "), nota))
}

# Definicao curta de cada tipo escolar (dica nos botoes e glossario). Os cortes
# do NSE vem da tabela meta$cortes_nse.
definicoes_tipo <- function() {
  q <- meta$cortes_nse$corte
  f <- function(x) num_br(x, 2)
  c(
    "Federal" = "Rede federal: institutos federais, colégios de aplicação e colégios militares.",
    "Privada NSE V" = paste0("Privadas com NSE acima de ", f(q[4]), " (quantil 99)."),
    "Privada NSE IV" = paste0("Privadas com NSE de ", f(q[3]), " a ", f(q[4]), " (quantis 95 a 99)."),
    "Privada NSE III" = paste0("Privadas com NSE de ", f(q[2]), " a ", f(q[3]), " (quantis 70 a 95)."),
    "Privada NSE I-II" = paste0("Privadas com NSE até ", f(q[2]), " (até o quantil 70)."),
    "Esc. Vinc. Dif." = "Estaduais com vínculo diferenciado (universidade, forças de segurança, rede técnica ou mais de 50% dos concluintes na educação profissional) e escolas do Sistema S.",
    "Estadual NSE III" = paste0("Estaduais com NSE acima de ", f(q[2]), " (quantil 70)."),
    "Estadual NSE II" = paste0("Estaduais com NSE de ", f(q[1]), " a ", f(q[2]), " (quantis 25 a 70)."),
    "Estadual NSE I" = paste0("Estaduais com NSE até ", f(q[1]), " (até o quantil 25)."),
    "Est. Loc. Dif." = "Estaduais em localização diferenciada: assentamento, terra indígena, quilombo ou comunidades tradicionais.",
    "Municipal" = "Escolas municipais, fora dos dez tipos.",
    "Sem informações" = "Sem informações para a classificação."
  )
}

# 4. School data for the browser ------------------------------------------------

# JSON colunar com os campos que a ficha, a busca e a comparacao usam no
# navegador (carregado por assets/explorador.js). Ids como texto, para
# casar com as chaves do crosstalk.
gerar_json_escolas <- function(escolas, ordem, destino = file.path("dados", "escolas_mapa.json")) {
  n_ou_null <- function(x, d) round(as.numeric(x), d)
  l <- list(
    id = escolas$id,
    nome = escolas$nome,
    mun = escolas$municipio,
    uf = escolas$uf,
    reg = escolas$regiao,
    tipo = match(as.character(escolas$tipologia), ordem) - 1L,
    rede = escolas$rede,
    dep = escolas$dep_adm,
    loc = escolas$localizacao,
    vinc = escolas$vinculo_dif,
    locdif = escolas$localizacao_dif,
    estrato = escolas$estrato_nse,
    concl = as.integer(escolas$concluintes),
    nse = n_ou_null(escolas$nse, 2),
    icg = as.integer(escolas$icg),
    afd = n_ou_null(escolas$afd, 1),
    iie = n_ou_null(escolas$iie, 1),
    lab = escolas$laboratorio,
    renda = n_ou_null(escolas$renda_entorno, 2),
    ppi = n_ou_null(escolas$p_ppi, 1),
    enem = n_ou_null(escolas$p_enem, 1),
    nota = n_ou_null(escolas$nota_enem, 1),
    es = n_ou_null(escolas$p_es, 1),
    espub = n_ou_null(escolas$p_es_pub, 1),
    estop = n_ou_null(escolas$p_es_top, 1),
    ruf = n_ou_null(escolas$p_ruf50, 1),
    supr = as.logical(escolas$suprimida),
    lat = n_ou_null(escolas$lat, 5),
    lon = n_ou_null(escolas$lon, 5)
  )
  writeLines(jsonlite::toJSON(l, na = "null", digits = NA, auto_unbox = FALSE), destino, useBytes = TRUE)
  invisible(destino)
}

# 5. Plotly theme --------------------------------------------------------------

FONTE_GRAFICOS <- "Inter, system-ui, sans-serif"

tema_plotly <- function(g, x_titulo = NULL, y_titulo = NULL, legenda_visivel = FALSE, ...) {
  base <- list(
    font = list(family = FONTE_GRAFICOS, size = 13, color = "#1f2933"),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    xaxis = list(title = list(text = if (is.null(x_titulo)) "" else x_titulo), gridcolor = "#e6e8eb", zeroline = FALSE),
    yaxis = list(title = list(text = if (is.null(y_titulo)) "" else y_titulo), gridcolor = "#e6e8eb", zeroline = FALSE),
    showlegend = legenda_visivel,
    legend = list(orientation = "h", x = 0, y = -0.18),
    hoverlabel = list(font = list(family = FONTE_GRAFICOS)),
    separators = ",.",
    margin = list(l = 10, r = 20, t = 10, b = 55)
  )
  # Ajustes da pagina sobrepoem o tema (inclusive dentro de xaxis e yaxis)
  extras <- list(...)
  for (eixo in c("xaxis", "yaxis")) {
    if (!is.null(extras[[eixo]]) && !is.null(extras[[eixo]]$title) && !is.list(extras[[eixo]]$title)) {
      extras[[eixo]]$title <- list(text = extras[[eixo]]$title)
    }
  }
  final <- modifyList(base, extras)
  # Altura do conteiner do widget igual a do grafico (evita corte)
  if (!is.null(final$height)) g$height <- final$height
  if (!is.null(final$yaxis$automargin)) final$yaxis$ticksuffix <- "  "
  g <- do.call(layout, c(list(p = g), final))
  g %>% config(displaylogo = FALSE, locale = "pt-BR",
               modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

# Fator ordenado pelo C4, de baixo (Est. Loc. Dif.) para cima (Federal), para
# graficos de barras horizontais
fator_tipos <- function(x, ordem = ORDEM_TIPOLOGIA_C4) factor(x, levels = rev(ordem))
