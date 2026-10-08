# Title: Parametros do site da tipologia de escolas (C4)
# Author: Victor G Alcantara | victorgalcantara@usp.br

# Concentra caminhos, versoes e decisoes de publicacao usados pelo preparo dos
# dados e pelas paginas do site. Alterar aqui, nunca dentro das paginas.

# 1. Paths ---------------------------------------------------------------------

# Raiz do site: pasta que contem _quarto.yml. Funciona no RStudio, no Rscript
# e no render do Quarto (que executa a partir da raiz do site).
localizar_site <- function() {
  candidatos <- c(
    Sys.getenv("SITE_TIPOLOGIA_DIR", unset = ""),
    tryCatch(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)),
             error = function(e) ""),
    getwd(),
    dirname(getwd())
  )
  candidatos <- candidatos[nzchar(candidatos)]
  for (pasta in candidatos) {
    if (file.exists(file.path(pasta, "_quarto.yml"))) {
      return(normalizePath(pasta, winslash = "/"))
    }
  }
  stop("Nao encontrei a raiz do site (pasta com _quarto.yml).")
}

caminho_site <- localizar_site()

# Raiz do repositorio da tese: o site fica em 2_code/site_tipologia_c4.
# A variavel de ambiente TESE_DIR permite apontar outra copia da tese.
caminho_tese <- Sys.getenv("TESE_DIR", unset = "")
if (!nzchar(caminho_tese)) {
  caminho_tese <- normalizePath(file.path(caminho_site, "../.."), winslash = "/")
}

caminho_painel <- file.path(caminho_tese, "1_data", "_2_tre", "painel_data")
caminho_sedap <- file.path(
  caminho_tese, "1_data", "_2_tre", "INEP-SEDAP",
  paste0("3", intToUtf8(170), "Extracao_Sedap_2026-09-08"), "3.3 Tabelas"
)
caminho_tabelas_tese <- file.path(caminho_tese, "3_outp", "1_tables")

caminho_dados_site <- file.path(caminho_site, "dados")
caminho_cache <- file.path(caminho_dados_site, "_cache")

# 2. Versions ------------------------------------------------------------------

# A pagina recusa uma base de outra versao da tipologia (mesma regra do 4.1).
VERSAO_TIPOLOGIA <- "C4-texto-2026-09-30"
EXTRACAO_SEDAP <- "3\u00aa extra\u00e7\u00e3o do Sedap/Inep (08/09/2026)"
SM_2015 <- 788

# 3. Publication decisions -----------------------------------------------------

# Resultados por escola so aparecem para escolas com ao menos este numero de
# concluintes na coorte; abaixo disso a celula fica suprimida (mesmo limiar
# das figuras 8 e 9 do C4).
MIN_CONCLUINTES_RESULTADOS <- 10

# Resultados dos egressos por escola (Enem e educacao superior). Conferir com o
# Sedap/Inep e com o orientador antes de publicar o site com TRUE.
PUBLICAR_RESULTADOS_ESCOLA <- TRUE

# Bolsa Familia (CadUnico): a autorizacao para publicar ainda nao foi
# confirmada (Agents.md, secao 10). Fica fora do site enquanto FALSE.
PUBLICAR_PBF <- FALSE

# Simplificacao das areas de ponderacao para a web (metros, EPSG 5880).
TOLERANCIA_SIMPLIFICACAO_AP <- 200

# 4. Typology order and palette ------------------------------------------------

# Mesma ordem e cores de 2_code/R/paleta_tipologia.R
ORDEM_TIPOLOGIA_C4 <- c(
  "Federal",
  "Privada NSE V", "Privada NSE IV", "Privada NSE III", "Privada NSE I-II",
  "Esc. Vinc. Dif.",
  "Estadual NSE III", "Estadual NSE II", "Estadual NSE I",
  "Est. Loc. Dif."
)

# Tipos do mapa: os dez da tipologia mais Municipal e Sem informacoes
ORDEM_LEGENDA_MAPA <- c(ORDEM_TIPOLOGIA_C4, "Municipal", "Sem informações")

PALETA_TIPOLOGIA <- c(
  "Federal"          = "#D4A017",
  "Privada NSE V"    = "#67000D",
  "Privada NSE IV"   = "#A50F15",
  "Privada NSE III"  = "#D7301F",
  "Privada NSE I-II" = "#F0603F",
  "Esc. Vinc. Dif."  = "#4D4D4D",
  "Estadual NSE III" = "#08306B",
  "Estadual NSE II"  = "#1F63A8",
  "Estadual NSE I"   = "#4A90CC",
  "Est. Loc. Dif."   = "#2E8B57",
  "Municipal"        = "#9E9E9E",
  "Sem informações" = "#C8C8C8"
)

# Censo 2022 (opcional) ---------------------------------------------------------
# Microdados da amostra (acesso controlado): so agregados por AP vao para o site.
PASTA_CENSO_2022 <- "E:/01 - data/IBGE/CENSO/2022/microdados_censo_amostra_2022_csv_20260902_055819"
# Malha de areas de ponderacao 2022 (shapefile/gpkg do IBGE). Se NULL, o script
# tenta geobr::read_weighting_area(year = 2022).
ARQUIVO_GEOM_AP_2022 <- NULL
SALARIO_MINIMO_2022 <- 1212          # valor vigente em 1/8/2022 (data de referencia)
MIN_DOMICILIOS_AP_2022 <- 30         # APs com menos domicilios na amostra ficam sem valor
