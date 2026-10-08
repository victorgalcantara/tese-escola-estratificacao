# Title: Areas de ponderacao do Censo 2022 para o mapa (opcional)
# Author: Victor G Alcantara | victorgalcantara@usp.br
#
# Rode na sua maquina, depois de 01_preparar_dados.R:
#   Rscript R/03_areas_2022.R
#
# Entrada: microdados da amostra do Censo 2022 (acesso controlado), registro de
# Domicilios, um CSV por UF. Saida: dados/areas_ponderacao_2022.geojson, que traz
# SOMENTE estimativas agregadas por area de ponderacao (renda, populacao,
# densidade, escolas). Os microdados NAO sao copiados nem publicados, como exige o
# termo de confidencialidade; confira com o termo e com o Sedap/IBGE se a divulgacao
# de estimativas por AP esta coberta antes de publicar o site.
#
# Sem esse arquivo, o site funciona normalmente e a opcao "Censo 2022" nao aparece.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(sf); library(data.table)
})
source("R/00_parametros.R", encoding = "UTF-8")
sf_use_s2(FALSE)

## 1. Estimativas por AP (Domicilios) ------------------------------------------

arquivos <- list.files(PASTA_CENSO_2022, pattern = "^Domicilios_.*_controlado\\.csv$",
                       recursive = TRUE, full.names = TRUE)
stopifnot("Nenhum arquivo Domicilios_*_controlado.csv em PASTA_CENSO_2022" = length(arquivos) > 0)

# D0090 AP | D0111 peso | D0130 especie (01 = particular permanente ocupado)
# D0150 moradores | D0350 renda domiciliar (R$)
ler_uf <- function(f) {
  d <- fread(f, sep = ";", dec = ".", colClasses = list(character = c("D0090", "D0130")),
             select = c("D0090", "D0111", "D0130", "D0150", "D0350"))
  d[D0130 == "01"]
}
dom <- rbindlist(lapply(arquivos, ler_uf))
message("Domicilios lidos: ", format(nrow(dom), big.mark = "."))

dom[, `:=`(peso = as.numeric(D0111), mor = as.numeric(D0150), renda = as.numeric(D0350))]

ap_est <- dom[!is.na(D0090) & nzchar(D0090), .(
  n_dom_amostra = .N,
  populacao = sum(peso * mor, na.rm = TRUE),
  # renda domiciliar per capita da AP = renda total / moradores (razao de totais)
  renda_pc = sum(peso * renda, na.rm = TRUE) / sum(peso * mor * !is.na(renda), na.rm = TRUE)
), by = .(cod = D0090)] |> as_tibble()

ap_est <- ap_est %>%
  mutate(renda_sm = if_else(n_dom_amostra >= MIN_DOMICILIOS_AP_2022,
                            renda_pc / SALARIO_MINIMO_2022, NA_real_),
         populacao_k = populacao / 1000)
rm(dom); invisible(gc())
message("APs 2022 com estimativa: ", nrow(ap_est), " (", sum(!is.na(ap_est$renda_sm)), " com renda)")

## 2. Malha das APs 2022 -----------------------------------------------------------

geom <- if (!is.null(ARQUIVO_GEOM_AP_2022)) {
  st_read(ARQUIVO_GEOM_AP_2022, quiet = TRUE)
} else {
  tryCatch(geobr::read_weighting_area(year = 2022, simplified = TRUE, showProgress = FALSE),
           error = function(e) NULL)
}
if (is.null(geom)) {
  stop("Sem malha de APs 2022. Baixe a malha do IBGE e informe o caminho em ",
       "ARQUIVO_GEOM_AP_2022 (R/00_parametros.R).")
}
# Nome da coluna do codigo da AP varia conforme a fonte
col_cod <- intersect(c("code_weighting", "CD_APONDE", "COD_AP", "cod_ap", "CD_AP"), names(geom))[1]
stopifnot("Nao achei a coluna do codigo da AP na malha" = !is.na(col_cod))

geom <- geom %>%
  mutate(cod = as.character(.data[[col_cod]])) %>%
  select(cod) %>%
  st_transform(5880) %>%
  filter(!is.na(cod), !st_is_empty(geometry)) %>%
  st_make_valid()
if (any(st_geometry_type(geom) == "GEOMETRYCOLLECTION")) {
  geom <- st_collection_extract(geom, "POLYGON", warn = FALSE)
}
geom <- geom %>% mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = TOLERANCIA_SIMPLIFICACAO_AP) %>%
  st_transform(4326)

## 3. Escolas por AP 2022 (juncao espacial) ----------------------------------------

escolas <- readRDS(file.path(caminho_dados_site, "escolas.rds")) %>%
  filter(!is.na(lat), !is.na(lon), tipologia %in% ORDEM_TIPOLOGIA_C4)
pts <- st_as_sf(escolas, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
esc_ap <- st_join(pts["tipologia"], geom["cod"]) %>% st_drop_geometry() %>%
  filter(!is.na(cod)) %>%
  group_by(cod) %>%
  summarise(esc = n(),
            topo = 100 * mean(tipologia %in% c("Privada NSE IV", "Privada NSE V",
                                               "Estadual NSE III", "Federal")),
            .groups = "drop")

## 4. GeoJSON ------------------------------------------------------------------------

mun <- readRDS(file.path(caminho_dados_site, "areas_ponderacao.rds")) %>%
  distinct(code_muni, name_muni, abbrev_state)

ap22 <- geom %>%
  left_join(ap_est, by = "cod") %>%
  left_join(esc_ap, by = "cod") %>%
  mutate(code_muni = substr(cod, 1, 7)) %>%
  left_join(mun, by = "code_muni") %>%
  transmute(
    cod, mun = name_muni, uf = abbrev_state,
    esc = coalesce(esc, 0L),
    renda = round(renda_sm, 3),
    densidade = round(populacao / area_km2, 1),
    populacao = round(populacao_k, 2),
    topo = if_else(esc > 0, round(topo, 1), NA_real_)
  )

saida <- file.path(caminho_dados_site, "areas_ponderacao_2022.geojson")
if (file.exists(saida)) file.remove(saida)
st_write(ap22, saida, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=3", "RFC7946=YES"))
message("Gerado: ", saida, " (", round(file.size(saida) / 1e6, 1), " MB; ",
        nrow(ap22), " APs; ", sum(!is.na(ap22$renda)), " com renda)")
