# Title: Preparo dos dados do site da tipologia de escolas (C4)
# Author: Victor G Alcantara | victorgalcantara@usp.br

# Le a tipologia local, as coordenadas, a coorte de 2015, o Censo 2010 por Area
# de Ponderacao e as tabelas liberadas do Sedap, e grava em dados/ os objetos
# compactos que as paginas do site usam. Rodar antes de quarto render.

# 1. Setup and packages --------------------------------------------------------

# Clean environment
rm(list = ls())
gc()

# Packages
library(pacman)
p_load(tidyverse, rio, sf)

# Parameters and paths (caminho_site, caminho_tese, flags de publicacao)
source_parametros <- function() {
  candidatos <- c("R/00_parametros.R", "00_parametros.R",
                  file.path(Sys.getenv("SITE_TIPOLOGIA_DIR"), "R/00_parametros.R"))
  tryCatch(candidatos <- c(file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
                                     "00_parametros.R"), candidatos),
           error = function(e) NULL)
  arquivo <- candidatos[file.exists(candidatos)][1]
  if (is.na(arquivo)) stop("Nao encontrei R/00_parametros.R")
  source(arquivo, encoding = "UTF-8")
}
source_parametros()

dir.create(caminho_dados_site, recursive = TRUE, showWarnings = FALSE)
dir.create(caminho_cache, recursive = TRUE, showWarnings = FALSE)

# s2 desligado: areas e simplificacao em EPSG 5880 (mesma convencao dos mapas)
sf_use_s2(FALSE)

# Helpers
corrigir_utf8 <- function(x) {
  # A base mistura UTF-8 e Latin-1 em alguns nomes de escola
  invalido <- !is.na(x) & !validUTF8(x)
  x[invalido] <- iconv(x[invalido], from = "latin1", to = "UTF-8")
  x
}

taxa <- function(numerador, denominador) {
  ifelse(denominador > 0, 100 * numerador / denominador, NA_real_)
}

# Nome em caixa mista, preservando siglas de escola, de redes e numerais romanos
SIGLAS_ESCOLA <- c("EE", "EEE", "EEM", "EEF", "EEEM", "EEEFM", "EEFM", "EEMTI", "EEEP", "EEEMTI",
                   "EEIEF", "EEFMT", "CE", "CEM", "CEF", "CED", "CEE", "CEJA", "CEEP", "CEPI",
                   "CETI", "CIEP", "CPM", "CPMG", "CMDP", "CEMI", "CMPM", "EMEF", "EMEFM", "EMEIEF",
                   "EMEB", "ETE", "ETEC", "EREM", "ESEDE", "IEE", "IE", "CEFET", "CAP", "CTU",
                   "SESI", "SENAI", "SENAC", "SESC", "SENAR", "SEST", "SENAT", "UNESP", "USP",
                   "UFRJ", "UERJ", "UFMG", "UFV", "UFU", "UFPE", "UFBA", "UNICAMP", "PM", "PMMG",
                   "EF", "EM", "EJA", "CIEJA", "CEU", "CEI", "CEIC", "CERE", "CEAN", "CEPMG",
                   "II", "III", "IV", "VI", "VII", "VIII", "IX", "XI", "XII", "XV", "XX")

nome_escola_legivel <- function(x) {
  x <- str_squish(toupper(x))
  vapply(str_split(x, " "), function(palavras) {
    sigla <- palavras %in% SIGLAS_ESCOLA | str_detect(palavras, "^IF[A-Z]{1,5}$") |
      str_detect(palavras, "^[^AEIOU\u00c1\u00c9\u00cd\u00d3\u00da\u00c2\u00ca\u00d4\u00c3\u00d5]{2,}$")
    sigla[palavras %in% c("DR", "SR", "STA", "STO", "PRF")] <- FALSE
    # "EM" so e sigla no inicio do nome; no meio e preposicao
    sigla[palavras == "EM" & seq_along(palavras) > 1] <- FALSE
    saida <- str_to_title(palavras, locale = "pt")
    saida[sigla] <- palavras[sigla]
    minusculas <- c("De", "Da", "Do", "Das", "Dos", "E", "Em", "Na", "No", "Nas", "Nos", "A", "O", "Ao")
    saida[seq_along(saida) > 1 & saida %in% minusculas] <- tolower(saida[seq_along(saida) > 1 & saida %in% minusculas])
    paste(saida, collapse = " ")
  }, character(1))
}

UF_SIGLA <- c(
  `11` = "RO", `12` = "AC", `13` = "AM", `14` = "RR", `15` = "PA", `16` = "AP", `17` = "TO",
  `21` = "MA", `22` = "PI", `23` = "CE", `24` = "RN", `25` = "PB", `26` = "PE", `27` = "AL",
  `28` = "SE", `29` = "BA", `31` = "MG", `32` = "ES", `33` = "RJ", `35` = "SP", `41` = "PR",
  `42` = "SC", `43` = "RS", `50` = "MS", `51` = "MT", `52` = "GO", `53` = "DF"
)
REGIAO <- c(`1` = "Norte", `2` = "Nordeste", `3` = "Sudeste", `4` = "Sul", `5` = "Centro-Oeste")

# 2. Import --------------------------------------------------------------------

## 2.1 Schools and typology ----------------------------------------------------

escolas_brutas <- import(file.path(caminho_painel, "mydata_tipologia.csv"),
                         encoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(across(where(is.character), corrigir_utf8))

auditoria <- import(file.path(caminho_tabelas_tese, "auditoria_tipologia_c4.csv"),
                    encoding = "UTF-8") %>%
  as_tibble() %>%
  select(CO_ENTIDADE, criterio_decisivo, versao_tipologia)

# PONTO CRITICO: o site so aceita a tipologia da versao declarada nos parametros
versoes <- unique(auditoria$versao_tipologia)
if (!identical(versoes, VERSAO_TIPOLOGIA)) {
  stop("Versao da tipologia diferente da esperada: ", paste(versoes, collapse = ", "),
       ". Atualize VERSAO_TIPOLOGIA em R/00_parametros.R depois de conferir.")
}
stopifnot(nrow(auditoria) == nrow(escolas_brutas),
          setequal(auditoria$CO_ENTIDADE, escolas_brutas$CO_ENTIDADE))

## 2.2 Territory: coordinates, weighting area and surroundings income ------------

# Coordenadas e AP vem do arquivo territorial completado (buffer de 400 m com
# complementacao pela AP). Resolver as duplicacoes priorizando o buffer.
territorio <- import(file.path(caminho_tese, "1_data", "mydata_renda_infra_400m_compl.csv"),
                     encoding = "UTF-8") %>%
  as_tibble() %>%
  arrange(CO_ENTIDADE, desc(me_rendpc_origem == "buffer_400m")) %>%
  distinct(CO_ENTIDADE, .keep_all = TRUE) %>%
  mutate(code_weighting = as.character(code_weighting),
         name_muni = corrigir_utf8(name_muni))

renda_ap <- import(file.path(caminho_tese, "1_data", "renda_areas_ponderacao_2010.csv")) %>%
  as_tibble() %>%
  mutate(code_weighting = as.character(code_weighting))

## 2.3 Cohort: race and RUF top 50 by school --------------------------------------

# Agregado por escola guardado em cache: a coorte tem 1,75 milhao de linhas.
arquivo_coorte_escola <- file.path(caminho_cache, "coorte_escola.csv")

if (!file.exists(arquivo_coorte_escola)) {

  colunas_coorte <- c("CO_ENTIDADE", "TP_COR_RACA",
                      paste0("CES_", 2016:2020), paste0("CO_IES_", 2016:2020))
  arquivo_parquet <- file.path(caminho_painel, "coorte2015.parquet")

  coorte <- if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::read_parquet(arquivo_parquet, col_select = all_of(colunas_coorte))
  } else {
    nanoparquet::read_parquet(arquivo_parquet, col_select = colunas_coorte)
  }

  # 50 primeiras do RUF 2015 (publicas e privadas), conferidas contra IES_2015
  ruf_top50 <- import(file.path(caminho_tese, "2_code", "sedap_plus_api", "INEP_DATA",
                                "ruf2015_co_ies.csv")) %>%
    filter(TOP50 == 1) %>%
    pull(CO_IES) %>%
    as.integer()

  ingressou_top50 <- rep(FALSE, nrow(coorte))
  for (ano in 2016:2020) {
    ingressou_top50 <- ingressou_top50 |
      (coorte[[paste0("CES_", ano)]] %in% 1 & coorte[[paste0("CO_IES_", ano)]] %in% ruf_top50)
  }

  # PPI: pretos, pardos e indigenas (2, 3 e 5) entre os declarados (cor != 0)
  coorte_escola <- tibble(
    CO_ENTIDADE = coorte$CO_ENTIDADE,
    declarou = coorte$TP_COR_RACA %in% 1:5,
    ppi = coorte$TP_COR_RACA %in% c(2, 3, 5),
    top50 = ingressou_top50
  ) %>%
    group_by(CO_ENTIDADE) %>%
    summarise(n_coorte = n(),
              n_cor_declarada = sum(declarou),
              n_ppi = sum(ppi),
              n_ruf50 = sum(top50),
              .groups = "drop")

  write_csv(coorte_escola, arquivo_coorte_escola)
  rm(coorte, ingressou_top50); gc()
}

coorte_escola <- read_csv(arquivo_coorte_escola, show_col_types = FALSE)

## 2.4 Weighting areas (geometry, Census 2010) ---------------------------------

# A malha de 2010 vem pronta do geobr; fica em cache para nao baixar de novo.
arquivo_geom_ap <- file.path(caminho_cache, "ap_2010_geom.rds")

if (!file.exists(arquivo_geom_ap)) {
  geom_ap <- geobr::read_weighting_area(year = 2010, simplified = TRUE,
                                        showProgress = FALSE) %>%
    transmute(code_weighting = as.character(code_weighting),
              code_muni = as.character(code_muni),
              code_state = as.character(code_state))
  saveRDS(geom_ap, arquivo_geom_ap)
}
geom_ap <- readRDS(arquivo_geom_ap)

# Composicao racial da AP pelos agregados por setor (censobr, cache local).
# Opcional: se o censobr nao estiver disponivel, a variavel fica de fora.
arquivo_raca_ap <- file.path(caminho_cache, "raca_ap_2010.csv")

if (!file.exists(arquivo_raca_ap)) {
  raca_ap <- tryCatch({
    basico <- censobr::read_tracts(year = 2010, dataset = "Basico", showProgress = FALSE) %>%
      select(code_tract, code_weighting)
    pessoa <- censobr::read_tracts(year = 2010, dataset = "Pessoa", showProgress = FALSE) %>%
      select(code_tract, pessoa03_V001, pessoa03_V003, pessoa03_V005, pessoa03_V006)
    basico %>%
      left_join(pessoa, by = "code_tract") %>%
      collect() %>%
      filter(!is.na(code_weighting)) %>%
      group_by(code_weighting) %>%
      summarise(
        pop_raca = sum(as.numeric(pessoa03_V001), na.rm = TRUE),
        pop_ppi = sum(as.numeric(pessoa03_V003), as.numeric(pessoa03_V005),
                      as.numeric(pessoa03_V006), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(code_weighting = as.character(code_weighting),
             p_ppi_ap = taxa(pop_ppi, pop_raca))
  }, error = function(e) {
    message("Composicao racial das APs nao calculada: ", conditionMessage(e))
    NULL
  })
  if (!is.null(raca_ap)) write_csv(raca_ap, arquivo_raca_ap)
}
raca_ap <- if (file.exists(arquivo_raca_ap)) {
  read_csv(arquivo_raca_ap, show_col_types = FALSE,
           col_types = cols(code_weighting = col_character()))
} else {
  NULL
}

## 2.5 Released Sedap tables and thesis tables ---------------------------------

tab_descritiva <- read.csv(file.path(caminho_sedap, "tab_2_descritiva_tipologia.csv"),
                           check.names = FALSE, encoding = "UTF-8") %>% as_tibble()
tab_resultados <- read.csv(file.path(caminho_sedap, "tab_3_resultados_tipologia.csv"),
                           check.names = FALSE, encoding = "UTF-8") %>% as_tibble()
cortes_nse <- import(file.path(caminho_tabelas_tese, "cortes_nse_c4.csv"), encoding = "UTF-8") %>%
  as_tibble()
perfil_social <- import(file.path(caminho_tabelas_tese, "tab_perfil_social_tipologia.csv"),
                        encoding = "UTF-8") %>% as_tibble()

# Conferencia da paleta com a da tese, quando o arquivo estiver acessivel
arquivo_paleta <- file.path(caminho_tese, "2_code", "R", "paleta_tipologia.R")
if (file.exists(arquivo_paleta)) {
  paleta_tese <- new.env()
  sys.source(arquivo_paleta, envir = paleta_tese)
  stopifnot(identical(paleta_tese$ORDEM_TIPOLOGIA_C4, ORDEM_TIPOLOGIA_C4),
            identical(paleta_tese$PALETA_TIPOLOGIA, PALETA_TIPOLOGIA[ORDEM_TIPOLOGIA_C4]))
}

# 3. Tidy ----------------------------------------------------------------------

## 3.1 School table ---------------------------------------------------------------

escolas <- escolas_brutas %>%
  left_join(auditoria, by = "CO_ENTIDADE") %>%
  left_join(territorio %>% select(CO_ENTIDADE, lat, lon, code_weighting, name_muni,
                                  infra_entorno = me_infrau_final),
            by = "CO_ENTIDADE") %>%
  left_join(coorte_escola, by = "CO_ENTIDADE") %>%
  mutate(
    uf = unname(UF_SIGLA[as.character(CO_UF)]),
    regiao = unname(REGIAO[as.character(CO_UF %/% 10)]),
    rede = case_when(dep_adm == "Privada" ~ "Privada",
                     dep_adm %in% c("Federal", "Estadual", "Municipal") ~ "Pública",
                     TRUE ~ NA_character_),
    localizacao = case_when(TP_LOCALIZACAO == 1 ~ "Urbana",
                            TP_LOCALIZACAO == 2 ~ "Rural",
                            TRUE ~ NA_character_),
    vinculo_dif = if_else(criterio_vinculo %in% TRUE, "Sim", "Não"),
    localizacao_dif = if_else(criterio_localizacao %in% TRUE, "Sim", "Não"),
    estrato_nse = if_else(elegivel_nse %in% TRUE & !is.na(GRUPO_ROMANO),
                          paste("NSE", GRUPO_ROMANO), "Não se aplica"),
    laboratorio = case_when(IN_LABORATORIO_CIENCIAS == 1 ~ "Sim",
                            IN_LABORATORIO_CIENCIAS == 0 ~ "Não",
                            TRUE ~ NA_character_),
    # Resultados da coorte (contagens de mydata_tipologia sobre os concluintes)
    p_enem = taxa(PENEM, matriculas),
    nota_enem = if_else(MENEM > 0, MENEM, NA_real_),
    p_es = taxa(CES, matriculas),
    p_es_pub = taxa(CES_PUB, matriculas),
    p_es_top = taxa(CES_PUB_TOP, matriculas),
    p_es_prv = taxa(CES_PRV, matriculas),
    p_ruf50 = taxa(n_ruf50, n_coorte),
    p_ppi = if_else(n_cor_declarada >= MIN_CONCLUINTES_RESULTADOS,
                    taxa(n_ppi, n_cor_declarada), NA_real_),
    suprimida = matriculas < MIN_CONCLUINTES_RESULTADOS
  )

# PONTO CRITICO: os totais da coorte nas duas fontes precisam coincidir
stopifnot(all(escolas$n_coorte == escolas$matriculas, na.rm = TRUE))

# Supressao das escolas pequenas e chave geral de publicacao dos resultados
colunas_resultado <- c("p_enem", "nota_enem", "p_es", "p_es_pub", "p_es_top", "p_es_prv", "p_ruf50")
escolas <- escolas %>%
  mutate(across(all_of(colunas_resultado),
                ~ if_else(suprimida | !PUBLICAR_RESULTADOS_ESCOLA, NA_real_, .x)))

escolas_site <- escolas %>%
  transmute(
    id = CO_ENTIDADE,
    nome = nome_escola_legivel(NO_ENTIDADE),
    municipio = name_muni,
    uf, regiao,
    tipologia, rede, dep_adm, localizacao,
    vinculo_dif, localizacao_dif, estrato_nse,
    criterio = criterio_decisivo,
    concluintes = matriculas,
    nse = round(NSE10, 2),
    icg = ICG,
    afd = round(afd, 1),
    iie = round(100 * IIE, 1),
    laboratorio,
    renda_entorno = round(RENDA_SM, 2),
    infra_entorno = round(100 * infra_entorno, 1),
    p_ppi = round(p_ppi, 1),
    p_enem = round(p_enem, 1),
    nota_enem = round(nota_enem, 1),
    p_es = round(p_es, 1),
    p_es_pub = round(p_es_pub, 1),
    p_es_top = round(p_es_top, 1),
    p_ruf50 = round(p_ruf50, 1),
    p_es_prv = round(p_es_prv, 1),
    suprimida,
    lat = round(lat, 5),
    lon = round(lon, 5),
    code_weighting
  ) %>%
  mutate(tipologia = factor(tipologia, levels = names(PALETA_TIPOLOGIA))) %>%
  arrange(tipologia, desc(concluintes))

## 3.2 Weighting areas ---------------------------------------------------------------

# Infraestrutura urbana media da AP (calculada no 02 - georef escolas.R)
infra_ap <- territorio %>%
  filter(!is.na(code_weighting)) %>%
  distinct(code_weighting, infra_ap)

escolas_por_ap <- escolas_site %>%
  filter(!is.na(code_weighting), tipologia %in% ORDEM_TIPOLOGIA_C4) %>%
  group_by(code_weighting) %>%
  summarise(n_escolas = n(),
            p_escolas_topo = 100 * mean(tipologia %in% c("Privada NSE IV", "Privada NSE V",
                                                         "Estadual NSE III", "Federal")),
            .groups = "drop")

municipios <- territorio %>%
  distinct(code_muni = as.character(CO_MUNICIPIO), name_muni, abbrev_state)

ap <- geom_ap %>%
  st_transform(5880) %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = TOLERANCIA_SIMPLIFICACAO_AP) %>%
  st_transform(4326) %>%
  left_join(renda_ap %>% select(code_weighting, pop_total, renda_pc_ap_2010, renda_sm_ap_2010),
            by = "code_weighting") %>%
  left_join(infra_ap, by = "code_weighting") %>%
  left_join(escolas_por_ap, by = "code_weighting") %>%
  left_join(municipios, by = "code_muni") %>%
  mutate(
    densidade = pop_total / area_km2,
    infra_ap = 100 * infra_ap,
    n_escolas = coalesce(n_escolas, 0L)
  )

if (!is.null(raca_ap)) {
  ap <- ap %>% left_join(raca_ap %>% select(code_weighting, p_ppi_ap), by = "code_weighting")
}

# Limpeza da malha: a malha do geobr traz fragmentos minusculos sem codigo de AP
# (nao ha como atribuir dado a eles) e alguns recortes viram GeometryCollection.
# Ambos apareciam no mapa como "sem informacao" ou quebravam a leitura do GeoJSON.
n_ap_antes <- nrow(ap)
ap <- ap %>%
  filter(!is.na(code_weighting), !st_is_empty(geometry)) %>%
  st_make_valid()
if (any(st_geometry_type(ap) == "GEOMETRYCOLLECTION")) {
  ap <- ap %>% st_collection_extract("POLYGON", warn = FALSE)
}
if (anyDuplicated(ap$code_weighting) > 0) {
  ap <- ap %>%
    group_by(code_weighting) %>%
    summarise(across(-geometry, ~ dplyr::first(.x)), geometry = st_union(geometry), .groups = "drop")
}
message("APs: ", n_ap_antes, " feicoes -> ", nrow(ap), " (", sum(!is.na(ap$renda_sm_ap_2010)),
        " com renda; ", sum(!is.na(ap$infra_ap)), " com infraestrutura)")

## 3.3 Indicators by type (local base, current typology) --------------------------

resumo_tipo <- function(d) {
  d %>%
    summarise(
      escolas = n(),
      concluintes = sum(concluintes, na.rm = TRUE),
      nse = mean(nse, na.rm = TRUE),
      p_icg_5_6 = 100 * mean(icg %in% 5:6 & !is.na(icg)) / mean(!is.na(icg)),
      afd = mean(afd, na.rm = TRUE),
      iie = mean(iie, na.rm = TRUE),
      p_laboratorio = 100 * mean(laboratorio == "Sim", na.rm = TRUE),
      renda_entorno = median(renda_entorno, na.rm = TRUE),
      p_rural = 100 * mean(localizacao == "Rural", na.rm = TRUE),
      .groups = "drop"
    )
}

indicadores_tipo <- bind_rows(
  escolas_site %>% filter(tipologia %in% ORDEM_TIPOLOGIA_C4) %>%
    resumo_tipo() %>% mutate(tipologia = "Brasil (dez tipos)"),
  escolas_site %>% filter(tipologia %in% ORDEM_TIPOLOGIA_C4) %>%
    mutate(tipologia = as.character(tipologia)) %>%
    group_by(tipologia) %>% resumo_tipo()
) %>%
  left_join(perfil_social %>% select(tipologia, P_PPI, P_PBF, P_RACA_NAO_DECLARADA) %>%
              mutate(tipologia = if_else(str_detect(tipologia, "^Brasil"),
                                         "Brasil (dez tipos)", tipologia)),
            by = "tipologia") %>%
  rename(p_ppi = P_PPI, p_pbf = P_PBF, p_cor_nd = P_RACA_NAO_DECLARADA)

if (!PUBLICAR_PBF) indicadores_tipo <- indicadores_tipo %>% select(-p_pbf)

contagem_tipos <- escolas_site %>%
  count(tipologia, name = "escolas") %>%
  mutate(tipologia = as.character(tipologia))

## 3.4 Results by type (released Sedap tables) ----------------------------------------

resultados <- tab_resultados %>% filter(tipologia %in% ORDEM_TIPOLOGIA_C4)
stopifnot(nrow(resultados) == 10)

# Linha Brasil: ponderada pelos concluintes de cada tipo
media_brasil <- function(x, peso) sum(x * peso, na.rm = TRUE) / sum(peso[!is.na(x)])

## 3.4.1 Enem and higher education ---------------------------------------------------

educacao <- resultados %>%
  transmute(tipologia, concluintes = matriculas,
            p_enem = 100 * PENEM, p_es = 100 * CES, p_es_pub = 100 * CES_PUB,
            p_es_top = 100 * CES_PUB_TOP, p_es_prv = 100 * CES_PRV,
            p_es_2016 = 100 * CES_2016, nota_enem = MENEM)
educacao <- bind_rows(
  educacao %>%
    summarise(tipologia = "Brasil (dez tipos)",
              # nota media ponderada pelos participantes do Enem (como na Tabela 3)
              nota_enem = media_brasil(nota_enem, concluintes * p_enem),
              across(-c(tipologia, concluintes, nota_enem), ~ media_brasil(.x, concluintes)),
              concluintes = sum(concluintes)),
  educacao
)

## 3.4.2 Formal labour market ------------------------------------------------------------

trabalho <- resultados %>%
  transmute(tipologia, concluintes = matriculas,
            p_coorte = 100 * matriculas / sum(matriculas),
            p_ocupado = 100 * P_OCUP,
            p_dirigentes = 100 * CLASSE_IBGE_P_DIR_CIEN,
            p_tecnicos = 100 * CLASSE_IBGE_P_TECN,
            p_servicos = 100 * CLASSE_IBGE_P_SERVICOS,
            p_industria = 100 * CLASSE_IBGE_P_INDUSTRIA)
trabalho <- bind_rows(
  trabalho %>%
    summarise(tipologia = "Brasil (dez tipos)",
              across(-c(tipologia, concluintes, p_coorte), ~ media_brasil(.x, concluintes)),
              p_coorte = 100, concluintes = sum(concluintes)),
  trabalho
)

salarios <- resultados %>%
  transmute(tipologia,
            ocupados = matriculas * P_OCUP,
            media = ME_SALARIO_RAIS, mediana = MA_SALARIO_RAIS,
            dp = sqrt(VAR_SALARIO_RAIS),
            q90 = Q90_SALARIO_RAIS, q95 = Q95_SALARIO_RAIS,
            q99 = Q99_SALARIO_RAIS, q999 = Q999_SALARIO_RAIS)

# Brasil: media e desvio-padrao combinados (entre + dentro dos tipos). Quantis
# do conjunto nao se recuperam dos agregados e ficam ausentes.
media_geral <- with(salarios, sum(media * ocupados) / sum(ocupados))
var_geral <- with(salarios, sum(ocupados * (dp^2 + (media - media_geral)^2)) / sum(ocupados))
salarios <- bind_rows(
  tibble(tipologia = "Brasil (dez tipos)", ocupados = sum(salarios$ocupados),
         media = media_geral, dp = sqrt(var_geral)),
  salarios
)

## 3.4.3 Variance share between types (eta squared) --------------------------------------

# Desfechos binarios: variancia entre tipos / p(1 - p), com os concluintes como peso
eta2_binario <- function(p, n) {
  p_geral <- sum(p * n) / sum(n)
  sum(n * (p - p_geral)^2) / sum(n) / (p_geral * (1 - p_geral))
}

eta2 <- tibble(
  desfecho = c("Ingresso na educação superior até 2020",
               "Ingresso na ES pública",
               "Ingresso nas universidades públicas de maior prestígio",
               "Ocupação formal em 2025",
               "Salário entre os ocupados"),
  eta2 = c(
    eta2_binario(resultados$CES, resultados$matriculas),
    eta2_binario(resultados$CES_PUB, resultados$matriculas),
    eta2_binario(resultados$CES_PUB_TOP, resultados$matriculas),
    eta2_binario(resultados$P_OCUP, resultados$matriculas),
    with(salarios %>% filter(tipologia != "Brasil (dez tipos)"),
         sum(ocupados * (media - media_geral)^2) / sum(ocupados) / var_geral)
  )
)

## 3.5 Distribution between schools (boxplots of the results page) -----------------------

distribuicao_escolas <- escolas_site %>%
  filter(tipologia %in% ORDEM_TIPOLOGIA_C4, !suprimida) %>%
  select(id, tipologia, p_enem, nota_enem, p_es, p_es_pub, p_ruf50) %>%
  mutate(tipologia = as.character(tipologia))

nse_distribuicao <- escolas %>%
  filter(elegivel_nse %in% TRUE, !is.na(NSE10)) %>%
  transmute(dep_adm, nse = round(NSE10, 3))

# 4. Analysis ------------------------------------------------------------------

## 4.1 Consistency checks against the chapter -----------------------------------------

# Tabela 3 do C4 (linha Brasil): 72,2; 47,6; 13,3; 4,9; 35,8; 504,1
stopifnot(abs(educacao$p_es[1] - 47.6) < 0.06,
          abs(educacao$p_enem[1] - 72.2) < 0.06,
          abs(educacao$nota_enem[1] - 504.1) < 0.06)
# Tabela 4 do C4: ocupados 59,0%; Tabela 5: salario medio 3.465
stopifnot(abs(trabalho$p_ocupado[1] - 59.0) < 0.06,
          abs(salarios$media[1] - 3465) < 1)

message("Escolas: ", nrow(escolas_site),
        " | com coordenadas: ", sum(!is.na(escolas_site$lat)),
        " | resultados suprimidos (< ", MIN_CONCLUINTES_RESULTADOS, " concluintes): ",
        sum(escolas_site$suprimida))
message("APs: ", nrow(ap), " | tamanho aproximado do GeoJSON: ",
        round(as.numeric(object.size(ap)) / 1e6, 1), " MB em memoria")

# 5. Export --------------------------------------------------------------------

meta <- list(
  versao_tipologia = VERSAO_TIPOLOGIA,
  extracao_sedap = EXTRACAO_SEDAP,
  min_concluintes = MIN_CONCLUINTES_RESULTADOS,
  publicar_resultados_escola = PUBLICAR_RESULTADOS_ESCOLA,
  publicar_pbf = PUBLICAR_PBF,
  cortes_nse = cortes_nse,
  n_escolas = nrow(escolas_site),
  n_escolas_tipos = sum(escolas_site$tipologia %in% ORDEM_TIPOLOGIA_C4),
  n_concluintes_local = sum(escolas_site$concluintes[escolas_site$tipologia %in% ORDEM_TIPOLOGIA_C4]),
  n_concluintes_sedap = sum(resultados$matriculas),
  n_com_coordenadas = sum(!is.na(escolas_site$lat)),
  tem_raca_ap = !is.null(raca_ap),
  gerado_em = format(Sys.time(), "%d/%m/%Y")
)

saveRDS(escolas_site, file.path(caminho_dados_site, "escolas.rds"))
saveRDS(st_drop_geometry(ap), file.path(caminho_dados_site, "areas_ponderacao.rds"))

# GeoJSON lido pelo navegador (fora do HTML da pagina, que fica mais leve).
# Os nomes das propriedades sao os ids das variaveis do mapa em escolas.qmd.
ap_web <- ap %>%
  transmute(
    cod = code_weighting,
    mun = name_muni,
    uf = abbrev_state,
    esc = n_escolas,
    renda = round(renda_sm_ap_2010, 3),
    densidade = round(densidade, 1),
    populacao = round(pop_total / 1000, 2),
    infra = round(infra_ap, 1),
    topo = if_else(n_escolas > 0, round(p_escolas_topo, 1), NA_real_),   # NA = AP sem escolas de EM
    ppi = if ("p_ppi_ap" %in% names(ap)) round(p_ppi_ap, 1) else NA_real_
  )
arquivo_geojson <- file.path(caminho_dados_site, "areas_ponderacao.geojson")
if (file.exists(arquivo_geojson)) file.remove(arquivo_geojson)
st_write(ap_web, arquivo_geojson, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=3", "RFC7946=YES"))
message("GeoJSON das APs: ", round(file.size(arquivo_geojson) / 1e6, 1), " MB")
saveRDS(indicadores_tipo, file.path(caminho_dados_site, "indicadores_tipo.rds"))
saveRDS(contagem_tipos, file.path(caminho_dados_site, "contagem_tipos.rds"))
saveRDS(educacao, file.path(caminho_dados_site, "resultados_educacao.rds"))
saveRDS(trabalho, file.path(caminho_dados_site, "resultados_trabalho.rds"))
saveRDS(salarios, file.path(caminho_dados_site, "resultados_salarios.rds"))
saveRDS(eta2, file.path(caminho_dados_site, "eta2.rds"))
saveRDS(distribuicao_escolas, file.path(caminho_dados_site, "distribuicao_escolas.rds"))
saveRDS(nse_distribuicao, file.path(caminho_dados_site, "nse_distribuicao.rds"))
saveRDS(meta, file.path(caminho_dados_site, "meta.rds"))

# Versao aberta da tabela de escolas para download no site
write_csv(escolas_site %>% select(-suprimida), file.path(caminho_dados_site, "escolas_tipologia_c4.csv"),
          na = "")
