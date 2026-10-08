# Origens & Destinos: As Escolas e a Estratificação (site em R e Quarto)

Site estático para o GitHub Pages que apresenta as escolas brasileiras de Ensino Médio: nome, características, resultados dos egressos da coorte de 2015 e localização. A página inicial apresenta a pesquisa; o texto da tese, o explorador de escolas, a descrição por tipo, os dados e as referências ficam em abas próprias.

## Estrutura

| Arquivo | Função |
|:--|:--|
| `R/00_parametros.R` | Caminhos, versão da tipologia, paleta e decisões de publicação |
| `R/01_preparar_dados.R` | Lê a base da tese e grava os dados compactos em `dados/` |
| `R/02_gerar_json.R` | Pré-render: gera `dados/escolas_mapa.json` (ficha, busca e comparação) |
| `R/funcoes_site.R` | Formatação pt-BR, componentes HTML, definições dos tipos, tema dos gráficos |
| `index.qmd` | Home: autoria, apresentação, citação e canal de sugestões e críticas |
| `tese.qmd` | A tese: texto navegável em formato de livro, com índice lateral |
| `escolas.qmd` | Explorador: busca, tipos, filtros, mapa (leaflet), ficha, comparação e tabela (reactable), ligados por crosstalk |
| `tipos.qmd` | Descrição: características e resultados por tipo (Enem, acesso, retornos), em tabela ou gráfico |
| `associacoes.qmd` | Associações: resultados de modelagem (em desenvolvimento) |
| `sobre.qmd` | Dados: fontes oficiais, protocolos de acesso, definições, proteção das informações e citação |
| `referencias.qmd` | Referências |
| `assets/explorador.js` | Mapa base, camadas, ficha, busca, filtros por tipo, área visível e comparação |
| `docs/` | Site gerado, publicado pelo GitHub Pages |

Mapas base sem chave de API: Esri (claro e satélite) e OpenStreetMap, com troca automática para o OpenStreetMap se o Esri não carregar.

## Como gerar

1. Pacotes (uma vez): `pacman::p_load(tidyverse, rio, sf, arrow, geobr, censobr, leaflet, reactable, crosstalk, plotly, htmlwidgets, htmltools, jsonlite)`. O `censobr` é opcional (composição racial das áreas de ponderação).
2. No RStudio, abrir e rodar `R/01_preparar_dados.R`. Ele lê `1_data`, `3_outp/1_tables` e a 3ª extração do Sedap a partir da raiz da tese (`../..`), baixa uma vez a malha das áreas de ponderação 2010 pelo `geobr` e guarda tudo em `dados/_cache`. Confere os totais Brasil contra as Tabelas 3, 4 e 5 do capítulo e para se algo divergir.
3. No terminal, na pasta do site: `quarto render` (o pré-render gera o JSON das escolas). Para ver localmente: `quarto preview` (abrir `docs/index.html` direto pelo arquivo não carrega as áreas de ponderação, que vêm de um GeoJSON externo).

## Como publicar no GitHub Pages

1. Criar o repositório (aqui, `tese-escola-estratificacao`) e ajustar `site-url` e `repo-url` em `_quarto.yml`.
2. Versionar esta pasta, inclusive `docs/` e `dados/*.rds`; o cache `dados/_cache` fica fora (`.gitignore`).
3. Em Settings > Pages: Deploy from a branch, branch `main`, pasta `/docs`.

## Antes de tornar público

- `PUBLICAR_RESULTADOS_ESCOLA` (em `R/00_parametros.R`): os resultados dos egressos por escola vêm da coorte pareada por CPF. Confirmar com o Sedap/Inep e com o orientador se a divulgação por escola é permitida; com `FALSE`, a tabela e o mapa mostram só os indicadores das escolas e os resultados ficam apenas por tipo.
- `MIN_CONCLUINTES_RESULTADOS = 10`: escolas menores têm os resultados suprimidos.
- `PUBLICAR_PBF = FALSE`: o percentual do Bolsa Família fica fora até a autorização do CadÚnico ser confirmada.
- A base completa por escola é oferecida para download em `dados/escolas_tipologia_c4.csv`.

## Versões

Tipologia `C4-texto-2026-09-30` (base local) e 3ª extração do Sedap/Inep (08/09/2026) para os resultados por tipo. Ao mudar a tipologia, atualizar `VERSAO_TIPOLOGIA` e rodar de novo o preparo; o script recusa uma base de outra versão.


## Censo 2022 no mapa (opcional)

`R/03_areas_2022.R` agrega o registro de Domicílios da amostra do Censo 2022 (acesso controlado) por área de ponderação e gera `dados/areas_ponderacao_2022.geojson`. Configure `PASTA_CENSO_2022` e, se o geobr não trouxer a malha de 2022, `ARQUIVO_GEOM_AP_2022` em `R/00_parametros.R`; depois rode `Rscript R/03_areas_2022.R` e renderize o site. Só estimativas agregadas vão para o site; os microdados não são copiados nem publicados (termo de confidencialidade). Sem o arquivo, a opção "Censo 2022" não aparece.
