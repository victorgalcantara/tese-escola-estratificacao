# Title: Leitura e formatacao das referencias da tese (BibTeX + CSV)
# Author: Victor G Alcantara | victorgalcantara@usp.br
#
# Le os arquivos .bib de referencias/, remove duplicatas, classifica por genero
# e devolve as referencias formatadas (estilo ABNT simplificado) com hiperlink.
# Genero adicional (Leis, Decretos, Fontes de dados, Filmes, Documentarios...):
# basta incluir uma linha em referencias/outras_referencias.csv.

ORDEM_GENEROS <- c("Artigos", "Livros e capítulos", "Teses e dissertações", "Leis", "Decretos",
                   "Documentos técnicos e institucionais", "Fontes de dados", "Normas e diretrizes",
                   "Software", "Filmes", "Documentários", "Outros")

# 1. Leitura de BibTeX ------------------------------------------------------------------

limpar_tex <- function(x) {
  x <- gsub("\\\\textsuperscript\\{o\\}", "º", x)
  x <- gsub("\\\\textsuperscript\\{a\\}", "ª", x)
  x <- gsub("\\\\&", "&", x)
  x <- gsub("\\\\_", "_", x)
  x <- gsub("---", "—", x, fixed = TRUE)
  x <- gsub("--", "–", x, fixed = TRUE)
  x <- gsub("[{}]", "", x)
  x <- gsub("\\\\[a-zA-Z]+\\s*", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

campo_bib <- function(corpo) {
  # Percorre "nome = valor", com valor entre chaves (aninhadas), aspas ou numero
  chars <- strsplit(corpo, "")[[1]]
  n <- length(chars); i <- 1; campos <- list()
  while (i <= n) {
    m <- regexpr("[A-Za-z_]+\\s*=\\s*", substr(corpo, i, n))
    if (m == -1) break
    ini <- i + m - 1
    cab <- substr(corpo, ini, ini + attr(m, "match.length") - 1)
    nome <- tolower(trimws(sub("=.*", "", cab)))
    j <- ini + attr(m, "match.length")
    c0 <- chars[j]
    if (c0 == "{") {
      prof <- 0; k <- j
      repeat {
        if (chars[k] == "{") prof <- prof + 1
        if (chars[k] == "}") prof <- prof - 1
        if (prof == 0 || k >= n) break
        k <- k + 1
      }
      valor <- substr(corpo, j + 1, k - 1); i <- k + 1
    } else if (c0 == "\"") {
      k <- j + 1
      while (k < n && chars[k] != "\"") k <- k + 1
      valor <- substr(corpo, j + 1, k - 1); i <- k + 1
    } else {
      k <- j
      while (k < n && !(chars[k] %in% c(",", "\n"))) k <- k + 1
      valor <- trimws(substr(corpo, j, k - 1)); i <- k + 1
    }
    campos[[nome]] <- valor
  }
  campos
}

ler_bib <- function(arquivo) {
  txt <- paste(readLines(arquivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  txt <- gsub("(?m)^\\s*%.*$", "", txt, perl = TRUE)
  inicios <- gregexpr("(?m)^@[A-Za-z]+\\s*\\{", txt, perl = TRUE)[[1]]
  if (inicios[1] == -1) return(list())
  fins <- c(inicios[-1] - 1, nchar(txt))
  lapply(seq_along(inicios), function(i) {
    bloco <- substr(txt, inicios[i], fins[i])
    tipo <- tolower(sub("^@([A-Za-z]+).*", "\\1", bloco))
    chave <- sub("^@[A-Za-z]+\\s*\\{\\s*([^,]+),.*", "\\1", bloco)
    corpo <- sub("^@[A-Za-z]+\\s*\\{\\s*[^,]+,", "", bloco)
    c(list(tipo = tipo, chave = trimws(chave)), campo_bib(corpo))
  })
}

# 2. Formatacao -----------------------------------------------------------------------------

esc_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

ano_de <- function(r) {
  a <- r$date %||% r$year %||% ""
  m <- regmatches(a, regexpr("[0-9]{4}", a))
  if (length(m)) m else "s.d."
}
`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

formatar_autores <- function(a) {
  if (is.null(a) || !nzchar(a)) return("")
  # Autor institucional vem entre chaves duplas: {{Nome}}
  partes <- trimws(strsplit(a, "\\s+and\\s+")[[1]])
  fmt <- vapply(partes, function(p) {
    inst <- grepl("^\\{.*\\}$", p)
    p <- limpar_tex(p)
    p <- sub(",\\s*$", "", p)
    if (inst || !grepl(",", p)) return(toupper(p))
    sob <- trimws(sub(",.*", "", p)); nom <- trimws(sub("^[^,]*,", "", p))
    paste0(toupper(sob), ", ", nom)
  }, character(1))
  out <- if (length(fmt) > 3) paste0(fmt[1], " et al.") else paste(fmt, collapse = "; ")
  sub("\\.$", "", out)
}

link_ref <- function(r) {
  doi <- r$doi %||% ""
  url <- r$url %||% ""
  if (nzchar(doi)) {
    d <- sub("^https?://(dx\\.)?doi\\.org/", "", doi)
    return(sprintf(' <a class="ref-link" href="https://doi.org/%s" target="_blank" rel="noopener">https://doi.org/%s</a>', d, d))
  }
  if (nzchar(url)) {
    return(sprintf(' Disponível em: <a class="ref-link" href="%s" target="_blank" rel="noopener">%s</a>',
                   url, esc_html(if (nchar(url) > 70) paste0(substr(url, 1, 67), "...") else url)))
  }
  ""
}

formatar_ref <- function(r) {
  aut <- esc_html(formatar_autores(r$author %||% r$editor %||% r$organization %||% ""))
  tit <- esc_html(limpar_tex(r$title %||% ""))
  ano <- ano_de(r)
  partes <- character()
  corpo <- switch(r$tipo,
    article = {
      rev <- esc_html(limpar_tex(r$journal %||% r$journaltitle %||% ""))
      vn <- paste0(if (nzchar(r$volume %||% "")) paste0(", v. ", r$volume) else "",
                   if (nzchar(r$number %||% "")) paste0(", n. ", r$number) else "",
                   if (nzchar(r$pages %||% "")) paste0(", p. ", limpar_tex(r$pages)) else "")
      sprintf("%s. <em>%s</em>%s, %s.", tit, rev, vn, ano)
    },
    book = , collection = , proceedings = {
      loc <- limpar_tex(r$address %||% r$location %||% "")
      ed <- limpar_tex(r$publisher %||% "")
      pub <- paste(c(loc, ed)[nzchar(c(loc, ed))], collapse = ": ")
      sprintf("<em>%s</em>.%s %s.", tit, if (nzchar(pub)) paste0(" ", esc_html(pub), ",") else "", ano)
    },
    inbook = , incollection = {
      bt <- limpar_tex(r$booktitle %||% "")
      ed <- limpar_tex(r$publisher %||% "")
      sprintf("%s.%s%s %s.", tit, if (nzchar(bt)) sprintf(" In: <em>%s</em>.", esc_html(bt)) else "",
              if (nzchar(ed)) paste0(" ", esc_html(ed), ",") else "", ano)
    },
    phdthesis = , mastersthesis = {
      esc <- limpar_tex(r$school %||% r$institution %||% "")
      tp <- limpar_tex(r$type %||% if (r$tipo == "phdthesis") "Tese (Doutorado)" else "Dissertação (Mestrado)")
      sprintf("<em>%s</em>. %s, %s.", tit, esc_html(tp), paste(c(esc_html(esc), ano)[nzchar(c(esc, ano))], collapse = ", "))
    },
    {
      org <- limpar_tex(r$publisher %||% r$organization %||% "")
      loc <- limpar_tex(r$address %||% "")
      pub <- paste(c(loc, org)[nzchar(c(loc, org))], collapse = ": ")
      sprintf("<em>%s</em>.%s %s.", tit, if (nzchar(pub)) paste0(" ", esc_html(pub), ",") else "", ano)
    })
  paste0(if (nzchar(aut)) paste0(aut, ". ") else "", corpo, link_ref(r))
}

genero_de <- function(r) {
  tit <- tolower(limpar_tex(r$title %||% ""))
  switch(r$tipo,
    article = "Artigos",
    book = , inbook = , incollection = , collection = , proceedings = "Livros e capítulos",
    phdthesis = , mastersthesis = "Teses e dissertações",
    dataset = "Fontes de dados",
    manual = if (grepl("^nbr|diretrizes", tit)) "Normas e diretrizes" else "Software",
    misc = , techreport = , report = if (grepl("nota t.cnica|nota\\s+t", tit)) "Documentos técnicos e institucionais" else "Outros",
    "Outros")
}

chave_ordem <- function(r) {
  a <- tolower(iconv(formatar_autores(r$author %||% r$editor %||% r$organization %||% r$title %||% ""),
                     to = "ASCII//TRANSLIT"))
  paste0(gsub("[^a-z0-9]", "", a), ano_de(r))
}
chave_dedup <- function(r) {
  # Teses: mesmo autor e ano contam como a mesma obra (a versao do C4.bib prevalece)
  if (r$tipo %in% c("phdthesis", "mastersthesis"))
    return(paste0("tese:", gsub("[^a-z0-9]", "", tolower(iconv(sub(",.*", "", limpar_tex(r$author %||% "")), to = "ASCII//TRANSLIT"))), ano_de(r)))
  doi <- tolower(r$doi %||% "")
  if (nzchar(doi)) return(paste0("doi:", sub("^https?://(dx\\.)?doi\\.org/", "", doi)))
  paste0("t:", gsub("[^a-z0-9]", "", tolower(iconv(limpar_tex(r$title %||% ""), to = "ASCII//TRANSLIT"))))
}

# 3. Interface ---------------------------------------------------------------------------------

carregar_referencias <- function(pasta = "referencias") {
  regs <- unlist(lapply(sort(list.files(pasta, pattern = "\\.bib$", full.names = TRUE)), ler_bib), recursive = FALSE)
  regs <- Filter(function(r) nzchar(r$title %||% ""), regs)
  # Duplicatas: mantem a primeira (ordem alfabetica dos arquivos; C4.bib vem primeiro)
  regs <- regs[!duplicated(vapply(regs, chave_dedup, character(1)))]
  d <- data.frame(
    genero = vapply(regs, genero_de, character(1)),
    ordem = vapply(regs, chave_ordem, character(1)),
    html = vapply(regs, formatar_ref, character(1)),
    stringsAsFactors = FALSE)

  arq_extras <- file.path(pasta, "outras_referencias.csv")
  if (file.exists(arq_extras)) {
    ex <- utils::read.csv(arq_extras, stringsAsFactors = FALSE, encoding = "UTF-8")
    rex <- lapply(seq_len(nrow(ex)), function(i) {
      e <- ex[i, ]
      list(genero = e$genero,
           ordem = paste0(gsub("[^a-z0-9]", "", tolower(iconv(e$autor, to = "ASCII//TRANSLIT"))),
                          gsub("[^a-z0-9]", "", tolower(iconv(e$titulo, to = "ASCII//TRANSLIT")))),
           html = paste0(esc_html(toupper(e$autor)), ". ", esc_html(e$titulo), ". ",
                         if (nzchar(e$detalhe)) paste0(esc_html(e$detalhe), ". ") else "", e$ano, ".",
                         link_ref(list(url = e$url))))
    })
    d <- rbind(d, data.frame(genero = vapply(rex, `[[`, "", "genero"), ordem = vapply(rex, `[[`, "", "ordem"),
                             html = vapply(rex, `[[`, "", "html"), stringsAsFactors = FALSE))
  }
  d$genero <- factor(d$genero, levels = union(ORDEM_GENEROS, unique(d$genero)))
  d[order(d$genero, d$ordem), ]
}
