// Explorador de escolas: mapa, ficha, busca, filtros por tipo, comparacao.
// Chamado por htmlwidgets::onRender em index.qmd. `dados` vem de R:
//   grupo_escolas, url_escolas, id_tabela, min_concluintes
//   tipos: { ordem, paleta, definicoes }
//   ap: { url, inicial, variaveis: [{id, rotulo, fonte, cortes, cores, rotulos}] }
//   variaveis_escolas: [{id, campo, rotulo, cortes, cores, rotulos}]
function(el, x, dados) {
  var map = this;
  var lm = map.layerManager;
  var SEM_DADO = "#d9d9d9";
  var MAX_COMP = 4;
  var BRASIL = L.latLngBounds([-33.9, -73.9], [5.4, -34.7]);

  var E = null;            // dados das escolas (JSON colunar)
  var posicao = {};        // id -> indice nas colunas de E
  var camadas = {};        // id -> camada leaflet (so escolas com coordenadas)
  var atual = null;        // id da escola aberta na ficha
  var comparadas = [];     // ids das escolas em comparacao

  // Utilidades -----------------------------------------------------------------------
  function q(sel, raiz) { return (raiz || document).querySelector(sel); }
  function qa(sel, raiz) { return Array.prototype.slice.call((raiz || document).querySelectorAll(sel)); }
  function esc(s) {
    return String(s === null || s === undefined ? "" : s).replace(/[&<>"]/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c];
    });
  }
  function fmt(v, casas, sufixo) {
    if (v === null || v === undefined || isNaN(v)) return null;
    return v.toLocaleString("pt-BR", { minimumFractionDigits: casas, maximumFractionDigits: casas }) + (sufixo || "");
  }
  function classe(valor, cortes) {
    if (valor === null || valor === undefined || isNaN(valor)) return -1;
    for (var i = 0; i < cortes.length; i++) if (valor <= cortes[i]) return i;
    return cortes.length;
  }
  function variavel(lista, id) { return lista.filter(function (d) { return d.id === id; })[0]; }
  function semAcento(s) { return String(s || "").normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase(); }
  function atraso(fn, ms) {
    var t = null;
    return function () { var a = arguments; clearTimeout(t); t = setTimeout(function () { fn.apply(null, a); }, ms); };
  }
  function pillTipo(i) {
    var tipo = dados.tipos.ordem[E.tipo[i]];
    var cor = dados.tipos.paleta[tipo] || "#999";
    return '<span class="tipo-pill" style="--tipo:' + cor + '"><span class="tipo-dot"></span>' + esc(tipo) + "</span>";
  }

  // 1. Mapas base sem chave de API ---------------------------------------------------
  var ATTR_ESRI = "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ";
  var ESRI = "https://server.arcgisonline.com/ArcGIS/rest/services/";
  map.createPane("rotulos");
  map.getPane("rotulos").style.zIndex = 380;
  map.getPane("rotulos").style.pointerEvents = "none";

  var bases = {
    claro: {
      rotulo: "Claro (Esri)",
      camadas: [
        L.tileLayer(ESRI + "Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
          { attribution: ATTR_ESRI, maxNativeZoom: 16, maxZoom: 19 }),
        L.tileLayer(ESRI + "Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
          { pane: "rotulos", maxNativeZoom: 16, maxZoom: 19 })
      ]
    },
    ruas: {
      rotulo: "Ruas (OpenStreetMap)",
      camadas: [
        L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png",
          { attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>', maxZoom: 19 })
      ]
    },
    satelite: {
      rotulo: "Satélite (Esri)",
      camadas: [
        L.tileLayer(ESRI + "World_Imagery/MapServer/tile/{z}/{y}/{x}",
          { attribution: "Tiles &copy; Esri &mdash; Esri, Maxar, Earthstar Geographics, and the GIS User Community",
            maxNativeZoom: 18, maxZoom: 19 })
      ]
    }
  };
  var baseAtual = null;
  function trocarBase(id) {
    if (baseAtual) bases[baseAtual].camadas.forEach(function (c) { map.removeLayer(c); });
    baseAtual = id;
    bases[id].camadas.forEach(function (c) { c.addTo(map); });
  }
  // Se o mapa base padrao nao carregar (rede bloqueada, servico fora do ar), troca para o OpenStreetMap
  var erros = 0, trocouAuto = false;
  bases.claro.camadas[0].on("tileerror", function () {
    erros++;
    if (erros >= 8 && !trocouAuto && baseAtual === "claro") {
      trocouAuto = true;
      trocarBase("ruas");
      var s = q(".sel-base", el);
      if (s) s.value = "ruas";
    }
  });
  trocarBase("claro");

  // 2. Areas de ponderacao (GeoJSON externo, um arquivo por censo) -----------------------
  map.createPane("areas");
  map.getPane("areas").style.zIndex = 350;
  var rendererAreas = L.canvas({ pane: "areas", padding: 0.3 });
  var cfgAnos = { "2010": dados.ap };
  if (dados.ap22) cfgAnos["2022"] = dados.ap22;
  var anoAp = "2010";
  var camadasAp = {};      // ano -> camada leaflet
  var carregandoAp = {};   // ano -> true enquanto o GeoJSON baixa
  var varAp = dados.ap.inicial;
  var cfg = function () { return cfgAnos[anoAp]; };

  function corAp(props) {
    var v = variavel(cfg().variaveis, varAp);
    if (!v) return SEM_DADO;
    var k = classe(props[varAp], v.cortes);
    return k < 0 ? SEM_DADO : v.cores[k];
  }
  function rotuloAp(p) {
    return "<b>" + esc(p.mun || "") + (p.uf ? " (" + esc(p.uf) + ")" : "") + "</b><br>" +
      "Área de ponderação " + esc(p.cod) + " (Censo " + anoAp + ")<br>" +
      "Renda per capita: " + (fmt(p.renda, 2, " SM") || "n.d.") + "<br>" +
      "Densidade: " + (fmt(p.densidade, 0, " hab./km²") || "n.d.") + "<br>" +
      "Escolas de Ensino Médio: " + (p.esc || 0);
  }
  // Parcela das areas com valor na variavel (ajuda a ver quando "sem informacao" e a regra, nao falha)
  function cobertura(idVar) {
    var camada = camadasAp[anoAp];
    if (!camada) return "";
    var n = 0, ok = 0;
    camada.eachLayer(function (l) {
      n++;
      var v = l.feature.properties[idVar];
      if (v !== null && v !== undefined && !isNaN(v)) ok++;
    });
    return n ? '<div class="leg-fonte">Com valor em ' + Math.round(100 * ok / n) + "% das " + n.toLocaleString("pt-BR") + " áreas.</div>" : "";
  }
  function legendaAp(idVar) {
    if (idVar === "nenhuma") return "";
    var v = variavel(cfg().variaveis, idVar);
    if (!v) return "";
    var html = '<div class="leg-titulo">' + v.rotulo + " · Censo " + anoAp + "</div>";
    v.cores.forEach(function (cor, i) {
      html += '<div class="leg-item"><span class="leg-cor" style="background:' + cor + '"></span>' + v.rotulos[i] + "</div>";
    });
    html += '<div class="leg-item"><span class="leg-cor" style="background:' + SEM_DADO + '"></span>' +
      (v.sem || "sem informação") + "</div>" + cobertura(idVar);
    return html + '<div class="leg-fonte">' + v.fonte + "</div>";
  }
  function atualizarLegendaAp() { legAp.innerHTML = legendaAp(selAp.value); }
  function pintarAp(idVar) {
    Object.keys(camadasAp).forEach(function (a) { if (a !== anoAp && map.hasLayer(camadasAp[a])) map.removeLayer(camadasAp[a]); });
    var camada = camadasAp[anoAp];
    if (!camada) return;
    if (idVar === "nenhuma") { if (map.hasLayer(camada)) map.removeLayer(camada); return; }
    varAp = idVar;
    if (!map.hasLayer(camada)) map.addLayer(camada);
    camada.setStyle(function (f) { return { fillColor: corAp(f.properties) }; });
  }
  function carregarAp(ano, depois) {
    if (camadasAp[ano]) { depois(); return; }
    if (carregandoAp[ano]) return;
    carregandoAp[ano] = true;
    legAp.innerHTML = '<div class="leg-fonte">Carregando áreas de ponderação de ' + ano + "...</div>";
    fetch(cfgAnos[ano].url)
      .then(function (r) { if (!r.ok) throw new Error(r.status); return r.json(); })
      .then(function (gj) {
        camadasAp[ano] = L.geoJSON(gj, {
          pane: "areas",
          renderer: rendererAreas,
          style: function (f) {
            return { fillColor: ano === anoAp ? corAp(f.properties) : SEM_DADO, fillOpacity: 0.62, color: "#ffffff", weight: 0.4, opacity: 0.8 };
          },
          onEachFeature: function (f, camada) {
            camada.bindTooltip(function () { return rotuloAp(f.properties); }, { sticky: true });
            camada.on("mouseover", function () { camada.setStyle({ weight: 1.6, color: "#0f1b2d" }); });
            camada.on("mouseout", function () { camada.setStyle({ weight: 0.4, color: "#ffffff" }); });
          }
        });
        carregandoAp[ano] = false;
        depois();
      })
      .catch(function () {
        carregandoAp[ano] = false;
        legAp.innerHTML = '<div class="leg-fonte">Não foi possível carregar as áreas de ponderação de ' + ano + ". " +
          "Abra o site por um servidor (GitHub Pages ou quarto preview), e não pelo arquivo local.</div>";
      });
  }
  function aplicarAp() {
    if (selAp.value === "nenhuma") { pintarAp("nenhuma"); legAp.innerHTML = ""; return; }
    carregarAp(anoAp, function () { pintarAp(selAp.value); atualizarLegendaAp(); });
  }
  function opcoesVariaveisAp() {
    return '<option value="nenhuma">Sem cor</option>' + cfg().variaveis.map(function (v) {
      return '<option value="' + v.id + '">' + v.rotulo + "</option>";
    }).join("");
  }

  // 3. Painel de controles do mapa ------------------------------------------------------
  var opcoesBase = Object.keys(bases).map(function (k) {
    return '<option value="' + k + '">' + bases[k].rotulo + "</option>";
  }).join("");
  var opcoesAnos = Object.keys(cfgAnos).map(function (a) { return '<option value="' + a + '">Censo ' + a + "</option>"; }).join("");
  var opcoesEsc = '<option value="tipologia">Tipo escolar</option>' + dados.variaveis_escolas.map(function (v) {
    return '<option value="' + v.id + '">' + v.rotulo + "</option>";
  }).join("");

  var Controle = L.Control.extend({
    options: { position: "topright" },
    onAdd: function () {
      var div = L.DomUtil.create("div", "controle-mapa");
      div.innerHTML =
        '<button type="button" class="cm-topo" aria-expanded="true"><span>Camadas e cores</span><span class="cm-seta">&#9662;</span></button>' +
        '<div class="cm-corpo">' +
        '<label>Mapa base</label><select class="sel-base">' + opcoesBase + "</select>" +
        '<label>Cor das escolas</label><select class="sel-esc">' + opcoesEsc + "</select>" +
        '<label class="cm-check"><input type="checkbox" class="chk-esc" checked> Mostrar escolas</label>' +
        '<label>Áreas de ponderação</label>' +
        (Object.keys(cfgAnos).length > 1 ? '<select class="sel-ano" aria-label="Censo">' + opcoesAnos + "</select>" : "") +
        '<select class="sel-ap">' + opcoesVariaveisAp() + "</select>" +
        '<div class="leg-esc"></div><div class="leg-ap"><div class="leg-fonte">Carregando áreas de ponderação...</div></div>' +
        "</div>";
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    }
  });
  map.addControl(new Controle());

  var selBase = q(".sel-base", el), selEsc = q(".sel-esc", el), selAp = q(".sel-ap", el);
  var legEsc = q(".leg-esc", el), legAp = q(".leg-ap", el), chkEsc = q(".chk-esc", el);
  var painel = q(".controle-mapa", el);
  var selAno = q(".sel-ano", el);
  selBase.value = "claro"; selEsc.value = "tipologia"; selAp.value = dados.ap.inicial;

  q(".cm-topo", el).addEventListener("click", function () {
    var fechado = painel.classList.toggle("recolhido");
    this.setAttribute("aria-expanded", String(!fechado));
  });
  if (window.innerWidth < 700) painel.classList.add("recolhido");

  selBase.addEventListener("change", function () { trocarBase(selBase.value); });
  selEsc.addEventListener("change", function () { pintarEscolas(selEsc.value); });
  selAp.addEventListener("change", aplicarAp);
  if (selAno) selAno.addEventListener("change", function () {
    var anterior = selAp.value;
    anoAp = selAno.value;
    selAp.innerHTML = opcoesVariaveisAp();
    // Mantem a variavel quando ela existe no outro censo
    selAp.value = variavel(cfg().variaveis, anterior) || anterior === "nenhuma" ? anterior : cfg().inicial;
    aplicarAp();
  });
  chkEsc.addEventListener("change", function () {
    if (chkEsc.checked) lm.showGroup(dados.grupo_escolas); else lm.hideGroup(dados.grupo_escolas);
  });

  aplicarAp();

  // 4. Cores dos pontos das escolas ---------------------------------------------------
  function pintarEscolas(idVar) {
    if (!E) return;
    var ids = Object.keys(camadas);
    if (idVar === "tipologia") {
      ids.forEach(function (id) {
        camadas[id].setStyle({ fillColor: dados.tipos.paleta[dados.tipos.ordem[E.tipo[posicao[id]]]] || SEM_DADO });
      });
      legEsc.innerHTML = "";
      return;
    }
    var v = variavel(dados.variaveis_escolas, idVar);
    var col = E[v.campo];
    ids.forEach(function (id) {
      var k = classe(col[posicao[id]], v.cortes);
      camadas[id].setStyle({ fillColor: k < 0 ? SEM_DADO : v.cores[k] });
    });
    var html = '<div class="leg-titulo">' + v.rotulo + "</div>";
    v.cores.forEach(function (cor, i) {
      html += '<div class="leg-item"><span class="leg-ponto" style="background:' + cor + '"></span>' + v.rotulos[i] + "</div>";
    });
    html += '<div class="leg-item"><span class="leg-ponto" style="background:' + SEM_DADO + '"></span>suprimido ou sem dado</div>';
    legEsc.innerHTML = html;
  }

  // 5. Ficha da escola ------------------------------------------------------------------
  var Ficha = L.Control.extend({
    options: { position: "topleft" },
    onAdd: function () {
      var div = L.DomUtil.create("div", "ficha oculta");
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    }
  });
  map.addControl(new Ficha());
  var elFicha = q(".ficha", el);
  var anel = null;

  function valor(i, campo, casas, sufixo, resultado) {
    var v = E[campo][i];
    var t = fmt(v, casas, sufixo);
    if (t !== null) return t;
    return resultado && E.supr[i] ? '<span class="suprimido" title="Menos de ' + dados.min_concluintes + ' concluintes">supr.</span>'
                                  : '<span class="suprimido">n.d.</span>';
  }
  function linhaBarra(rotulo, i, campo, destaque) {
    var v = E[campo][i];
    var barra = v === null || v === undefined ? "" :
      '<div class="mini-trilho"><div class="' + (destaque ? "forte" : "") + '" style="width:' + Math.min(100, v) + '%"></div></div>';
    return '<div class="res-linha"><div class="res-topo"><span>' + rotulo + "</span><b>" +
      valor(i, campo, 1, "%", true) + "</b></div>" + barra + "</div>";
  }

  function htmlFicha(i) {
    var caract = [];
    caract.push(E.rede[i]);
    caract.push(E.dep[i]);
    caract.push(E.loc[i]);
    if (E.vinc[i] === "Sim") caract.push("Vínculo diferenciado");
    if (E.locdif[i] === "Sim") caract.push("Localização diferenciada");
    if (E.estrato[i] && E.estrato[i] !== "Não se aplica") caract.push(E.estrato[i]);
    var chips = caract.filter(Boolean).map(function (c) { return '<span class="chip-car">' + esc(c) + "</span>"; }).join("");

    var temCoord = E.lat[i] !== null && E.lat[i] !== undefined;
    var resultados;
    if (E.supr[i]) {
      resultados = '<p class="ficha-aviso">Resultados não exibidos: menos de ' + dados.min_concluintes +
        " concluintes na coorte de 2015.</p>";
    } else {
      resultados =
        '<div class="res-linha"><div class="res-topo"><span>Nota média no Enem</span><b>' + valor(i, "nota", 1, "", true) + "</b></div></div>" +
        linhaBarra("Participou do Enem", i, "enem") +
        linhaBarra("Ingressou no ensino superior", i, "es", true) +
        linhaBarra("em instituição pública", i, "espub") +
        linhaBarra("em pública de maior prestígio", i, "estop") +
        linhaBarra("em uma das 50 primeiras do RUF", i, "ruf");
    }

    return '<div class="ficha-topo"><div><div class="ficha-nome">' + esc(E.nome[i]) + "</div>" +
      '<div class="ficha-local">' + esc(E.mun[i]) + " (" + esc(E.uf[i]) + ")</div>" + pillTipo(i) + "</div>" +
      '<button type="button" class="ficha-fechar" data-acao="fechar" aria-label="Fechar ficha">&times;</button></div>' +
      '<div class="ficha-corpo">' +
      '<div class="ficha-chips">' + chips + "</div>" +
      '<h5>Características</h5><div class="ficha-grade">' +
      '<div><span>Concluintes (2015)</span><b>' + valor(i, "concl", 0, "") + "</b></div>" +
      '<div><span>NSE (0 a 10)</span><b>' + valor(i, "nse", 2, "") + "</b></div>" +
      '<div><span>ICG (1 a 6)</span><b>' + valor(i, "icg", 0, "") + "</b></div>" +
      '<div><span>AFD adequada</span><b>' + valor(i, "afd", 1, "%") + "</b></div>" +
      '<div><span>IIE (0 a 100)</span><b>' + valor(i, "iie", 1, "") + "</b></div>" +
      '<div><span>Lab. de ciências</span><b>' + esc(E.lab[i] || "n.d.") + "</b></div>" +
      '<div><span>Renda do entorno</span><b>' + valor(i, "renda", 2, " SM") + "</b></div>" +
      '<div><span>Pretos, pardos e indígenas</span><b>' + valor(i, "ppi", 1, "%") + "</b></div></div>" +
      "<h5>Resultados dos egressos</h5>" + resultados +
      '<div class="ficha-acoes">' +
      (temCoord ? '<button type="button" class="btn-acao" data-acao="aproximar">Aproximar no mapa</button>' : '<span class="suprimido">Sem coordenadas</span>') +
      '<button type="button" class="btn-acao" data-acao="comparar">+ Comparar</button>' +
      '<button type="button" class="btn-acao" data-acao="link">Copiar link</button></div>' +
      '<div class="ficha-cod">Código Inep ' + esc(E.id[i]) + "</div></div>";
  }

  function destacarNoMapa(i) {
    if (anel) { map.removeLayer(anel); anel = null; }
    if (E.lat[i] === null || E.lat[i] === undefined) return;
    var raio = 8 + (camadas[E.id[i]] ? camadas[E.id[i]].getRadius() : 5);
    anel = L.circleMarker([E.lat[i], E.lon[i]], {
      radius: raio, color: "#c8553d", weight: 3, fill: false, interactive: false
    }).addTo(map);
    anel.bringToFront();
  }

  function irParaTabela(id) {
    if (!window.Reactable) return;
    try {
      Reactable.setMeta(dados.id_tabela, { atual: id });
      var st = Reactable.getState(dados.id_tabela);
      var linhas = st.sortedData || [];
      for (var k = 0; k < linhas.length; k++) {
        if (String(linhas[k].id) === id) {
          Reactable.gotoPage(dados.id_tabela, Math.floor(k / st.pageSize));
          return;
        }
      }
    } catch (e) { /* tabela ainda nao pronta */ }
  }

  function centrarCom(ll, zoom) {
    // Desloca o centro para que a escola fique fora da area coberta pela ficha
    var largura = map.getSize().x;
    var deslocamento = largura >= 700 ? 165 : 0;
    var alvo = map.project(ll, zoom).subtract([deslocamento, 0]);
    return map.unproject(alvo, zoom);
  }

  function abrir(id, opcoes) {
    id = String(id);
    var i = posicao[id];
    if (i === undefined || !E) return;
    opcoes = opcoes || {};
    atual = id;
    elFicha.innerHTML = htmlFicha(i);
    elFicha.classList.remove("oculta");
    destacarNoMapa(i);
    irParaTabela(id);
    try { history.replaceState(null, "", "#escola=" + id); } catch (e) { /* ignora */ }
    if (opcoes.voar && E.lat[i] !== null && E.lat[i] !== undefined) {
      var zoom = Math.max(map.getZoom(), 14);
      map.flyTo(centrarCom(L.latLng(E.lat[i], E.lon[i]), zoom), zoom, { duration: 0.9 });
    } else if (E.lat[i] !== null && E.lat[i] !== undefined && map.getSize().x >= 700) {
      var p = map.latLngToContainerPoint([E.lat[i], E.lon[i]]);
      if (p.x < 340) map.panBy([p.x - 360, 0], { animate: true });
    }
    var el_busca = q("#busca-escola");
    if (el_busca && opcoes.limparBusca) el_busca.value = "";
  }

  function fechar() {
    atual = null;
    elFicha.classList.add("oculta");
    if (anel) { map.removeLayer(anel); anel = null; }
    if (window.Reactable) { try { Reactable.setMeta(dados.id_tabela, { atual: "" }); } catch (e) { /* ignora */ } }
    try { history.replaceState(null, "", location.pathname + location.search); } catch (e) { /* ignora */ }
  }

  elFicha.addEventListener("click", function (ev) {
    var b = ev.target.closest("[data-acao]");
    if (!b || atual === null) return;
    var acao = b.getAttribute("data-acao");
    if (acao === "fechar") fechar();
    if (acao === "aproximar") abrir(atual, { voar: true });
    if (acao === "comparar") comparar(atual);
    if (acao === "link") {
      var url = location.origin + location.pathname + "#escola=" + atual;
      var ok = function () { b.textContent = "Link copiado"; setTimeout(function () { b.textContent = "Copiar link"; }, 1800); };
      if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(url).then(ok);
      else ok();
    }
  });

  // 6. Comparacao ----------------------------------------------------------------------------
  var elComp = q("#comparador");

  var LINHAS_COMP = [
    ["Município", function (i) { return esc(E.mun[i]) + " (" + esc(E.uf[i]) + ")"; }],
    ["Tipo", pillTipo],
    ["Dependência / localização", function (i) { return esc(E.dep[i]) + " · " + esc(E.loc[i]); }],
    ["Concluintes (2015)", function (i) { return valor(i, "concl", 0, ""); }],
    ["NSE (0 a 10)", function (i) { return valor(i, "nse", 2, ""); }],
    ["ICG (1 a 6)", function (i) { return valor(i, "icg", 0, ""); }],
    ["AFD adequada", function (i) { return valor(i, "afd", 1, "%"); }],
    ["IIE (0 a 100)", function (i) { return valor(i, "iie", 1, ""); }],
    ["Renda do entorno (SM)", function (i) { return valor(i, "renda", 2, ""); }],
    ["Pretos, pardos e indígenas", function (i) { return valor(i, "ppi", 1, "%"); }],
    ["Participou do Enem", function (i) { return valor(i, "enem", 1, "%", true); }],
    ["Nota média no Enem", function (i) { return valor(i, "nota", 1, "", true); }],
    ["Ingressou na ES", function (i) { return valor(i, "es", 1, "%", true); }],
    ["ES pública", function (i) { return valor(i, "espub", 1, "%", true); }],
    ["Públicas de maior prestígio", function (i) { return valor(i, "estop", 1, "%", true); }],
    ["50 primeiras do RUF", function (i) { return valor(i, "ruf", 1, "%", true); }]
  ];

  function desenharComparacao() {
    if (!elComp) return;
    var resumo = q(".comp-resumo", elComp), corpo = q(".comp-corpo", elComp);
    resumo.textContent = comparadas.length ? "Comparar escolas (" + comparadas.length + " de " + MAX_COMP + ")" : "Comparar escolas";
    if (!comparadas.length) {
      corpo.innerHTML = '<p class="comp-vazio">Use <b>+</b> na tabela ou <b>+ Comparar</b> na ficha para colocar até ' +
        MAX_COMP + " escolas lado a lado.</p>";
      return;
    }
    var cab = "<tr><th></th>" + comparadas.map(function (id) {
      var i = posicao[id];
      return '<th><div class="comp-nome">' + esc(E.nome[i]) + '</div><button type="button" class="comp-x" data-remover="' + id +
        '" aria-label="Remover da comparação">&times;</button></th>';
    }).join("") + "</tr>";
    var corpoTab = LINHAS_COMP.map(function (l) {
      return "<tr><th>" + l[0] + "</th>" + comparadas.map(function (id) { return "<td>" + l[1](posicao[id]) + "</td>"; }).join("") + "</tr>";
    }).join("");
    corpo.innerHTML = '<div class="comp-rolagem"><table class="tabela-comp">' + cab + corpoTab + "</table></div>" +
      '<button type="button" class="btn-acao" data-limpar="1">Limpar comparação</button>';
  }

  function comparar(id) {
    id = String(id);
    if (posicao[id] === undefined) return;
    if (comparadas.indexOf(id) >= 0) { aviso("Esta escola já está na comparação."); }
    else if (comparadas.length >= MAX_COMP) { aviso("A comparação comporta até " + MAX_COMP + " escolas."); }
    else { comparadas.push(id); desenharComparacao(); aviso("Escola adicionada à comparação."); }
    if (elComp) elComp.open = true;
  }

  if (elComp) {
    elComp.addEventListener("click", function (ev) {
      var r = ev.target.closest("[data-remover]");
      if (r) { comparadas = comparadas.filter(function (x) { return x !== r.getAttribute("data-remover"); }); desenharComparacao(); }
      if (ev.target.closest("[data-limpar]")) { comparadas = []; desenharComparacao(); }
    });
  }

  var tAviso = null;
  function aviso(texto) {
    var a = q("#aviso-explorador");
    if (!a) return;
    a.textContent = texto; a.classList.add("visivel");
    clearTimeout(tAviso);
    tAviso = setTimeout(function () { a.classList.remove("visivel"); }, 2200);
  }

  // 7. Filtros, status e enquadramento ---------------------------------------------------
  var fhTipo = new crosstalk.FilterHandle(dados.grupo_cross);
  var fhArea = new crosstalk.FilterHandle(dados.grupo_cross);
  var fhAtrib = new crosstalk.FilterHandle(dados.grupo_cross);
  var fhStatus = new crosstalk.FilterHandle(dados.grupo_cross);
  var chkAjustar = q("#chk-ajustar"), chkArea = q("#chk-area");
  var areaAtiva = false;

  function atualizarStatus(chaves) {
    var total = 0, noMapa = 0, concl = 0;
    if (chaves === null || chaves === undefined) {
      total = E.id.length;
      for (var k = 0; k < total; k++) {
        concl += E.concl[k] || 0;
        if (E.lat[k] !== null) noMapa++;
      }
    } else {
      total = chaves.length;
      for (var j = 0; j < total; j++) {
        var i = posicao[chaves[j]];
        if (i === undefined) continue;
        concl += E.concl[i] || 0;
        if (E.lat[i] !== null) noMapa++;
      }
    }
    var s = q("#status-escolas");
    if (s) {
      s.innerHTML = "<b>" + total.toLocaleString("pt-BR") + "</b> escolas · <b>" + concl.toLocaleString("pt-BR") +
        "</b> concluintes · " + noMapa.toLocaleString("pt-BR") + " no mapa";
    }
  }

  function enquadrar(chaves) {
    var pts = [];
    var fonte = chaves === null || chaves === undefined ? null : chaves;
    if (fonte === null) { map.flyToBounds(BRASIL, { duration: 0.8 }); return; }
    for (var j = 0; j < fonte.length; j++) {
      var i = posicao[fonte[j]];
      if (i !== undefined && E.lat[i] !== null) pts.push([E.lat[i], E.lon[i]]);
    }
    if (pts.length) map.flyToBounds(L.latLngBounds(pts).pad(0.1), { duration: 0.8, maxZoom: 13 });
  }

  var enquadrarDepois = atraso(function (chaves) { enquadrar(chaves); }, 350);
  var jaFiltrou = false;
  fhStatus.on("change", function (e) {
    atualizarStatus(e.value);
    if (e.value !== null && e.value !== undefined) jaFiltrou = true;
    // Enquadra so depois que o usuario filtrou algo (evita mexer no mapa ao carregar)
    if (!jaFiltrou) return;
    if (e.sender === fhArea || areaAtiva) return;
    if (chkAjustar && chkAjustar.checked) enquadrarDepois(e.value);
  });

  var btnEnq = q("#btn-enquadrar");
  if (btnEnq) btnEnq.addEventListener("click", function () { enquadrar(fhStatus.filteredKeys !== undefined ? fhStatus.filteredKeys : null); });

  function filtrarArea() {
    if (!areaAtiva) return;
    var b = map.getBounds().pad(0.05);
    var chaves = [];
    for (var i = 0; i < E.id.length; i++) {
      if (E.lat[i] !== null && b.contains([E.lat[i], E.lon[i]])) chaves.push(E.id[i]);
    }
    fhArea.set(chaves);
  }
  var filtrarAreaDepois = atraso(filtrarArea, 250);
  map.on("moveend", filtrarAreaDepois);
  if (chkArea) chkArea.addEventListener("change", function () {
    areaAtiva = chkArea.checked;
    if (chkAjustar) { chkAjustar.disabled = areaAtiva; }
    if (areaAtiva) filtrarArea(); else fhArea.clear();
  });

  // Botoes por tipo --------------------------------------------------------------------------
  var chips = qa(".chip-tipo");
  function aplicarChips() {
    var ativos = chips.filter(function (c) { return c.classList.contains("ativo"); })
      .map(function (c) { return c.getAttribute("data-tipo"); });
    if (!ativos.length) { fhTipo.clear(); return; }
    var chaves = [];
    for (var i = 0; i < E.id.length; i++) {
      if (ativos.indexOf(dados.tipos.ordem[E.tipo[i]]) >= 0) chaves.push(E.id[i]);
    }
    fhTipo.set(chaves);
  }
  chips.forEach(function (c) {
    c.addEventListener("click", function () {
      c.classList.toggle("ativo");
      c.setAttribute("aria-pressed", String(c.classList.contains("ativo")));
      aplicarChips();
    });
  });

  // Listas de filtro (territorio e criterios) ----------------------------------------------
  // "Geral" (valor vazio) = sem filtro. As opcoes vem do proprio JSON das escolas;
  // regiao > UF > municipio se restringem em cascata.
  var selCampos = qa(".filtros select[data-campo]");
  var selPor = {};
  selCampos.forEach(function (sel) { selPor[sel.getAttribute("data-campo")] = sel; });
  var ORDEM_REGIAO = ["Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"];

  function unicos(campo, filtro) {
    var m = {};
    for (var i = 0; i < E.id.length; i++) {
      if (filtro && !filtro(i)) continue;
      var v = E[campo][i];
      if (v !== null && v !== undefined && v !== "") m[v] = true;
    }
    return Object.keys(m);
  }
  function ordenar(v, ordem) {
    return v.sort(ordem ? function (a, b) { return ordem.indexOf(a) - ordem.indexOf(b); } : function (a, b) { return a.localeCompare(b, "pt-BR"); });
  }
  function preencher(sel, opcoes, valorAtual) {
    // opcoes: [[valor, rotulo], ...]
    sel.innerHTML = '<option value="">Geral</option>' + opcoes.map(function (o) {
      return '<option value="' + esc(o[0]) + '">' + esc(o[1]) + "</option>";
    }).join("");
    var existe = opcoes.some(function (o) { return o[0] === valorAtual; });
    sel.value = existe ? valorAtual : "";
  }
  function atualizarListasTerritorio() {
    var reg = selPor.reg.value, uf = selPor.uf.value;
    preencher(selPor.uf, ordenar(unicos("uf", function (i) { return !reg || E.reg[i] === reg; })).map(function (u) { return [u, u]; }), uf);
    uf = selPor.uf.value;
    var muns = {};
    for (var i = 0; i < E.id.length; i++) {
      if (reg && E.reg[i] !== reg) continue;
      if (uf && E.uf[i] !== uf) continue;
      muns[E.uf[i] + "|" + E.mun[i]] = true;
    }
    var lista = Object.keys(muns).map(function (k) {
      var par = k.split("|");
      return [k, uf ? par[1] : par[1] + " (" + par[0] + ")"];
    }).sort(function (a, b) { return a[1].localeCompare(b[1], "pt-BR"); });
    preencher(selPor.mun, lista, selPor.mun.value);
  }
  function montarListas() {
    preencher(selPor.reg, ordenar(unicos("reg"), ORDEM_REGIAO).map(function (v) { return [v, v]; }), "");
    ["loc", "rede", "dep", "vinc", "locdif", "estrato"].forEach(function (c) {
      var vals = unicos(c);
      var ord = c === "estrato" ? vals.sort() : ordenar(vals);
      preencher(selPor[c], ord.map(function (v) { return [v, v]; }), "");
    });
    atualizarListasTerritorio();
  }
  function aplicarListas() {
    var f = {};
    var ativo = false;
    Object.keys(selPor).forEach(function (c) { f[c] = selPor[c].value; if (f[c]) ativo = true; });
    if (!ativo) { fhAtrib.clear(); return; }
    var chaves = [];
    for (var i = 0; i < E.id.length; i++) {
      if (f.reg && E.reg[i] !== f.reg) continue;
      if (f.uf && E.uf[i] !== f.uf) continue;
      if (f.mun && (E.uf[i] + "|" + E.mun[i]) !== f.mun) continue;
      if (f.loc && E.loc[i] !== f.loc) continue;
      if (f.rede && E.rede[i] !== f.rede) continue;
      if (f.dep && E.dep[i] !== f.dep) continue;
      if (f.vinc && E.vinc[i] !== f.vinc) continue;
      if (f.locdif && E.locdif[i] !== f.locdif) continue;
      if (f.estrato && E.estrato[i] !== f.estrato) continue;
      chaves.push(E.id[i]);
    }
    fhAtrib.set(chaves);
  }
  selCampos.forEach(function (sel) {
    sel.addEventListener("change", function () {
      var c = sel.getAttribute("data-campo");
      if (c === "reg") { selPor.uf.value = ""; selPor.mun.value = ""; atualizarListasTerritorio(); }
      else if (c === "uf") { selPor.mun.value = ""; atualizarListasTerritorio(); }
      aplicarListas();
    });
  });

  // No celular a barra de filtros comeca recolhida (depois que o crosstalk leu os controles)
  function recolherFiltros() {
    setTimeout(function () {
      var f = q(".filtros");
      if (f && window.innerWidth < 992) f.classList.add("recolhido");
    }, 600);
  }
  var tituloFiltros = q(".fb-toggle");
  if (tituloFiltros) tituloFiltros.addEventListener("click", function () {
    q(".filtros").classList.toggle("recolhido");
    setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 50);   // as faixas recalculam a largura
  });
  if (document.readyState === "complete") recolherFiltros();
  else window.addEventListener("load", recolherFiltros);

  // Limpar todos os filtros ---------------------------------------------------------------------
  function limparFiltros() {
    chips.forEach(function (c) { c.classList.remove("ativo"); c.setAttribute("aria-pressed", "false"); });
    fhTipo.clear();
    if (chkArea && chkArea.checked) { chkArea.checked = false; areaAtiva = false; if (chkAjustar) chkAjustar.disabled = false; fhArea.clear(); }
    selCampos.forEach(function (sel) { sel.value = ""; });
    if (E) atualizarListasTerritorio();
    fhAtrib.clear();
    // Faixas: volta ao intervalo inicial e retira o filtro (que exclui escolas sem o dado)
    qa(".filtros .crosstalk-input-slider").forEach(function (div) {
      var $ = window.jQuery;
      var inp = $(div).find("input");
      var rs = inp.data("ionRangeSlider");
      if (!rs) return;
      inp.data("updating", true);
      rs.update({ from: rs.result.min, to: rs.result.max });
      inp.data("updating", false);
      var inst = $(div).data("crosstalk-instance");
      if (inst) inst.suspend();
    });
  }
  var btnLimpar = q("#btn-limpar");
  if (btnLimpar) btnLimpar.addEventListener("click", limparFiltros);

  // 8. Busca de escolas ----------------------------------------------------------------------
  var inBusca = q("#busca-escola"), lista = q("#sugestoes-escola");
  var indiceBusca = null, ativoSug = -1, sugestoes = [];
  function montarIndice() {
    indiceBusca = new Array(E.id.length);
    for (var i = 0; i < E.id.length; i++) indiceBusca[i] = semAcento(E.nome[i] + " " + E.mun[i] + " " + E.uf[i]);
  }
  function buscar(texto) {
    var termos = semAcento(texto).split(/\s+/).filter(Boolean);
    if (!termos.length) return [];
    var achados = [];
    for (var i = 0; i < indiceBusca.length; i++) {
      var ok = true;
      for (var t = 0; t < termos.length; t++) if (indiceBusca[i].indexOf(termos[t]) < 0) { ok = false; break; }
      if (ok) achados.push(i);
    }
    achados.sort(function (a, b) { return (E.concl[b] || 0) - (E.concl[a] || 0); });
    return achados.slice(0, 8);
  }
  function desenharSugestoes() {
    if (!sugestoes.length) { lista.innerHTML = ""; lista.classList.remove("aberta"); return; }
    lista.innerHTML = sugestoes.map(function (i, k) {
      var cor = dados.tipos.paleta[dados.tipos.ordem[E.tipo[i]]] || "#999";
      return '<li role="option" data-i="' + i + '" class="' + (k === ativoSug ? "ativa" : "") + '">' +
        '<span class="leg-ponto" style="background:' + cor + '"></span><span class="sug-nome">' + esc(E.nome[i]) +
        '</span><span class="sug-local">' + esc(E.mun[i]) + " (" + esc(E.uf[i]) + ")</span></li>";
    }).join("");
    lista.classList.add("aberta");
  }
  function escolherSugestao(i) {
    lista.classList.remove("aberta");
    inBusca.value = E.nome[i];
    abrir(E.id[i], { voar: true });
  }
  if (inBusca) {
    inBusca.addEventListener("input", atraso(function () {
      if (!E) return;
      if (!indiceBusca) montarIndice();
      sugestoes = buscar(inBusca.value); ativoSug = -1; desenharSugestoes();
    }, 120));
    inBusca.addEventListener("keydown", function (ev) {
      if (ev.key === "ArrowDown") { ativoSug = Math.min(sugestoes.length - 1, ativoSug + 1); desenharSugestoes(); ev.preventDefault(); }
      else if (ev.key === "ArrowUp") { ativoSug = Math.max(0, ativoSug - 1); desenharSugestoes(); ev.preventDefault(); }
      else if (ev.key === "Enter" && sugestoes.length) { escolherSugestao(sugestoes[Math.max(0, ativoSug)]); ev.preventDefault(); }
      else if (ev.key === "Escape") { lista.classList.remove("aberta"); }
    });
    lista.addEventListener("mousedown", function (ev) {
      var li = ev.target.closest("li");
      if (li) { escolherSugestao(parseInt(li.getAttribute("data-i"), 10)); ev.preventDefault(); }
    });
    document.addEventListener("click", function (ev) {
      if (!ev.target.closest(".busca-escola")) lista.classList.remove("aberta");
    });
  }

  // 9. Dados das escolas: carregar e ligar tudo -----------------------------------------------
  function iniciar(dadosEscolas) {
    E = dadosEscolas;
    E.id.forEach(function (id, i) { posicao[id] = i; });

    Object.keys(lm._byStamp).forEach(function (stamp) {
      var info = lm._byStamp[stamp];
      if (!info || info.group !== dados.grupo_escolas) return;
      camadas[String(info.layerId)] = info.layer;
    });

    Object.keys(camadas).forEach(function (id) {
      var i = posicao[id];
      if (i === undefined) return;
      var c = camadas[id];
      c.bindTooltip(function () { return esc(E.nome[i]) + "<br><span class='tt-sub'>" + esc(E.mun[i]) + " (" + esc(E.uf[i]) + ")</span>"; },
        { direction: "top", offset: [0, -4] });
      c.on("click", function (ev) { L.DomEvent.stopPropagation(ev); abrir(id, { voar: false }); });
      c.on("mouseover", function () { c.setStyle({ weight: 2, color: "#0f1b2d" }); });
      c.on("mouseout", function () { c.setStyle({ weight: 0.6, color: "#ffffff" }); });
    });

    montarListas();
    pintarEscolas(selEsc.value);
    atualizarStatus(null);
    desenharComparacao();

    window.Escolas = { abrir: abrir, comparar: comparar, fechar: fechar };

    var m = location.hash.match(/escola=(\d+)/);
    if (m && posicao[m[1]] !== undefined) abrir(m[1], { voar: true });
  }

  fetch(dados.url_escolas)
    .then(function (r) { if (!r.ok) throw new Error(r.status); return r.json(); })
    .then(iniciar)
    .catch(function () {
      var s = q("#status-escolas");
      if (s) s.textContent = "Não foi possível carregar os dados das escolas. Abra o site por um servidor (GitHub Pages ou quarto preview).";
    });
}
