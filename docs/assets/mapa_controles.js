// Controles do mapa da tipologia (chamado por htmlwidgets::onRender em escolas.qmd).
// 1) carrega as areas de ponderacao de um GeoJSON externo e as colore pela
//    variavel escolhida do Censo 2010; 2) muda a cor dos pontos das escolas;
// 3) monta popups e rotulos das escolas sob demanda; 4) aproxima o mapa das
//    escolas selecionadas na tabela.
// `dados` vem de R:
//   grupo_escolas: nome do grupo das escolas no leaflet
//   ap: { url, inicial, variaveis: [{id, rotulo, fonte, cortes, cores, rotulos}] }
//   escolas: { ids, tipos, paleta, ordem, campos: {nome, municipio, uf, ...},
//              variaveis: [{id, rotulo, cortes, cores, rotulos, valores}] }
function(el, x, dados) {
  var map = this;
  var lm = map.layerManager;
  var SEM_DADO = "#d9d9d9";

  function fmt(v, casas, sufixo) {
    if (v === null || v === undefined || isNaN(v)) return "n.d.";
    return v.toLocaleString("pt-BR", { minimumFractionDigits: casas, maximumFractionDigits: casas }) +
      (sufixo || "");
  }

  function classe(valor, cortes) {
    if (valor === null || valor === undefined || isNaN(valor)) return -1;
    for (var i = 0; i < cortes.length; i++) {
      if (valor <= cortes[i]) return i;
    }
    return cortes.length;
  }

  function variavel(lista, id) {
    return lista.filter(function (d) { return d.id === id; })[0];
  }

  // 1. Schools: index layers, lazy popups and tooltips ------------------------------
  var camadasEscolas = {};
  var posicao = {};
  dados.escolas.ids.forEach(function (id, i) { posicao[String(id)] = i; });

  Object.keys(lm._byStamp).forEach(function (stamp) {
    var info = lm._byStamp[stamp];
    if (!info || info.group !== dados.grupo_escolas) return;
    var id = String(info.layerId);
    camadasEscolas[id] = info.layer;
  });

  var c = dados.escolas.campos;
  function popupEscola(i) {
    var tipo = dados.escolas.tipos[i];
    var linhas = [
      ["Concluintes (2015)", fmt(c.concluintes[i], 0)],
      ["NSE", fmt(c.nse[i], 1)],
      ["Renda do entorno", fmt(c.renda_entorno[i], 2, " SM")],
      ["Participou do Enem", fmt(c.p_enem[i], 0, "%")],
      ["Ingresso no ensino superior", fmt(c.p_es[i], 0, "%")],
      ["Ingresso na ES pública", fmt(c.p_es_pub[i], 0, "%")]
    ];
    var html = '<div class="popup-escola"><b>' + c.nome[i] + '</b><br>' +
      (c.municipio[i] || "") + " (" + c.uf[i] + ")<br>" +
      '<span style="color:' + (dados.escolas.paleta[tipo] || "#999") + ';font-weight:700">&#9679;</span> ' +
      tipo + '<table>';
    linhas.forEach(function (l) { html += "<tr><td>" + l[0] + "</td><td>" + l[1] + "</td></tr>"; });
    if (c.suprimida[i]) html += '<tr><td colspan="2" class="suprimido">Resultados suprimidos: menos de 10 concluintes</td></tr>';
    return html + "</table></div>";
  }

  Object.keys(camadasEscolas).forEach(function (id) {
    var i = posicao[id];
    if (i === undefined) return;
    var camada = camadasEscolas[id];
    camada.bindPopup(function () { return popupEscola(i); }, { maxWidth: 280 });
    camada.bindTooltip(function () { return c.nome[i]; }, { direction: "top", offset: [0, -4] });
  });

  function pintarEscolas(idVar) {
    if (idVar === "tipologia") {
      Object.keys(camadasEscolas).forEach(function (id) {
        var i = posicao[id];
        camadasEscolas[id].setStyle({ fillColor: dados.escolas.paleta[dados.escolas.tipos[i]] || SEM_DADO });
      });
      return;
    }
    var v = variavel(dados.escolas.variaveis, idVar);
    Object.keys(camadasEscolas).forEach(function (id) {
      var k = classe(v.valores[posicao[id]], v.cortes);
      camadasEscolas[id].setStyle({ fillColor: k < 0 ? SEM_DADO : v.cores[k] });
    });
  }

  function legendaEscolas(idVar) {
    var html = "";
    if (idVar === "tipologia") {
      html += '<div class="leg-titulo">Tipo escolar</div>';
      dados.escolas.ordem.forEach(function (tipo) {
        html += '<div class="leg-item"><span class="leg-ponto" style="background:' +
          dados.escolas.paleta[tipo] + '"></span>' + tipo + '</div>';
      });
      return html;
    }
    var v = variavel(dados.escolas.variaveis, idVar);
    html += '<div class="leg-titulo">' + v.rotulo + '</div>';
    v.cores.forEach(function (cor, i) {
      html += '<div class="leg-item"><span class="leg-ponto" style="background:' + cor +
        '"></span>' + v.rotulos[i] + '</div>';
    });
    html += '<div class="leg-item"><span class="leg-ponto" style="background:' + SEM_DADO +
      '"></span>suprimido ou sem dado</div>';
    return html;
  }

  // 2. Weighting areas from an external GeoJSON -------------------------------------
  map.createPane("areas");
  map.getPane("areas").style.zIndex = 350;
  var rendererAreas = L.canvas({ pane: "areas", padding: 0.3 });
  var camadaAp = null;
  var varAp = dados.ap.inicial;

  function corAp(props) {
    var v = variavel(dados.ap.variaveis, varAp);
    var k = classe(props[varAp], v.cortes);
    return k < 0 ? SEM_DADO : v.cores[k];
  }

  function rotuloAp(p) {
    return "<b>" + (p.mun || "") + (p.uf ? " (" + p.uf + ")" : "") + "</b><br>" +
      "Área de ponderação " + p.cod + "<br>" +
      "Renda per capita: " + fmt(p.renda, 2, " SM") + "<br>" +
      "Densidade: " + fmt(p.densidade, 0, " hab./km²") + "<br>" +
      "Escolas de Ensino Médio: " + (p.esc || 0);
  }

  function legendaAp(idVar) {
    if (idVar === "nenhuma") return "";
    var v = variavel(dados.ap.variaveis, idVar);
    var html = '<div class="leg-titulo">' + v.rotulo + '</div>';
    v.cores.forEach(function (cor, i) {
      html += '<div class="leg-item"><span class="leg-cor" style="background:' + cor +
        '"></span>' + v.rotulos[i] + '</div>';
    });
    html += '<div class="leg-item"><span class="leg-cor" style="background:' + SEM_DADO +
      '"></span>sem informação</div>';
    html += '<div class="leg-fonte">' + v.fonte + '</div>';
    return html;
  }

  function pintarAp(idVar) {
    if (!camadaAp) return;
    if (idVar === "nenhuma") {
      if (map.hasLayer(camadaAp)) map.removeLayer(camadaAp);
      return;
    }
    varAp = idVar;
    if (!map.hasLayer(camadaAp)) map.addLayer(camadaAp);
    camadaAp.setStyle(function (f) { return { fillColor: corAp(f.properties) }; });
  }

  // 3. Control panel -------------------------------------------------------------------
  var opcoesAp = '<option value="nenhuma">Não colorir</option>' +
    dados.ap.variaveis.map(function (v) {
      return '<option value="' + v.id + '">' + v.rotulo + '</option>';
    }).join("");
  var opcoesEsc = '<option value="tipologia">Tipo escolar</option>' +
    dados.escolas.variaveis.map(function (v) {
      return '<option value="' + v.id + '">' + v.rotulo + '</option>';
    }).join("");

  var Controle = L.Control.extend({
    options: { position: "topright" },
    onAdd: function () {
      var div = L.DomUtil.create("div", "controle-mapa");
      div.innerHTML =
        '<label>Cor das escolas</label>' +
        '<select class="sel-esc">' + opcoesEsc + '</select>' +
        '<label>Áreas de ponderação (Censo 2010)</label>' +
        '<select class="sel-ap">' + opcoesAp + '</select>' +
        '<details open><summary style="cursor:pointer;font-weight:600">Legenda</summary>' +
        '<div class="leg-esc"></div><div class="leg-ap"><div class="leg-fonte">Carregando áreas de ponderação...</div></div></details>';
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    }
  });
  map.addControl(new Controle());

  var selEsc = el.querySelector(".sel-esc");
  var selAp = el.querySelector(".sel-ap");
  var legEsc = el.querySelector(".leg-esc");
  var legAp = el.querySelector(".leg-ap");
  selEsc.value = "tipologia";
  selAp.value = dados.ap.inicial;
  legEsc.innerHTML = legendaEscolas("tipologia");

  selEsc.addEventListener("change", function () {
    pintarEscolas(selEsc.value);
    legEsc.innerHTML = legendaEscolas(selEsc.value);
  });
  selAp.addEventListener("change", function () {
    pintarAp(selAp.value);
    legAp.innerHTML = legendaAp(selAp.value);
  });

  fetch(dados.ap.url)
    .then(function (r) {
      if (!r.ok) throw new Error(r.status);
      return r.json();
    })
    .then(function (gj) {
      camadaAp = L.geoJSON(gj, {
        pane: "areas",
        renderer: rendererAreas,
        style: function (f) {
          return { fillColor: corAp(f.properties), fillOpacity: 0.62, color: "#ffffff",
                   weight: 0.4, opacity: 0.8 };
        },
        onEachFeature: function (f, camada) {
          camada.bindTooltip(function () { return rotuloAp(f.properties); }, { sticky: true });
          camada.on("mouseover", function () { camada.setStyle({ weight: 1.6, color: "#0f1b2d" }); });
          camada.on("mouseout", function () { camada.setStyle({ weight: 0.4, color: "#ffffff" }); });
        }
      });
      if (selAp.value !== "nenhuma") {
        varAp = selAp.value;
        camadaAp.addTo(map);
        pintarAp(varAp);
      }
      legAp.innerHTML = legendaAp(selAp.value);
    })
    .catch(function () {
      legAp.innerHTML = '<div class="leg-fonte">Não foi possível carregar as áreas de ponderação. ' +
        'Abra o site por um servidor (GitHub Pages ou quarto preview), e não pelo arquivo local.</div>';
    });

  // 4. Zoom to the schools selected in the table ----------------------------------------
  if (window.crosstalk) {
    var selecao = new crosstalk.SelectionHandle("escolas");
    selecao.on("change", function (e) {
      if (!e.value || !e.value.length) return;
      var pontos = [];
      e.value.forEach(function (id) {
        var camada = camadasEscolas[String(id)];
        if (camada && camada.getLatLng) pontos.push(camada.getLatLng());
      });
      if (pontos.length === 1) {
        map.flyTo(pontos[0], 14, { duration: 0.8 });
        setTimeout(function () { camadasEscolas[String(e.value[0])].openPopup(); }, 900);
      } else if (pontos.length > 1) {
        map.flyToBounds(L.latLngBounds(pontos).pad(0.2), { duration: 0.8, maxZoom: 14 });
      }
    });
  }
}
