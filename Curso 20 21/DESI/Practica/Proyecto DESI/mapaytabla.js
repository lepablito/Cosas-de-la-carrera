const WIDTH = 1000;
const HEIGHT = 550;

//Creamos SVG donde pintaremos el grafico
let svg = d3.select("#grafico")
    .append("svg")
    .attr("width", WIDTH)
    .attr("height", HEIGHT);

//Leemos los datos
let datos = d3.csv("datos.csv");
let mapamundi = d3.json("world.geo.json");

//Variables globales
let colors;
let tooltip;
let table;
let maximo;

Promise.all([datos, mapamundi])
    .then((data) => {
        let datos = data[0];
        let mapamundi = data[1];
        let features = mapamundi.features;

        let region = "mundo";
        let year = "2015";
        let orden = "masfeliz";
        //Vemos que color corresponden a cada país
        colors = updateColor(features, datos, year);
        //Calculamos el ranking y creamos la tabla
        let imprimir = updateOrden(datos, year, orden);
        let top = updateTop(imprimir, region);
        table = d3.select('#lista').append('table');
        tabulate(top, ["rank", "name", "score", "region"]);
        //Creamos el mapa
        let projection = d3.geoMercator()
            .fitExtent([[0, 0], [WIDTH, HEIGHT - 70]], mapamundi);
        projection.fitExtent([[0, 0], [WIDTH, HEIGHT - 65]], mapamundi);
        projection.scale(140);
        let geoPath = d3.geoPath(projection);
        let mapa = svg
            .append("g")

        mapa.selectAll(".country")
            .data(features)
            .enter()
            .append("path")
            .attr("class", "country")
            .attr("d", geoPath)
            .style("fill", (data) => {
                if (data.properties.score) {
                    return colors(data.properties.score);
                } else {
                    return "#dde"
                }
            })
            .on("mouseover", mouseOver)
            .on('mouseout', mouseOut);
        //Texto para poner en el ranking la region y el año
        rankingdatos = d3.select("#seleccion").append("text")
            .attr("id", "rankingdatos")
            .style("font-family", "Segoe UI")
            .style("fill", "black")
            .style("font-weight", "bold")
            .text(region + ", " + year);
        //Desplegable para mostrar nombre pais, puntuación y posicion ranking
        tooltip = d3.select("#grafico")
            .append("div")
            .style("position", "absolute")
            .style("visibility", "hidden")
            .style("padding", "8px")
            .style("background-color", "#bc3a3a")
            .style("opacity", "1")
            .style("stroke", "black")
            .style("border-style", "solid")
            .style("border-width", "1px")
            .style("text-align", "center")
            .style("font-size", "small")
            .style("font-family", "Segoe UI")
            .text("");
        //Para la leyenda
        Mini = svg.append("text")
            .attr("id", "mini")
            .attr("x", "10")
            .attr("y", "528")
            .style("font-family", "Segoe UI")
            .style("font-size", "small")
            .style("fill", "black")
            .text("2.5");
        Maxi = svg.append("text")
            .attr("id", "maxi")
            .attr("x", "205")
            .attr("y", "528")
            .style("font-family", "Segoe UI")
            .style("font-size", "small")
            .style("fill", "black")
            .text(maximo.toFixed(2));

        //Leyenda con el gradiente de color
        let defs = svg.append("defs");
        let linearGradient = defs.append("linearGradient")
            .attr("id", "linear-gradient");
        linearGradient.append("stop")
            .attr("offset", 0)
            .attr("stop-color", "blue");
        linearGradient.append("stop")
            .attr("offset", maximo)
            .attr("stop-color", "yellow");
        svg.append("rect")
            .attr("x", 20)
            .attr("y", 530)
            .attr("width", 200)
            .attr("height", 20)
            .style("fill", "url(#linear-gradient)");

        //Cuando cambia de año
        d3.selectAll("input[name=year]").on("change", function () {
            year = this.value;
            colors = updateColor(features, datos, year);
            imprimir = updateOrden(datos, year, orden);
            top = updateTop(imprimir, region);
            d3.selectAll("table").remove();
            table = d3.select('#lista').append('table');
            tabulate(top, ["rank", "name", "score", "region"]);
            rankingdatos.text(region + ", " + year);
            Maxi.text(maximo.toFixed(2));
            d3.selectAll(".country")
                .transition()
                .style("fill", cambiaColor);
        });
        //Cuando cambia el orden
        d3.select("#orden").on("change", function () {
            orden = this.value;
            imprimir = updateOrden(datos, year, orden);
            top = updateTop(imprimir, region);
            d3.selectAll("table").remove();
            table = d3.select('#lista').append('table');
            tabulate(top, ["rank", "name", "score", "region"]);
        });
        //Cuando cambia la region
        d3.select("#region").on("change", function () {
            region = this.value;
            rankingdatos.text(region + ", " + year);
            imprimir = updateOrden(datos, year, orden);
            top = updateTop(imprimir, region);
            d3.selectAll("table").remove();
            table = d3.select('#lista').append('table');
            tabulate(top, ["rank", "name", "score", "region"]);
            switch (region) {
                case "mundo":
                    projection.scale(140);
                    projection.center([0, 0]);
                    geoPath = d3.geoPath(projection);
                    svg.selectAll("path")
                        .transition()
                        .attr("d", geoPath);
                    break;
                case "europe":
                    projection.scale(580);
                    projection.center([20, 49]);
                    geoPath = d3.geoPath(projection);
                    svg.selectAll("path")
                        .transition()
                        .attr("d", geoPath);
                    break;
                case "northamerica":
                    projection.scale(400);
                    projection.center([-120, 47]);
                    geoPath = d3.geoPath(projection);
                    svg.selectAll("path")
                        .transition()
                        .attr("d", geoPath);
                    break;
                case "southamerica":
                    projection.scale(320);
                    projection.center([-100, -30]);
                    geoPath = d3.geoPath(projection);
                    svg.selectAll("path")
                        .transition()
                        .attr("d", geoPath);
                    break;
                case "asia":
                    projection.scale(380);
                    projection.center([120, 25]);
                    geoPath = d3.geoPath(projection);
                    svg.selectAll("path")
                        .transition()
                        .attr("d", geoPath);
                    break;
                case "africa":
                    projection.scale(400);
                    projection.center([10, -7]);
                    geoPath = d3.geoPath(projection);
                    svg.selectAll("path")
                        .transition()
                        .attr("d", geoPath);
                    break;
                case "oceania":
                    projection.scale(580);
                    projection.center([140, -40]);
                    geoPath = d3.geoPath(projection);
                    svg.selectAll("path")
                        .transition()
                        .attr("d", geoPath);
                    break;
            }
        });
    })
    .catch((e) => {
        alert("Hubo un error al cargar los datos");
        console.error(e);
    });
//Cambiamos el color del que se pinta cada pais del mapa
function cambiaColor(data) {
    if (data.properties.score) {
        return colors(data.properties.score);
    }
    else {
        return "#dde";
    }
}
//Al pasar el raton sobre un pais se despliega tooltip
function mouseOver(data) {
    let nombre = data.properties.name;
    let score = data.properties.score;
    let rank = data.properties.rank;
    if (data.properties.score) {
        tooltip.html("<big><b><ins>" + nombre + "</ins></b></big>"
            + "<br>Score: " + score
            + "<br>Rank: " + rank);
    } else {
        tooltip.html("<big><b><ins>" + nombre + "</ins></b></big>"
            + "<br>Score: NA <br>Rank: NA");
    }

    tooltip.style("top", (d3.event.pageY + 10) + "px")
    tooltip.style("left", (d3.event.pageX - 10) + "px")
    tooltip.style("background-color", "white");
    tooltip.style("visibility", "visible");
    d3.select(this).style("stroke", "red");
}

function mouseOut(d) {
    tooltip.style("visibility", "hidden");
    d3.select(this).style("stroke", "none");
}

//Asociamos los datos con el mapa y vemos que color corresponde a cada pais
function updateColor(features, datos, year) {
    maxScore = 0;
    for (let i = 0; i < features.length; i++) {
        for (let j = 0; j < datos.length; j++) {
            if (features[i].properties.iso_a3 == datos[j].ISO) {
                switch (year) {
                    case "2015":
                        if (parseFloat(datos[j].score2015) > maxScore) {
                            maxScore = parseFloat(datos[j].score2015);
                        }
                        features[i].properties.score = parseFloat(datos[j].score2015);
                        break;
                    case "2016":
                        if (parseFloat(datos[j].score2016) > maxScore) {
                            maxScore = parseFloat(datos[j].score2016);
                        }
                        features[i].properties.score = parseFloat(datos[j].score2016);
                        break;
                    case "2017":
                        if (parseFloat(datos[j].score2017) > maxScore) {
                            maxScore = parseFloat(datos[j].score2017);
                        }
                        features[i].properties.score = parseFloat(datos[j].score2017);
                        break;
                    case "2018":
                        if (parseFloat(datos[j].score2018) > maxScore) {
                            maxScore = parseFloat(datos[j].score2018);
                        }
                        features[i].properties.score = parseFloat(datos[j].score2018);
                        break;
                    case "2019":
                        if (parseFloat(datos[j].score2019) > maxScore) {
                            maxScore = parseFloat(datos[j].score2019);
                        }
                        features[i].properties.score = parseFloat(datos[j].score2019);
                        break;
                }
            }
        }
    }
    for (let i = 0; i < features.length; i++) {
        for (let j = 0; j < datos.length; j++) {
            if (features[i].properties.iso_a3 == datos[j].ISO) {
                switch (year) {
                    case "2015":
                        features[i].properties.rank = parseFloat(datos[j].rank2015);
                        break;
                    case "2016":
                        features[i].properties.rank = parseFloat(datos[j].rank2016);
                        break;
                    case "2017":
                        features[i].properties.rank = parseFloat(datos[j].rank2017);
                        break;
                    case "2018":
                        features[i].properties.rank = parseFloat(datos[j].rank2018);
                        break;
                    case "2019":
                        features[i].properties.rank = parseFloat(datos[j].rank2019);
                        break;
                }
            }
        }
    }

    let color = d3.scaleLinear().domain([2.5, maxScore]).range(["Blue", 'yellow']);
    maximo = maxScore;

    return color;
}

//Obtenemos el orden del ranking
function updateOrden(datos, year, orden) {
    let newdatos = [];
    let j = 0;
    switch (year) {
        case "2015":
            datos = [].slice.call(datos).sort(function (a, b) {
                if (isNaN(a.rank2015) && isNaN(b.rank2015)) {
                    return 0;
                }
                if (isNaN(a.rank2015)) {
                    return 1;
                }
                if (isNaN(b.rank2015)) {
                    return -1;
                }
                return a.rank2015 - b.rank2015;
            });
            if (orden == "menosfeliz") {
                datos = [].slice.call(datos).reverse();
            }
            for (i = 0; i < datos.length; i++) {
                if (!isNaN(datos[i].score2015)) {
                    newdatos[j] = { name: datos[i].Country, rank: datos[i].rank2015, score: datos[i].score2015, region: datos[i].Region };
                    j++;
                }
            }
            break;
        case "2016":
            datos = [].slice.call(datos).sort(function (a, b) {
                if (isNaN(a.rank2016) && isNaN(b.rank2016)) {
                    return 0;
                }
                if (isNaN(a.rank2016)) {
                    return 1;
                }
                if (isNaN(b.rank2016)) {
                    return -1;
                }
                return a.rank2016 - b.rank2016;
            });
            if (orden == "menosfeliz") {
                datos = [].slice.call(datos).reverse();
            }
            for (i = 0; i < datos.length; i++) {
                if (!isNaN(datos[i].score2016)) {
                    newdatos[j] = { name: datos[i].Country, rank: datos[i].rank2016, score: datos[i].score2016, region: datos[i].Region };
                    j++;
                }
            }
            break;
        case "2017":
            datos = [].slice.call(datos).sort(function (a, b) {
                if (isNaN(a.rank2017) && isNaN(b.rank2017)) {
                    return 0;
                }
                if (isNaN(a.rank2017)) {
                    return 1;
                }
                if (isNaN(b.rank2017)) {
                    return -1;
                }
                return a.rank2017 - b.rank2017;
            });
            if (orden == "menosfeliz") {
                datos = [].slice.call(datos).reverse();
            }
            for (i = 0; i < datos.length; i++) {
                if (!isNaN(datos[i].score2017)) {
                    newdatos[j] = { name: datos[i].Country, rank: datos[i].rank2017, score: datos[i].score2017, region: datos[i].Region };
                    j++;
                }
            }
            break;
        case "2018":
            datos = [].slice.call(datos).sort(function (a, b) {
                if (isNaN(a.rank2018) && isNaN(b.rank2018)) {
                    return 0;
                }
                if (isNaN(a.rank2018)) {
                    return 1;
                }
                if (isNaN(b.rank2018)) {
                    return -1;
                }
                return a.rank2018 - b.rank2018;
            });
            if (orden == "menosfeliz") {
                datos = [].slice.call(datos).reverse();
            }
            for (i = 0; i < datos.length; i++) {
                if (!isNaN(datos[i].score2018)) {
                    newdatos[j] = { name: datos[i].Country, rank: datos[i].rank2018, score: datos[i].score2018, region: datos[i].Region };
                    j++;
                }
            }
            break;
        case "2019":
            datos = [].slice.call(datos).sort(function (a, b) {
                if (isNaN(a.rank2019) && isNaN(b.rank2019)) {
                    return 0;
                }
                if (isNaN(a.rank2019)) {
                    return 1;
                }
                if (isNaN(b.rank2019)) {
                    return -1;
                }
                return a.rank2019 - b.rank2019;
            });
            if (orden == "menosfeliz") {
                datos = [].slice.call(datos).reverse();
            }
            for (i = 0; i < datos.length; i++) {
                if (!isNaN(datos[i].score2019)) {
                    newdatos[j] = { name: datos[i].Country, rank: datos[i].rank2019, score: datos[i].score2019, region: datos[i].Region };
                    j++;
                }
            }
            break;
    }

    return newdatos;
}
//Mostramos el ranking por regiones
function updateTop(datos, region) {
    let top = [];
    let j = 0;
    switch (region) {
        case "mundo":
            for (i = 0; i < datos.length; i++) {
                top[i] = datos[i];
            }
            break;
        case "europe":
            j = 0;
            for (i = 0; i < datos.length; i++) {
                if (datos[i].region == "Western Europe" || datos[i].region == "Central and Eastern Europe") {
                    top[j] = datos[i];
                    j++;
                }
            }
            break;
        case "northamerica":
            j = 0;
            for (i = 0; i < datos.length; i++) {
                if (datos[i].region == "North America") {
                    top[j] = datos[i];
                    j++;
                }
            }
            break;
        case "southamerica":
            j = 0;
            for (i = 0; i < datos.length; i++) {
                if (datos[i].region == "Latin America and Caribbean") {
                    top[j] = datos[i];
                    j++;
                }
            }
            break;
        case "africa":
            j = 0;
            for (i = 0; i < datos.length; i++) {
                if (datos[i].region == "Sub-Saharan Africa" || datos[i].region == "Middle East and Northern Africa") {
                    top[j] = datos[i];
                    j++;
                }
            }
            break;
        case "asia":
            j = 0;
            for (i = 0; i < datos.length; i++) {
                if (datos[i].region == "Southern Asia" || datos[i].region == "Southeastern Asia" || datos[i].region == "Eastern Asia") {
                    top[j] = datos[i];
                    j++;
                }
            }
            break;
        case "oceania":
            j = 0;
            for (i = 0; i < datos.length; i++) {
                if (datos[i].region == "Australia and New Zealand") {
                    top[j] = datos[i];
                    j++;
                }
            }
            break;
    }
    return top;
}
//Creamos la tabla para el ranking
function tabulate(data, columns) {
    let thead = table.append('thead');
    let tbody = table.append('tbody');

    //Pone la cabecera
    thead.append('tr')
        .selectAll('th')
        .data(columns).enter()
        .append('th')
        .text(function (column) { return column; });

    //Crea una fila para cada objeto de los datos
    let rows = tbody.selectAll('tr')
        .data(data)
        .enter()
        .append('tr')
        .on("mouseover", function (d) {
            d3.select(this)
                .style("background-color", "yellow");
        })
        .on("mouseout", function (d) {
            d3.select(this)
                .style("background-color", "transparent");
        });

    //Crea una celda para cada dato
    let cells = rows.selectAll('td')
        .data(function (row) {
            return columns.map(function (column) {
                return { column: column, value: row[column] };
            });
        })
        .enter()
        .append('td')
        .text(function (d) { return d.value; });

    return table;
}