let table;

let year = "2015";
let orden = "masfeliz";
let region = "mundo"
datos = d3.csv("2015renovado.csv");
Promise.all([datos])
    .then((data) => {
        let datos = data[0];
        let imprimir = updateOrden(datos, year, orden);
        let top = updateTop(imprimir, region);
        table=d3.select('#lista').append('table');
        tabulate(top,["rank", "name", "score", "region"]);
        
        d3.selectAll("input[name=year]").on("change", function () {
            year = this.value;
            imprimir = updateOrden(datos, year, orden);
            top=updateTop(imprimir,region);
            d3.selectAll("table").remove();
            table=d3.select('#lista').append('table');
            tabulate(top,["rank", "name", "score", "region"]);
        });

        d3.select("#region").on("change", function () {
            region = this.value;
            top = updateTop(imprimir, region);
            d3.selectAll("table").remove();
            table=d3.select('#lista').append('table');
            tabulate(top,["rank", "name", "score", "region"]);
        });

        d3.select("#orden").on("change", function () {
            orden = this.value;
            imprimir = updateOrden(datos, year, orden);
            top = updateTop(imprimir, region);
            d3.selectAll("table").remove();
            table=d3.select('#lista').append('table');
            tabulate(top,["rank", "name", "score", "region"]);
        });

    })
    .catch((e) => {
        alert("Hubo un error al cargar los datos");
        console.error(e);
    });


function updateOrden(datos, year, orden) {
    let newdatos = [];
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
                newdatos[i] = { name: datos[i].Country, rank: datos[i].rank2015, score: datos[i].score2015, region: datos[i].Region };
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
                newdatos[i] = { name: datos[i].Country, rank: datos[i].rank2016, score: datos[i].score2016, region: datos[i].Region };
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
                newdatos[i] = { name: datos[i].Country, rank: datos[i].rank2017, score: datos[i].score2017, region: datos[i].Region };
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
                newdatos[i] = { name: datos[i].Country, rank: datos[i].rank2018, score: datos[i].score2018, region: datos[i].Region };
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
                newdatos[i] = { name: datos[i].Country, rank: datos[i].rank2019, score: datos[i].score2019, region: datos[i].Region };
            }
            break;
    }

    return newdatos;
}
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

function tabulate(data, columns) {
	var thead = table.append('thead');
	var	tbody = table.append('tbody');

	// append the header row
	thead.append('tr')
	  .selectAll('th')
	  .data(columns).enter()
	  .append('th')
	    .text(function (column) { return column; });

	// create a row for each object in the data
	var rows = tbody.selectAll('tr')
	  .data(data)
	  .enter()
	  .append('tr');

	// create a cell in each row for each column
	var cells = rows.selectAll('td')
	  .data(function (row) {
	    return columns.map(function (column) {
	      return {column: column, value: row[column]};
	    });
	  })
	  .enter()
	  .append('td')
	    .text(function (d) { return d.value; });

  return table;
}