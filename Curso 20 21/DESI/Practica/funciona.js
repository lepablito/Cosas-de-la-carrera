const WIDTH = 1000;
const HEIGHT = 580;

let svg = d3.select("#grafico")
    .append("svg")
    .attr("width",WIDTH)
    .attr("height",HEIGHT);

let countryName;
let datos = d3.csv("2015renovado.csv");
let mapamundi = d3.json("world.geo.json");
let colors;
let maximo;
let minimo;

Promise.all([datos, mapamundi])
.then((data)=>{
    let datos = data[0];
    let mapamundi = data[1];
    let features = mapamundi.features;
    
    let region = "mundo";
    let year = "2015";

    colors = updateColor(features,datos,year);



    let projection = d3.geoMercator()
    .fitExtent([[0,0],[WIDTH,HEIGHT-70]],mapamundi);
    projection.fitExtent([[0,0],[WIDTH,HEIGHT-65]],mapamundi);
    projection.scale(140);
    let geoPath = d3.geoPath(projection);
    let mapa = svg
    .append("g")

    mapa.selectAll(".country")
        .data(features)
        .enter()
        .append("path")
        .attr("class","country")
        .attr("d", geoPath)
        .style("stroke","black")
        .style("fill",(data)=>{
            if(data.properties.score){
                return colors(data.properties.score);
            }else{
                return "white"
            }
        })
        .on("mouseover",mouseOver);  

    nombrePais = svg.append("text")
    .attr("id","nombre")
    .attr("x","20")
    .attr("y","20")
    .style("font-family","Segoe UI")
    .style("fill","black")
    .style("font-weight","bold")
    .text("");

    Score = svg.append("text")
    .attr("id","score")
    .attr("x","20")
    .attr("y",45)
    .style("font-family","Segoe UI")
    .style("fill","black")
    .style("font-weight","bold")
    .text("");

    Rank = svg.append("text")
    .attr("id","rank")
    .attr("x","20")
    .attr("y","65")
    .style("font-family","Segoe UI")
    .style("fill","black")
    .style("font-weight","bold")
    .text("");

    Mini = svg.append("text")
    .attr("id","mini")
    .attr("x","5")
    .attr("y","548")
    .style("font-family","Segoe UI")
    .style("font-size", "small")
    .style("fill","black")
    .text(minimo.toFixed(2));

    Maxi = svg.append("text")
    .attr("id","maxi")
    .attr("x","205")
    .attr("y","548")
    .style("font-family","Segoe UI")
    .style("font-size", "small")
    .style("fill","black")
    .text(maximo.toFixed(2));


    //Append a defs (for definition) element to your SVG
    var defs = svg.append("defs");
    //Append a linearGradient element to the defs and give it a unique id
    let linearGradient = defs.append("linearGradient")
    .attr("id", "linear-gradient");
    //Set the color for the start (0%)
    linearGradient.append("stop")
    .attr("offset", 0)
    .attr("stop-color", "blue");
    //Set the color for the end (100%)
    linearGradient.append("stop")
    .attr("offset", maximo)
    .attr("stop-color", "yellow");
    //Draw the rectangle and fill with gradient
    svg.append("rect")
    .attr("x",20)
    .attr("y",550)
    .attr("width", 200)
    .attr("height", 20)
    .style("fill", "url(#linear-gradient)");


    d3.selectAll("input[name=year]").on("change",function(){
        year = this.value;
        colors=updateColor(features,datos,year);
        Mini.text(minimo.toFixed(2));
        Maxi.text(maximo.toFixed(2));
        d3.selectAll(".country")
        .style("fill",cambiaColor);
    });

    d3.select("#region").on("change",function(){
        region = this.value;
        console.log(region);
        if (region=="mundo"){
            projection.scale(140);
            projection.center([0,0]);
            geoPath = d3.geoPath(projection);
            svg.selectAll("path")
            .transition() 
            .attr("d", geoPath); 
        }
        else if(region=="europe"){
            projection.scale(620);
            projection.center([0,50]);
            geoPath = d3.geoPath(projection);
            svg.selectAll("path") 
            .transition() 
            .attr("d", geoPath); 
        }
        else if(region=="northamerica"){
            projection.scale(300);
            projection.center([-130,45]);
            geoPath = d3.geoPath(projection);
            svg.selectAll("path")  
            .transition()
            .attr("d", geoPath); 
        }
        else if(region=="southamerica"){
            projection.scale(350);
            projection.center([-80,-40]);
            geoPath = d3.geoPath(projection);
            svg.selectAll("path")
            .transition()  
            .attr("d", geoPath); 
        }
        else if(region=="asia"){
            projection.scale(400);
            projection.center([100,25]);
            geoPath = d3.geoPath(projection);
            svg.selectAll("path") 
            .transition()
            .attr("d", geoPath); 
        }
        else if(region=="africa"){
            projection.scale(400);
            projection.center([10,-12]);
            geoPath = d3.geoPath(projection);
            svg.selectAll("path")
            .transition()  
            .attr("d", geoPath); 
        }else if(region=="oceania"){
            projection.scale(580);
            projection.center([140,-40]);
            geoPath = d3.geoPath(projection);
            svg.selectAll("path")
            .transition()  
            .attr("d", geoPath); 
        }
    });
})
.catch((e)=>{
    alert("Hubo un error al cargar los datos");
    console.error(e);
});

function cambiaColor(data){
    if(data.properties.score){
        return colors(data.properties.score);
    }
    else{
        return "white"
    }
}

function clone(selector) {
    var node = d3.select(selector).node();
    return d3.select(node.parentNode.appendChild(node.cloneNode(true), node.nextSibling));
}

function mouseOver(data){
    d3.select(".currentCountry").remove();
    let nombre = data.properties.name;
    let score = data.properties.score;
    let rank = data.properties.rank;

    nombrePais.text(nombre);
    if(data.properties.score){
        Score.text("Puntuación: "+score+" sobre 10");
        Rank.text("Posición ranking: "+rank);
    }else{
        Score.text("Puntuación: NA");
        Rank.text("Posición ranking: NA");
    }
    let pais = clone(this);
    pais
        .attr("class","currentCountry")
        .style("stroke","yellow")
        .on("mouseout",function(){
            d3.select(this).remove();
        });
}

function updateColor(features,datos,year){
    minScore=10;
    maxScore=0;
    for(let i=0;i<features.length;i++){
        for(let j=0;j<datos.length;j++){
            if(features[i].properties.iso_a3 == datos[j].ISO){
                if(year=="2015"){
                    if(parseFloat(datos[j].score2015) > maxScore){
                        maxScore = parseFloat(datos[j].score2015);
                    }
                    if(parseFloat(datos[j].score2015) < minScore){
                        minScore = parseFloat(datos[j].score2015);
                    }
                    features[i].properties.score = parseFloat(datos[j].score2015);
                }
                else if(year=="2016"){
                    if(parseFloat(datos[j].score2016) > maxScore){
                        maxScore = parseFloat(datos[j].score2016);
                    }
                    if(parseFloat(datos[j].score2016) < minScore){
                        minScore = parseFloat(datos[j].score2016);
                    }
                    features[i].properties.score = parseFloat(datos[j].score2016);
                }
                else if(year=="2017"){
                    if(parseFloat(datos[j].score2017) > maxScore){
                        maxScore = parseFloat(datos[j].score2017);
                    }   
                    if(parseFloat(datos[j].score2017) < minScore){
                        minScore = parseFloat(datos[j].score2017);
                    }                     
                    features[i].properties.score = parseFloat(datos[j].score2017);
                }
                else if(year=="2018"){
                    if(parseFloat(datos[j].score2018) > maxScore){
                        maxScore = parseFloat(datos[j].score2018);
                    }
                    if(parseFloat(datos[j].score2018)< minScore){
                        minScore = parseFloat(datos[j].score2018);
                    }
                    features[i].properties.score = parseFloat(datos[j].score2018);
                }
                else{
                    if(parseFloat(datos[j].score2019) > maxScore){
                        maxScore = parseFloat(datos[j].score2019);
                    }
                    if(parseFloat(datos[j].score2019)< minScore){
                        minScore = parseFloat(datos[j].score2019);
                    }
                    features[i].properties.score = parseFloat(datos[j].score2019);
                }

            }
        }
    }
    for(let i=0;i<features.length;i++){
        for(let j=0;j<datos.length;j++){
            if(features[i].properties.iso_a3 == datos[j].ISO){
                if(year=="2015"){
                    features[i].properties.rank = parseFloat(datos[j].rank2015);
                }
                else if(year=="2016"){
                    if(parseFloat(datos[j].score2016) > maxScore){
                        maxScore = parseFloat(datos[j].score2016);
                    }
                    features[i].properties.rank = parseFloat(datos[j].rank2016);
                }
                else if(year=="2017"){
                    if(parseFloat(datos[j].score2017) > maxScore){
                        maxScore = parseFloat(datos[j].score2017);
                    }                    
                    features[i].properties.rank = parseFloat(datos[j].rank2017);
                }
                else if(year=="2018"){
                    if(parseFloat(datos[j].score2018) > maxScore){
                        maxScore = parseFloat(datos[j].score2018);
                    }
                    features[i].properties.rank = parseFloat(datos[j].rank2018);
                }
                else{
                    if(parseFloat(datos[j].score2019) > maxScore){
                        maxScore = parseFloat(datos[j].score2019);
                    }
                    features[i].properties.rank = parseFloat(datos[j].rank2019);
                }

            }
        }
    }

let color = d3.scaleLinear().domain([minScore,maxScore]).range(['Blue', 'yellow']);
console.log(minScore);
maximo=maxScore;
minimo=minScore;

return color;
}