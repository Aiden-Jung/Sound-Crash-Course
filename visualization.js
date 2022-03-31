var margin = ({ top: 20, right: 30, bottom: 30, left: 40 }),
  width = width - margin.left - margin.right,
  height = height - margin.top - margin.bottom;

svg.append("rect")
  .attr("width", "100%")
  .attr("height", "100%")
  .attr("fill", "#092b36");

svg
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .style("color", "#859596")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

// Initialise a X axis:
var x = d3.scaleLinear();
var xAxis = d3.axisBottom(x);
svg.select("g").append("g")
  .attr("transform", "translate(0," + height + ")")
  .attr("class", "myXaxis")

// Initialize an Y axis
var y = d3.scaleLinear();
var yAxis = d3.axisLeft(y);
svg.select("g").append("g")
  .attr("class", "myYaxis");

var wave = document.getElementById('playAroundAud');

var call = 0;

Shiny.setInputValue("callback", call);

  call = 1;

  Shiny.setInputValue("callback", call);

var amp, time, rate;

Shiny.addCustomMessageHandler('amp', function (ampr) {
  amp = ampr;
});

Shiny.addCustomMessageHandler('time', function (timer) {
  time = timer/2;
});

Shiny.addCustomMessageHandler('rate', function (rater) {
  rate = rater;
});

var myReq;

function renderChart() {

  myReq=requestAnimationFrame(renderChart);

  var temp;

  if (Math.round(wave.currentTime * rate - rate*time, 2) < 0) {
    temp = r2d3.data.slice(0, Math.round(wave.currentTime * rate + rate*time));
  } else {
    temp = r2d3.data.slice(Math.round(wave.currentTime * rate - rate*time), Math.round(wave.currentTime * rate + rate*time));
  }

  // Create the X axis
  x.domain([wave.currentTime - time, wave.currentTime + time])
    .range([0, width]);

  svg.selectAll(".myXaxis")
    .transition()
    .duration(0)
    .call(xAxis)
    .style("stroke", "#859596");

  // create the Y axis
  y.domain([d3.min(r2d3.data, function (d) { 
    if(d < 0){
      return -10*Math.log10(Math.abs(d))
    }else if (d>0){
      return 10*Math.log10(d)
    }else{
      return d
    }}), d3.max(r2d3.data, function (d) {
      if(d < 0){
        return -10*Math.log10(Math.abs(d))
      }else if (d>0){
        return 10*Math.log10(d)
      }else{
        return d
      }
     })])
    .range([height, 0]);
  svg.selectAll(".myYaxis")
    .transition()
    .duration(0)
    .call(yAxis)
    .style("stroke", "#859596");

  // Create a update selection: bind to the new data
  var u = svg.select("g").selectAll(".lineTest")
    .data([temp], function (d) { return d });

  // Update the line
  u
    .enter()
    .append("path")
    .attr("class", "lineTest")
    .merge(u)
    .transition()
    .duration(0)
    .attr("d", d3.line()
      .x(function (d, i) {
        if(wave.currentTime * rate - rate*time<0){
          return x(i/rate);
        }else{
         return x((wave.currentTime - time) + i / rate);
      }})
      .y(function (d) { 
        if(d < 0){
          return y(-10*Math.log10(Math.abs(d))*amp)
        }else if (d>0){
          return y(10*Math.log10(d)*amp)
        }else{
          return y(d);
        }
       }))
    .attr("fill", "none")
    .attr("stroke", "#b58900")
    .attr("stroke-width", 1)

  u
    .exit()
    .transition()
    .duration(0)
    .remove();

}

renderChart();

r2d3.onRender(function(data, svg, width, height, options) {
  renderChart();
})

//wave.onpause = cancelAnimationFrame(myReq)
/*
r2d3.onRender(function (data, svg, width, height, options) {
  wave.addEventListener("canplay", renderChart, true);
  wave.addEventListener("play", renderChart, true);
  wave.addEventListener("pause", function(){cancelAnimationFrame(myReq)}, true);
})

r2d3.onResize(function(width, height) {
  cancelAnimationFrame(myReq);

  width = width - margin.left - margin.right,
  height = height - margin.top - margin.bottom;

  svg.selectAll('g').remove();
  svg.selectAll('rect').remove();

  svg.append("rect")
  .attr("width", "100%")
  .attr("height", "100%")
  .attr("fill", "#092b36");

svg
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .style("color", "#859596")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

  // Initialise a X axis:
svg.select("g").append("g")
  .attr("transform", "translate(0," + height + ")")
  .attr("class", "myXaxis")

// Initialize an Y axis
svg.select("g").append("g")
  .attr("class", "myYaxis");
});
*/