/**
 * D3 v7 – Slowest‑Scanner CSV explorer
 * -------------------------------------------------
 * This script loads `slowest_scanner-10.csv` (expected to be in the same
 * directory as this HTML file) and visualises the mean runtime for each
 * command across the different `counto` values.
 *
 * Features:
 *   • Scatter plot (counto vs mean runtime)
 *   • Colour‑coded series per command
 *   • Tooltip on hover
 *   • Clickable legend to toggle series visibility
 *   • Zoom & pan (mouse wheel / drag)
 *
 * The CSV format (see docs/slowest_scanner-10.csv) is:
 *   command,mean,stddev,median,user,system,min,max,parameter_num_count
 *
 * The `command` column is prefixed with `counto=<N> ` – we strip that prefix
 * for the legend and keep the numeric `<N>` as the X‑axis value.
 */

 // ----------------------------------------------
 // CONFIG
 // ----------------------------------------------
 const CSV_URL = "slowest_scanner-10.csv"; // relative to this HTML file
 const MARGIN = {top: 40, right: 20, bottom: 50, left: 70};
 const WIDTH = window.innerWidth - MARGIN.left - MARGIN.right;
 const HEIGHT = window.innerHeight - MARGIN.top - MARGIN.bottom;
 const PALETTE = [
   "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
   "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
   "#bcbd22", "#17becf"
 ];

 // ----------------------------------------------
 // TOOLTIP
 // ----------------------------------------------
 const tooltip = d3.select("body").append("div")
     .attr("class", "tooltip")
     .style("opacity", 0);

 // ----------------------------------------------
 // MAIN
 // ----------------------------------------------
 (async function() {
   // 1️⃣ Load CSV (raw text) – we need the original command column
   const rawText = await d3.text(CSV_URL);
   const rows = rawText.trim().split(/\n/).slice(1) // drop header
       .map(parseCsvLine)
       .filter(d => d !== null);

   // 2️⃣ Group by command (cleaned, without the leading "counto=…")
   const groups = d3.group(rows, d => d.command);

   // 3️⃣ Scales – X is the "counto" (the first column after cleaning),
   //    Y is the mean time (seconds). Both are linear.
   const xExtent = d3.extent(rows, d => d.x);
   const yExtent = d3.extent(rows, d => d.y);
   const xScale = d3.scaleLinear().domain(xExtent).nice().range([0, WIDTH]);
   const yScale = d3.scaleLinear().domain(yExtent).nice().range([HEIGHT, 0]);

   // 4️⃣ SVG container + clipping (so zoomed points don’t overflow)
   const svg = d3.select("#chart").append("svg")
       .attr("width", WIDTH + MARGIN.left + MARGIN.right)
       .attr("height", HEIGHT + MARGIN.top + MARGIN.bottom)
     .append("g")
       .attr("transform", `translate(${MARGIN.left},${MARGIN.top})`);

   svg.append("defs").append("clipPath")
       .attr("id", "clip")
     .append("rect")
       .attr("width", WIDTH)
       .attr("height", HEIGHT);

   const chart = svg.append("g").attr("clip-path", "url(#clip)");

   // 5️⃣ Draw lines per command (different colour per series)
   let colorIdx = 0;
   const lineGenerator = d3.line()
       .x(d => xScale(d.x))
       .y(d => yScale(d.y))
       .curve(d3.curveMonotoneX);   // smooth monotone line (optional)

   for (const [cmd, pts] of groups) {
     // Ensure points are sorted by the X value so the line is drawn correctly
     const sorted = pts.slice().sort((a, b) => d3.ascending(a.x, b.x));

     const colour = PALETTE[colorIdx % PALETTE.length];
     const seriesId = `series-${colorIdx + 1}`;   // unique class/id for toggling

     // Append the path (the line)
     chart.append("path")
         .datum(sorted)
         .attr("class", seriesId)
         .attr("fill", "none")
         .attr("stroke", colour)
         .attr("stroke-width", 2)
         .attr("d", lineGenerator);

     // Also keep invisible circles for tooltip (optional – improves hover precision)
     chart.selectAll(`.dot-${colorIdx + 1}`)
         .data(sorted)
         .enter()
         .append("circle")
         .attr("class", `dot-${colorIdx + 1}`)
         .attr("cx", d => xScale(d.x))
         .attr("cy", d => yScale(d.y))
         .attr("r", 4)               // slightly larger hit‑area
         .attr("fill", colour)
         .attr("opacity", 0)         // invisible; only for mouse events
         .on("mouseover", (event, d) => {
           tooltip.transition().duration(100).style("opacity", .9);
           tooltip.html(
             `<strong>${cmd}</strong><br/>counto=${d.x}<br/>mean=${d.y.toFixed(4)}s`
           )
           .style("left", (event.pageX + 8) + "px")
           .style("top", (event.pageY - 28) + "px");
         })
         .on("mouseout", () => tooltip.transition().duration(200).style("opacity", 0));

     colorIdx++;
   }

   // 6️⃣ Axes
   const xAxis = d3.axisBottom(xScale).ticks(10).tickFormat(d3.format("d"));
   const yAxis = d3.axisLeft(yScale).ticks(10);
   svg.append("g")
       .attr("transform", `translate(0,${HEIGHT})`)
       .call(xAxis)
     .append("text")
       .attr("x", WIDTH/2)
       .attr("y", 35)
       .attr("fill", "black")
       .attr("text-anchor", "middle")
       .text("counto (lines to emit)");

   svg.append("g")
       .call(yAxis)
     .append("text")
       .attr("transform", "rotate(-90)")
       .attr("x", -HEIGHT/2)
       .attr("y", -45)
       .attr("fill", "black")
       .attr("text-anchor", "middle")
       .text("Mean runtime (seconds)");

   // 7️⃣ Legend (click to toggle visibility)
   const legend = svg.append("g")
       .attr("class", "legend")
       .attr("transform", `translate(${WIDTH+10},0)`);
   let legendY = 0;
   colorIdx = 0;
   for (const [cmd] of groups) {
     const colour = PALETTE[colorIdx % PALETTE.length];
     const entry = legend.append("g")
         .attr("transform", `translate(0,${legendY})`)
         .style("cursor", "pointer");
     entry.append("rect")
         .attr("width", 12).attr("height", 12)
         .attr("fill", colour);
     entry.append("text")
         .attr("x", 16).attr("y", 10)
         .text(cmd);
     entry.on("click", () => {
       const seriesClass = `series-${colorIdx + 1}`;
       const dotsClass   = `dot-${colorIdx + 1}`;

       const currentlyVisible = chart.selectAll(`.${seriesClass}`).style("display") !== "none";

       // Toggle line visibility
       chart.selectAll(`.${seriesClass}`)
            .style("display", currentlyVisible ? "none" : null);

       // Toggle the invisible circles used for tooltip
       chart.selectAll(`.${dotsClass}`)
            .style("display", currentlyVisible ? "none" : null);

       // Dim/restore legend swatch
       entry.select("rect")
            .attr("fill-opacity", currentlyVisible ? 0.2 : 1);
     });
     legendY += 18;
     colorIdx++;
   }

   // 8️⃣ Zoom & pan (preserves axes)
   const zoom = d3.zoom()
       .scaleExtent([0.5, 20])
       .extent([[0,0],[WIDTH,HEIGHT]])
       .on("zoom", zoomed);
   svg.call(zoom);

   function zoomed(event) {
     const t = event.transform;
     const newX = t.rescaleX(xScale);
     const newY = t.rescaleY(yScale);

     // Update axes
     svg.selectAll("g.x-axis").call(d3.axisBottom(newX).ticks(10).tickFormat(d3.format("d")));
     svg.selectAll("g.y-axis").call(d3.axisLeft(newY).ticks(10));

     // Update line generators with the new scales
     const updatedLine = d3.line()
         .x(d => newX(d.x))
         .y(d => newY(d.y))
         .curve(d3.curveMonotoneX);

     // Redraw each series path
     chart.selectAll("path")
         .attr("d", d => updatedLine(d));

     // Update invisible circles (still needed for tooltip)
     chart.selectAll("circle")
         .attr("cx", d => newX(d.x))
         .attr("cy", d => newY(d.y));
   }
 })();

 // -------------------------------------------------
 // Helper – parse a line of the CSV produced by the benchmark script
 // -------------------------------------------------
 function parseCsvLine(line) {
   // Expected columns (see docs/slowest_scanner-10.csv header):
   // command,mean,stddev,median,user,system,min,max,parameter_num_count
   const parts = line.split(',');
   if (parts.length < 9) return null;
   const rawCommand = parts[0];
   // Strip the leading "counto=\S+ " that the original script adds
   const command = rawCommand.replace(/^counto=\S+\s*/, '').trim();
   const countoMatch = rawCommand.match(/^counto=(\d+)/);
   const x = countoMatch ? +countoMatch[1] : null; // the "counto" value (X‑axis)
   const y = +parts[1]; // mean runtime (seconds)
   return {command, x, y};
 }
