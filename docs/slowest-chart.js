d3.csv("slowest-chart.csv").then(data => {
    // Convert numeric fields from strings to numbers
    data.forEach(d => {
        d.parameter_num_count = +d.parameter_num_count;
        d.mean = +d.mean;
    });

    // Group data by command, stripping any leading prefix before a colon
    const groups = {};
    data.forEach(row => {
        const command = row.command.replace(/^[^:]*:/, "");
        if (!groups[command]) {
            groups[command] = [];
        }
        groups[command].push({ parameter_num_count: row.parameter_num_count, mean: row.mean });
    });

    const margin = { top: 20, right: 20, bottom: 30, left: 40 };
    const width = 800 - margin.left - margin.right;
    const height = 600 - margin.top - margin.bottom;

    const svg = d3.select("#chart")
        .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

    const xScale = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.parameter_num_count)])
        .range([0, width]);

    const yScale = d3.scaleLinear()
        .domain([0, d3.max(data, d => d.mean)])
        .range([height, 0]);

    const lineGenerator = d3.line()
        .x(d => xScale(d.parameter_num_count))
        .y(d => yScale(d.mean));

    const color = d3.scaleOrdinal(d3.schemeCategory10);

    Object.keys(groups).forEach((key, i) => {
        const lineData = groups[key];
        svg.append("path")
            .datum(lineData)
            .attr("fill", "none")
            .attr("stroke", color(i))
            .attr("stroke-width", 1.5)
            .attr("d", lineGenerator);
    });

    // X Axis
    svg.append("g")
        .attr("transform", `translate(0,${height})`)
        .call(d3.axisBottom(xScale).ticks(10).tickFormat(d3.format(".0f")));

    // Y Axis
    svg.append("g")
        .call(d3.axisLeft(yScale).ticks(10).tickFormat(d3.format(".2f")));
});
