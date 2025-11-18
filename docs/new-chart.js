/**
 * Utility: strip the leading "counto=… " from a command string.
 * Regex:  ^counto=\S+\s*
 */
function cleanCommand(cmd) {
    return cmd.replace(/^counto=\S+\s*/, '').trim();
}

/**
 * Parse a CSV line into an object.
 * Expected columns (header):
 * command,mean,stddev,median,user,system,min,max,parameter_num_count
 */
function parseCsvLine(line) {
    const parts = line.split(',');
    if (parts.length < 9) return null;               // malformed row
    const [
        rawCommand,
        mean,
        /* stddev */ ,
        /* median */ ,
        /* user */ ,
        /* system */ ,
        /* min */ ,
        /* max */ ,
        paramCount
    ] = parts;

    const command = cleanCommand(rawCommand);
    const x = Number(paramCount);
    const y = Number(mean);
    if (Number.isNaN(x) || Number.isNaN(y)) return null;

    return { command, x, y };
}

/**
 * Load a CSV file, return a Promise that resolves to an array of parsed rows.
 */
async function loadCsv(url) {
    const resp = await fetch(url);
    if (!resp.ok) throw new Error(`Failed to fetch ${url}: ${resp.status}`);
    const text = await resp.text();
    const lines = text.trim().split('\n');
    // drop header
    lines.shift();
    const rows = [];
    for (const line of lines) {
        const row = parseCsvLine(line);
        if (row) rows.push(row);
    }
    return rows;
}

/**
 * Transform the X‑value according to the chart’s rule.
 *   - For the first chart (slowest_scanner‑10.csv) we keep the raw count.
 *   - For all others we use 2 ** count.
 */
function xTransform(x, isFirstChart) {
    return isFirstChart ? x : Math.pow(2, x);
}

/**
 * Build a Chart.js line chart in the given canvas.
 *
 * @param {HTMLCanvasElement} canvas
 * @param {Array<Object>} rows   – objects {command, x, y}
 * @param {boolean} isFirstChart – true for slowest_scanner‑10.csv
 */
function buildChart(canvas, rows, isFirstChart) {
    // Group rows by command
    const groups = new Map(); // command → [{x, y}, …]
    for (const { command, x, y } of rows) {
        const transformedX = xTransform(x, isFirstChart);
        if (!groups.has(command)) groups.set(command, []);
        groups.get(command).push({ x: transformedX, y });
    }

    // Sort each series by X (ascending) – Chart.js expects ordered points
    const datasets = [];
    const palette = [
        '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
        '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
        '#bcbd22', '#17becf'
    ];
    let idx = 0;
    for (const [cmd, points] of groups.entries()) {
        points.sort((a, b) => a.x - b.x);
        const color = palette[idx % palette.length];
        datasets.push({
            label: cmd,
            data: points.map(p => ({ x: p.x, y: p.y })),
            borderColor: color,
            backgroundColor: color + '33',
            fill: false,
            tension: 0.1
        });
        idx++;
    }

    new Chart(canvas.getContext('2d'), {
        type: 'line',
        data: { datasets },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            scales: {
                x: {
                    type: 'linear',
                    title: {
                        display: true,
                        text: isFirstChart
                            ? 'parameter_num_count'
                            : '2 ^ parameter_num_count'
                    }
                },
                y: {
                    title: {
                        display: true,
                        text: 'mean (seconds)'
                    }
                }
            },
            plugins: {
                legend: { position: 'bottom' },
                tooltip: {
                    callbacks: {
                        // Show the original count in the tooltip for the power‑of‑2 charts
                        title: ctx => {
                            const rawX = ctx[0].raw.x;
                            if (isFirstChart) return `count = ${rawX}`;
                            // reverse the power‑of‑2 transformation
                            const original = Math.log2(rawX);
                            return `count = ${original}`;
                        }
                    }
                }
            }
        }
    });
}

/**
 * Main driver – load all six CSVs and render the charts.
 */
async function renderAllCharts() {
    const files = [
        { url: './slowest_scanner-10.csv', canvasId: 'chart0', first: true },
        { url: './slow_scanner-7.csv',      canvasId: 'chart1', first: false },
        { url: './scanner-11.csv',          canvasId: 'chart2', first: false },
        { url: './fast_scanner-15.csv',     canvasId: 'chart3', first: false },
        { url: './faster_scanner-20.csv',   canvasId: 'chart4', first: false },
        { url: './fastest_scanner-22.csv',  canvasId: 'chart5', first: false }
    ];

    for (const { url, canvasId, first } of files) {
        try {
            const rows = await loadCsv(url);
            const canvas = document.getElementById(canvasId);
            if (!canvas) throw new Error(`Canvas #${canvasId} not found`);
            buildChart(canvas, rows, first);
        } catch (e) {
            console.error(`Failed to render ${url}:`, e);
        }
    }
}

// Run after the DOM is ready (deferred script tag already ensures this)
renderAllCharts();
