/* ------------------------------------------ *
 *  slowest‑chart.js – draw a line chart for slowest_scanner-10.csv
 * ------------------------------------------ */

/**
 * Load a CSV file, return a Map where the key is the *cleaned* command string
 * and the value is an array of points {x: parameter_num_count, y: mean}.
 */
async function loadAndGroup(csvPath) {
    const response = await fetch(csvPath);
    if (!response.ok) {
        throw new Error(`Failed to fetch ${csvPath}: ${response.status}`);
    }
    const text = await response.text();

    // Split into lines, drop the header
    const lines = text.trim().split('\n');
    const header = lines.shift();               // remove header line

    // Map<string, Array<{x:number,y:number}>>
    const groups = new Map();

    // Regex that removes the leading “counto=… ” part
    const stripCounto = /^counto=\S+\s*/;

    for (const raw of lines) {
        if (!raw.trim()) continue;               // skip empty lines

        // CSV is simple – no quoted commas in this data set
        const parts = raw.split(',');

        // Guard against malformed rows
        if (parts.length < 9) continue;

        // 0: command, 1: mean, 8: parameter_num_count
        const rawCommand = parts[0].trim();
        const command = rawCommand.replace(stripCounto, '');

        const mean = parseFloat(parts[1]);
        const paramCount = parseInt(parts[8], 10);   // already a power of two

        if (Number.isNaN(mean) || Number.isNaN(paramCount)) continue;

        const point = { x: paramCount, y: mean };

        if (!groups.has(command)) groups.set(command, []);
        groups.get(command).push(point);
    }

    // Ensure points are sorted by x (parameter count) for each command
    for (const pts of groups.values()) {
        pts.sort((a, b) => a.x - b.x);
    }

    return groups;
}

/**
 * Return a deterministic colour for a dataset index.
 * Uses a small built‑in palette; falls back to HSL if we run out.
 */
function getColor(idx) {
    const palette = [
        '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
        '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
        '#bcbd22', '#17becf'
    ];
    if (idx < palette.length) return palette[idx];
    // generate a hue‑based colour for larger indices
    const hue = (idx * 137) % 360;               // golden‑angle spread
    return `hsl(${hue},70%,50%)`;
}

/**
 * Build the Chart.js configuration and render it into the canvas.
 */
async function draw() {
    const groups = await loadAndGroup('slowest_scanner-10.csv');

    const datasets = [];
    let i = 0;
    for (const [cmd, points] of groups.entries()) {
        const color = getColor(i);
        datasets.push({
            label: cmd,
            data: points,
            borderColor: color,
            backgroundColor: color + '33',   // 20 % opacity for fill (if needed)
            tension: 0,                      // straight lines
            pointRadius: 3,
            fill: false
        });
        i++;
    }

    const ctx = document.getElementById('slowestChart').getContext('2d');

    new Chart(ctx, {
        type: 'line',
        data: { datasets },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            interaction: { mode: 'nearest', intersect: false },
            scales: {
                x: {
                    type: 'linear',
                    title: { display: true, text: 'parameter_num_count (2ⁿ)' },
                    ticks: { precision: 0 }
                },
                y: {
                    title: { display: true, text: 'mean (seconds)' },
                    beginAtZero: false
                }
            },
            plugins: {
                tooltip: {
                    callbacks: {
                        title: ctx => ctx[0].dataset.label,
                        label: ctx => `count = ${ctx.parsed.x}, mean = ${ctx.parsed.y}`
                    }
                },
                legend: {
                    position: 'right',
                    labels: { usePointStyle: true }
                }
            }
        }
    });
}

/* ------------------------------------------ *
 *  Kick‑off when the page has finished loading.
 * ------------------------------------------ */
window.addEventListener('DOMContentLoaded', draw);
