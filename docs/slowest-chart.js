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
    /* ------------------------------------------------
     *  Compute global axis limits (so we can always “reset” to them)
     * ------------------------------------------------ */
    let globalXMin = Infinity, globalXMax = -Infinity;
    let globalYMin = Infinity, globalYMax = -Infinity;
    for (const pts of groups.values()) {
        for (const p of pts) {
            if (p.x < globalXMin) globalXMin = p.x;
            if (p.x > globalXMax) globalXMax = p.x;
            if (p.y < globalYMin) globalYMin = p.y;
            if (p.y > globalYMax) globalYMax = p.y;
        }
    }

    //  -----------------------------------------------------------------
    //  Add a small padding so the outermost points are not drawn on the
    //  exact edge of the chart.  This also protects against the edge case
    //  where min === max (all points have the same value).
    //  -----------------------------------------------------------------
    const padX = (globalXMax - globalXMin) * 0.03;   // 3 % of the X‑range
    const padY = (globalYMax - globalYMin) * 0.10;   // 10 % of the Y‑range

    if (padX === 0) {               // all X values identical
        globalXMin -= 1;
        globalXMax += 1;
    } else {
        globalXMin -= padX;
        globalXMax += padX;
    }

    if (padY === 0) {               // all Y values identical
        globalYMin = globalYMin ? globalYMin * 0.9 : -1;
        globalYMax = globalYMax ? globalYMax * 1.1 : 1;
    } else {
        globalYMin -= padY;
        globalYMax += padY;
    }

    console.log('Chart Y‑range:', globalYMin, '→', globalYMax);

    const chart = new Chart(ctx, {
        type: 'line',
        data: { datasets },
        options: {
            responsive: true,
            maintainAspectRatio: false,
            interaction: { mode: 'nearest', intersect: false },

            // ---------- enable zoom / pan ----------
            plugins: {
                zoom: {
                    // Enable zooming with mouse wheel or pinch
                    zoom: {
                        wheel: { enabled: true },
                        pinch: { enabled: true },
                        mode: 'xy'          // allow both axes
                    },
                    // Enable panning (drag)
                    pan: {
                        enabled: true,
                        mode: 'xy'
                    },
                    // Keep the *original* limits we just computed
                    limits: {
                        x: { min: globalXMin, max: globalXMax },
                        y: { min: globalYMin, max: globalYMax }
                    }
                },
                // keep existing tooltip & legend definitions
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
            },
            // ---------- end zoom / pan ----------
            scales: {
                x: {
                    type: 'linear',
                    title: { display: true, text: 'parameter_num_count (2ⁿ)' },
                    ticks: { precision: 0 },
                    min: globalXMin,
                    max: globalXMax
                },
                y: {
                    title: { display: true, text: 'mean (seconds)' },
                    beginAtZero: false,
                    // No explicit min / max – Chart.js will auto‑scale to include all points.
                    // The zoom plugin still uses the pre‑computed limits (globalYMin/YMax)
                    // so the “Reset zoom” button continues to restore the original padded range.
                    ticks: {
                        // Show a readable number (3 decimal places is enough for the data)
                        callback: v => Number(v).toFixed(3)
                    }
                }
            }
        }
    });
    // expose chart for the reset button
    window._slowestChartInstance = chart;
}

/* ------------------------------------------ *
 *  Kick‑off when the page has finished loading.
 * ------------------------------------------ */
window.addEventListener('DOMContentLoaded', () => {
    draw().then(() => {
        const resetBtn = document.getElementById('resetZoomBtn');
        if (resetBtn) {
            resetBtn.addEventListener('click', () => {
                if (window._slowestChartInstance) {
                    window._slowestChartInstance.resetZoom();
                }
            });
        }
    });
});
