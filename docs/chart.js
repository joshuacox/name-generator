// --------------------------------------------------------
// chart.js – draws a line chart of `mean` (y) vs `parameter_num_count` (x)
// from the CSV file `log/scanner-12.csv`.
// ---------------------------------------------------------------------------

function parseLine(line) {
  // CSV columns (see the file header):
  // command,mean,stddev,median,user,system,min,max,parameter_num_count
  const parts = line.split(',');
  // Guard against malformed rows
  if (parts.length < 9) return null;

  // Strip the leading “counto=<num> ” that the benchmark script prefixes.
  // Example: “counto=1 ./name-generator” → “./name-generator”
  const rawCommand = parts[0].trim();                 // column 1
  const command = rawCommand.replace(/^counto=\S+\s*/, '');
  const mean = parseFloat(parts[1]);                  // column 2
  const paramCount = parseInt(parts[8], 10);          // column 9

  // NEW – X‑value is 2 raised to the parameter count
  const x = Math.pow(2, paramCount);

  if (!command || isNaN(mean) || isNaN(paramCount)) return null;

  // Return an object that carries the command together with the point.
  return { command, x, y: mean };
}

async function loadDataFrom(csvPath) {
  // Load and parse a CSV file (e.g. scanner-12.csv or fast_scanner-12.csv)
  const response = await fetch(csvPath);
  if (!response.ok) {
    throw new Error(`Failed to load CSV ${csvPath}: ${response.status}`);
  }
  const text = await response.text();
  const lines = text.trim().split('\n');

  // Remove header line
  lines.shift();

  /** Map<string, Array<{x:number,y:number}>> */
  const byCommand = new Map();

  for (const line of lines) {
    const pt = parseLine(line);
    if (!pt) continue;
    const { command, x, y } = pt;
    if (!byCommand.has(command)) byCommand.set(command, []);
    byCommand.get(command).push({ x, y });
  }

  // Ensure points are ordered by x (2^parameter_num_count)
  for (const series of byCommand.values()) {
    series.sort((a, b) => a.x - b.x);
  }
  return byCommand;
}

/* ---------- Shared colour palette ---------- */
const palette = [
  '#4e79a7', '#f28e2b', '#e15759', '#76b7b2',
  '#59a14f', '#edc949', '#af7aa1', '#ff9da7',
  '#9c755f', '#bab0ab'
];
const getColor = i => palette[i % palette.length];

/* Draw a Chart.js line chart on the canvas identified by `canvasId` */
function drawChartOn(canvasId, byCommand) {
  const ctx = document.getElementById(canvasId).getContext('2d');
  const datasets = [];
  let idx = 0;
  for (const [cmd, points] of byCommand.entries()) {
    datasets.push({
      label: cmd,
      data: points,
      borderColor: getColor(idx),
      backgroundColor: getColor(idx) + '33',
      fill: false,
      tension: 0.1,
      pointRadius: 3,
    });
    idx++;
  }

  new Chart(ctx, {
    type: 'line',
    data: { datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        x: {
          title: { display: true, text: '2^parameter_num_count' },
          type: 'linear',
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
        }
      }
    }
  });
}

/* Top‑level orchestrator – load both CSVs and render two charts */
async function drawCharts() {
  const scannerData = await loadDataFrom('scanner-12.csv');
  const fastData = await loadDataFrom('fast_scanner-15.csv');
  const fasterData = await loadDataFrom('faster_scanner-20.csv');
  const fastestData = await loadDataFrom('fastest_scanner-22.csv');

  drawChartOn('myChart', scannerData);
  drawChartOn('myChartFast', fastData);
  drawChartOn('myChartFaster', fasterData);
  drawChartOn('myChartFastest', fastestData);
}

// Run when the page is ready
document.addEventListener('DOMContentLoaded', drawCharts);
