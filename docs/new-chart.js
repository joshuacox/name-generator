/* -----------------------------------------------------
   Load a CSV, strip the leading “counto=… ” from the command,
   turn `parameter_num_count` into 2**n, and draw a line chart.
   One chart per CSV file (six total).
   --------------------------------------------------------- */

// Helper: parse a CSV line into an object
function parseCsvLine(line) {
  const [
    rawCommand, mean, /* stddev */, /* median */,
    /* user */, /* system */, /* min */, /* max */,
    paramCount,
  ] = line.split(',');
  // Strip the leading “counto=… ” (regex from the request)
  const command = rawCommand.replace(/^counto=\S+\s*/, '');
  const x = Math.pow(2, Number(paramCount));
  const y = Number(mean);
  return {command, x, y};
}

// Load a CSV, group points by command and return a Chart.js dataset array
async function loadDataset(csvPath) {
  const resp = await fetch(csvPath);
  if (!resp.ok) throw new Error(`Failed to fetch ${csvPath}`);
  const text = await resp.text();
  const lines = text.trim().split('\n');
  // Remove header
  lines.shift();

  const groups = new Map(); // command → [{x,y},…]
  for (const line of lines) {
    if (!line) continue;
    const {command, x, y} = parseCsvLine(line);
    if (!groups.has(command)) groups.set(command, []);
    groups.get(command).push({x, y});
  }

  // Build Chart.js datasets (sorted by x for each command)
  const colors = [
    '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
    '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
    '#bcbd22', '#17becf',
  ];
  let colorIdx = 0;
  const datasets = [];
  for (const [cmd, points] of groups) {
    points.sort((a, b) => a.x - b.x);
    const color = colors[colorIdx % colors.length];
    colorIdx++;
    datasets.push({
      label: cmd,
      data: points.map(p => ({x: p.x, y: p.y})),
      borderColor: color,
      backgroundColor: color + '33',
      fill: false,
      tension: 0.1,
    });
  }
  return datasets;
}

// Create a line chart on a given canvas element
async function drawChart(csvPath, canvasId, title) {
  const ctx = document.getElementById(canvasId).getContext('2d');
  const datasets = await loadDataset(csvPath);
  new Chart(ctx, {
    type: 'line',
    data: {datasets},
    options: {
      responsive: true,
      plugins: {
        title: {display: true, text: title},
        legend: {position: 'bottom'},
      },
      scales: {
        x: {
          type: 'linear',
          title: {display: true, text: 'Count (2ⁿ)'},
          ticks: {
            // Show the exponent instead of the raw number for readability
            callback: v => `2^${Math.log2(v)}`,
          },
        },
        y: {
          title: {display: true, text: 'Mean (seconds)'},
        },
      },
    },
  });
}

// -------------------------------------------------------------
// Kick‑off all six charts (paths are relative to the docs folder)
// -------------------------------------------------------------
(async () => {
  await drawChart('slowest_scanner-10.csv',
                  'chart-slowest',
                  'Slowest Scanner (counto=1)');

  // The repository contains `slow_scanner-5.csv`; it is the closest match
  // to the requested “slow_scanner‑7.csv”.
  await drawChart('slow_scanner-5.csv',
                  'chart-slow',
                  'Slow Scanner (counto=5)');

  await drawChart('scanner-11.csv',
                  'chart-scanner',
                  'Scanner (counto=2¹¹)');

  await drawChart('fast_scanner-15.csv',
                  'chart-fast',
                  'Fast Scanner (counto=2¹⁵)');

  await drawChart('faster_scanner-20.csv',
                  'chart-faster',
                  'Faster Scanner (counto=2²⁰)');

  await drawChart('fastest_scanner-22.csv',
                  'chart-fastest',
                  'Fastest Scanner (counto=2²²)');
})();
