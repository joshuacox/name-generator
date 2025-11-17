// --------------------------------------------------------
// chart.js – draws a line chart of `mean` (y) vs `parameter_num_count` (x)
// from the CSV file `log/scanner-12.csv`.
// ---------------------------------------------------------------------------

// Helper: parse a CSV line into an object {x: Number, y: Number}
function parseLine(line) {
  // CSV columns (see the file header):
  // command,mean,stddev,median,user,system,min,max,parameter_num_count
  const parts = line.split(',');
  // Guard against malformed rows
  if (parts.length < 9) return null;

  const mean = parseFloat(parts[1]);               // column 2
  const paramCount = parseInt(parts[8], 10);       // column 9
  if (isNaN(mean) || isNaN(paramCount)) return null;

  return { x: paramCount, y: mean };
}

// Fetch the CSV, turn it into an array of {x,y}
async function loadData() {
  const response = await fetch('../log/scanner-12.csv');
  if (!response.ok) {
    throw new Error(`Failed to load CSV: ${response.status}`);
  }
  const text = await response.text();
  const lines = text.trim().split('\n');

  // Remove header line
  lines.shift();

  const points = [];
  for (const line of lines) {
    const pt = parseLine(line);
    if (pt) points.push(pt);
  }

  // Sort by x so the line chart is monotonic in the x‑direction
  points.sort((a, b) => a.x - b.x);
  return points;
}

// Create the Chart.js instance
async function drawChart() {
  const ctx = document.getElementById('myChart').getContext('2d');
  const dataPoints = await loadData();

  // Separate the arrays for Chart.js (labels = x, data = y)
  const labels = dataPoints.map(p => p.x);
  const data   = dataPoints.map(p => p.y);

  new Chart(ctx, {
    type: 'line',
    data: {
      labels: labels,
      datasets: [{
        label: 'Mean',
        data: data,
        borderColor: 'rgba(75, 192, 192, 1)',
        backgroundColor: 'rgba(75, 192, 192, 0.2)',
        fill: false,
        tension: 0.1,          // smooth line
        pointRadius: 3,
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        x: {
          title: { display: true, text: 'parameter_num_count' },
          ticks: { precision: 0 }   // integer ticks
        },
        y: {
          title: { display: true, text: 'mean (seconds)' },
          beginAtZero: false
        }
      },
      plugins: {
        tooltip: {
          callbacks: {
            // Show both x and y in the tooltip
            label: ctx => `mean = ${ctx.parsed.y}`
          }
        }
      }
    }
  });
}

// Run when the page is ready
document.addEventListener('DOMContentLoaded', drawChart);
