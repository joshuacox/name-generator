/**
 * Chart.js line chart for slowest-scanner-10.csv
 * Parses the CSV, strips the leading "counto=..." from command names,
 * and plots mean execution times versus parameter count.
 */

const commandRegex = /^counto=\S+\s*/;

fetch('slowest-scanner-10.csv')
    .then(response => response.text())
    .then(data => {
        const rows = data.trim().split('\n');
        const labels = [];
        const datasets = {};

        rows.forEach((row, index) => {
            if (index === 0) return; // Skip header
            const [command, parameter_num_count, mean] = row.split(',');
            const commandName = command.replace(commandRegex, '');
            if (!datasets[commandName]) datasets[commandName] = [];
            datasets[commandName].push(parseFloat(mean));

            // Ensure each parameter count appears once in the labels array
            if (!labels.includes(parameter_num_count)) {
                labels.push(parameter_num_count);
            }
        });

        const ctx = document.getElementById('chart').getContext('2d');
        new Chart(ctx, {
            type: 'line',
            data: {
                labels,
                datasets: Object.keys(datasets).map(command => ({
                    label: command,
                    data: datasets[command],
                    borderColor: getRandomColor(),
                    backgroundColor: getRandomColor(),
                    fill: false,
                })),
            },
            options: {
                scales: {
                    y: {
                        beginAtZero: true
                    }
                }
            }
        });
    })
    .catch(err => console.error('Error loading CSV:', err));

/**
 * Generates a random RGBA color string.
 * @returns {string} RGBA color with 0.5 opacity.
 */
function getRandomColor() {
    const r = Math.floor(Math.random() * 256);
    const g = Math.floor(Math.random() * 256);
    const b = Math.floor(Math.random() * 256);
    return `rgba(${r}, ${g}, ${b}, 0.5)`;
}
