/**
 * d3_slowest_scanner.js
 * Main script for the D3 Slowest Scanner visualization.
 * Existing scanner code would be placed here.
 */

// ---- Theme toggle logic ----

// Helper to apply the stored theme on page load
function applyStoredTheme() {
  const saved = localStorage.getItem('theme');
  if (saved === 'dark') {
    document.body.classList.add('dark-theme');
    updateToggleLabel(true);
  }
}

// Update button label/icon based on current mode (optional)
function updateToggleLabel(isDark) {
  const btn = document.getElementById('theme-toggle');
  if (!btn) return;
  btn.textContent = isDark ? '☀️ Light Mode' : '🌙 Dark Mode';
}

// Theme toggle handler
function toggleTheme() {
  const isDark = document.body.classList.toggle('dark-theme');
  localStorage.setItem('theme', isDark ? 'dark' : 'light');
  updateToggleLabel(isDark);
}

// Attach click listener once DOM is ready
document.addEventListener('DOMContentLoaded', () => {
  const btn = document.getElementById('theme-toggle');
  if (btn) {
    btn.addEventListener('click', toggleTheme);
  }
  applyStoredTheme(); // set initial state
});
