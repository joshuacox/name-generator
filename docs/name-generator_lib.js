/**
 * Minimal name‑generator library for the browser.
 *
 * It fetches two plain‑text files (one with nouns, one with adjectives),
 * picks a random entry from each, lower‑cases the noun, and joins them
 * with a separator (default “-”).  The behaviour mirrors the POSIX
 * shell script `name-generator.sh`.
 *
 * The files are expected to be served from the same origin, e.g.
 *   /nouns/full.list   (or any file inside the `nouns` folder)
 *   /adjectives/full.list   (or any file inside the `adjectives` folder)
 *
 * If you need different locations, pass them as arguments to
 * `generateName` or `generateNames`.
 *
 * Theming support:
 *   The library exports a `setTheme` function that allows you to
 *   customise CSS variables used by the UI (see name-generator.html).
 *
 * Copy‑to‑clipboard support:
 *   `copyToClipboard(text)` copies the supplied text to the clipboard.
 *   `nameWithCopyButton(name)` returns an HTML snippet that displays the
 *   name together with a “Copy” button.
 *
 * Toast feedback support:
 *   `showToast(message, duration)` displays a temporary toast notification.
 */

/* ------------------------------- */
/* Configuration                  */
/* ------------------------------- */

let SEPARATOR = '-'; // default separator, can be changed via setSeparator()

/* ------------------------------- */
/* Helper functions               */
/* ------------------------------- */

/**
 * Fetch a text file and return an array of non‑empty trimmed lines.
 *
 * @param {string} url - URL of the file to fetch.
 * @returns {Promise<string[]>} Array of lines.
 */
async function fetchLines(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to fetch ${url}: ${response.status} ${response.statusText}`);
  }
  const text = await response.text();
  return text
    .split('\n')
    .map(line => line.trim())
    .filter(line => line.length > 0);
}

/**
 * Copy a string to the clipboard using the Clipboard API.
 *
 * @param {string} text - Text to copy.
 * @returns {Promise<void>}
 */
export async function copyToClipboard(text) {
  if (!navigator.clipboard) {
    // Fallback for older browsers
    const textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.style.position = 'fixed'; // avoid scrolling to bottom
    document.body.appendChild(textarea);
    textarea.focus();
    textarea.select();
    try {
      document.execCommand('copy');
    } finally {
      document.body.removeChild(textarea);
    }
    return;
  }
  await navigator.clipboard.writeText(text);
}

/**
 * Return an HTML snippet that shows a name together with a copy button.
 *
 * @param {string} name - The generated name.
 * @returns {string} HTML string.
 */
export function nameWithCopyButton(name) {
  // Escape any potential HTML characters in the name
  const escaped = name
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;');
  return `
    <div class="name-item">
      <span class="generated-name">${escaped}</span>
      <button class="copy-btn" data-name="${escaped}">Copy</button>
    </div>
    <hr>
  `.trim();
}

/**
 * Show a temporary toast notification.
 *
 * @param {string} message - Text to display.
 * @param {number} [duration=2000] - How long (ms) the toast stays visible.
 */
export function showToast(message, duration = 2000) {
  const toast = document.createElement('div');
  toast.className = 'toast';
  toast.textContent = message;
  document.body.appendChild(toast);
  // Force reflow so transition works
  void toast.offsetWidth;
  toast.classList.add('show');
  setTimeout(() => {
    toast.classList.remove('show');
    toast.addEventListener('transitionend', () => toast.remove(), { once: true });
  }, duration);
}

/**
 * Change the separator used when joining adjective and noun.
 *
 * @param {string} newSep - New separator string (typically a single character).
 */
export function setSeparator(newSep) {
  SEPARATOR = newSep;
}

/**
 * Retrieve the current separator.
 *
 * @returns {string} The current separator.
 */
export function getSeparator() {
  return SEPARATOR;
}

/* ------------------------------- */
/* Public API                     */
/* ------------------------------- */

/**
 * Generate a single random name.
 *
 * @param {string} [nounUrl='/nouns/full.list']        - URL of the noun list.
 * @param {string} [adjUrl='/adjectives/full.list']    - URL of the adjective list.
 * @returns {Promise<string>} The generated name.
 */
export async function generateName(nounUrl = '/nouns/full.list', adjUrl = '/adjectives/full.list') {
  const [nounLines, adjLines] = await Promise.all([
    fetchLines(nounUrl),
    fetchLines(adjUrl)
  ]);

  if (nounLines.length === 0) {
    throw new Error('Noun list is empty');
  }
  if (adjLines.length === 0) {
    throw new Error('Adjective list is empty');
  }

  const noun = nounLines[Math.floor(Math.random() * nounLines.length)].toLowerCase();
  const adjective = adjLines[Math.floor(Math.random() * adjLines.length)];

  return `${adjective}${SEPARATOR}${noun}`;
}

/**
 * Generate multiple random names.
 *
 * @param {number} [count=50] - How many names to generate (defaults to 50).
 * @param {string} [nounUrl='/nouns/full.list']        - URL of the noun list.
 * @param {string} [adjUrl='/adjectives/full.list']    - URL of the adjective list.
 * @returns {Promise<string[]>} Array of generated names.
 */
export async function generateNames(
  count = 50,
  nounUrl = '/nouns/full.list',
  adjUrl = '/adjectives/full.list'
) {
  if (!Number.isInteger(count) || count <= 0) {
    throw new Error('Count must be a positive integer');
  }

  const [nounLines, adjLines] = await Promise.all([
    fetchLines(nounUrl),
    fetchLines(adjUrl)
  ]);

  if (nounLines.length === 0) {
    throw new Error('Noun list is empty');
  }
  if (adjLines.length === 0) {
    throw new Error('Adjective list is empty');
  }

  const names = [];
  for (let i = 0; i < count; i++) {
    const noun = nounLines[Math.floor(Math.random() * nounLines.length)].toLowerCase();
    const adjective = adjLines[Math.floor(Math.random() * adjLines.length)];
    names.push(`${adjective}${SEPARATOR}${noun}`);
  }
  return names;
}

/**
 * Apply a simple theme by setting CSS custom properties.
 *
 * @param {Object} theme - Theme configuration.
 * @param {string} [theme.primaryColor] - Primary accent colour.
 * @param {string} [theme.backgroundColor] - Page background colour.
 * @param {string} [theme.textColor] - Default text colour.
 */
export function setTheme({ primaryColor, backgroundColor, textColor } = {}) {
  const root = document.documentElement;
  if (primaryColor) root.style.setProperty('--primary-color', primaryColor);
  if (backgroundColor) root.style.setProperty('--bg-color', backgroundColor);
  if (textColor) root.style.setProperty('--text-color', textColor);
}
