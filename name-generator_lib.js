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
 *   The library now exports a `setTheme` function that allows you to
 *   customise CSS variables used by the UI (see name-generator.html).
 *   Example:
 *     import { setTheme } from './name-generator_lib.js';
 *     setTheme({ primaryColor: '#ff6600', backgroundColor: '#111111' });
 */

const SEPARATOR = '-';

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
