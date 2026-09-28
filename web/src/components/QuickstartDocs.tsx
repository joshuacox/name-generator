'use client';

import React, { useState } from 'react';
import { Terminal, GitPullRequest, Settings, Check, Copy, Cpu, BookOpen } from 'lucide-react';

export const QuickstartDocs: React.FC = () => {
  const [activeTab, setActiveTab] = useState<'compiled' | 'scripting' | 'make'>('compiled');
  const [copiedSnippet, setCopiedSnippet] = useState<string | null>(null);

  const copyCode = (text: string, id: string) => {
    navigator.clipboard.writeText(text);
    setCopiedSnippet(id);
    setTimeout(() => setCopiedSnippet(null), 2000);
  };

  const snippets = {
    compiled: `# Run the ultra-fast Zig implementation (1.2 ms)
counto=10 ./name-generator_zig

# Custom separator and custom wordcount
SEPARATOR=_ counto=25 ./name-generator_go

# Ada, Fortran, Odin, or Crystal
counto=5 ./name-generator_odin
counto=5 ./name-generator_ada
counto=5 ./name-generator_fortran
counto=5 ./name-generator_crystal`,

    scripting: `# Run the AWK speed champion (19.3 ms)
counto=10 ./name-generator.awk

# Unix shells (POSIX sh, Bash, Zsh, KornShell, Nushell)
counto=5 ./name-generator.sh
counto=5 ./name-generator.ksh
counto=5 ./name-generator.nu

# Node.js and TypeScript
counto=10 ./name-generator.js
counto=10 ./name-generator.ts`,

    make: `# Build all available compilers & targets
make all

# Build specific contenders
make name-generator_zig
make name-generator_crystal
make name-generator_fortran
make name-generator_ada
make name-generator_odin

# Run the full BATS regression test suite
make test`,
  };

  return (
    <section id="docs" className="py-16 md:py-24 bg-slate-50/50 dark:bg-[#0b101d]/50 border-t border-slate-200/80 dark:border-slate-800/80">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="max-w-3xl mb-12">
          <div className="inline-flex items-center gap-1.5 px-3 py-1 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20 text-xs font-semibold mb-3">
            <BookOpen className="w-3.5 h-3.5" />
            Developer Reference & CLI Guide
          </div>
          <h2 className="text-3xl sm:text-4xl font-extrabold text-slate-900 dark:text-white tracking-tight">
            How It Works & How to Contribute
          </h2>
          <p className="mt-2 text-slate-600 dark:text-slate-400 text-sm sm:text-base">
            Every implementation adheres to standard environment variables, casing rules, and fallback mechanisms.
          </p>
        </div>

        {/* Code Snippet Runner Box */}
        <div className="rounded-2xl border border-slate-200 dark:border-slate-800 bg-slate-950 text-slate-200 shadow-xl overflow-hidden mb-12">
          <div className="flex items-center justify-between px-4 py-3 bg-slate-900/80 border-b border-slate-800">
            <div className="flex items-center gap-2">
              <div className="flex gap-1.5">
                <div className="w-3 h-3 rounded-full bg-rose-500/80" />
                <div className="w-3 h-3 rounded-full bg-amber-500/80" />
                <div className="w-3 h-3 rounded-full bg-emerald-500/80" />
              </div>
              <span className="text-xs font-mono text-slate-400 ml-2 flex items-center gap-1.5">
                <Terminal className="w-3.5 h-3.5 text-orange-400" />
                terminal execution
              </span>
            </div>

            <div className="flex items-center gap-1 bg-slate-800/70 p-1 rounded-lg text-xs">
              <button
                onClick={() => setActiveTab('compiled')}
                className={`px-3 py-1 rounded-md transition-colors cursor-pointer ${
                  activeTab === 'compiled' ? 'bg-orange-500 text-white font-medium' : 'text-slate-400 hover:text-white'
                }`}
              >
                Compiled
              </button>
              <button
                onClick={() => setActiveTab('scripting')}
                className={`px-3 py-1 rounded-md transition-colors cursor-pointer ${
                  activeTab === 'scripting' ? 'bg-orange-500 text-white font-medium' : 'text-slate-400 hover:text-white'
                }`}
              >
                Scripting
              </button>
              <button
                onClick={() => setActiveTab('make')}
                className={`px-3 py-1 rounded-md transition-colors cursor-pointer ${
                  activeTab === 'make' ? 'bg-orange-500 text-white font-medium' : 'text-slate-400 hover:text-white'
                }`}
              >
                Makefile
              </button>
            </div>
          </div>

          <div className="relative p-5 font-mono text-xs sm:text-sm overflow-x-auto">
            <button
              onClick={() => copyCode(snippets[activeTab], activeTab)}
              aria-label="Copy code"
              className="absolute top-4 right-4 p-2 rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-400 hover:text-white transition-colors cursor-pointer"
            >
              {copiedSnippet === activeTab ? <Check className="w-4 h-4 text-emerald-400" /> : <Copy className="w-4 h-4" />}
            </button>
            <pre className="text-slate-300 leading-relaxed">
              <code>{snippets[activeTab]}</code>
            </pre>
          </div>
        </div>

        {/* Environment Variables & Specs */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 mb-12">
          <div className="p-6 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2 mb-4">
              <Settings className="w-4 h-4 text-orange-500" />
              Standard Environment Variables
            </h3>
            <div className="space-y-3 text-xs">
              <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80">
                <div className="flex items-center justify-between mb-1">
                  <code className="font-mono font-bold text-orange-600 dark:text-orange-400">counto</code>
                  <span className="text-slate-400">Default: Terminal height / 1</span>
                </div>
                <p className="text-slate-600 dark:text-slate-400">
                  Number of random names to generate. Defaults to available terminal lines (via <code>tput lines</code>) or 1 in scripts.
                </p>
              </div>

              <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80">
                <div className="flex items-center justify-between mb-1">
                  <code className="font-mono font-bold text-orange-600 dark:text-orange-400">SEPARATOR</code>
                  <span className="text-slate-400">Default: - (hyphen)</span>
                </div>
                <p className="text-slate-600 dark:text-slate-400">
                  The character or delimiter placed between the adjective and the noun (e.g. <code>_</code> or <code>.</code>).
                </p>
              </div>

              <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80">
                <div className="flex items-center justify-between mb-1">
                  <code className="font-mono font-bold text-orange-600 dark:text-orange-400">NOUN_FILE / ADJ_FILE</code>
                  <span className="text-slate-400">Optional path</span>
                </div>
                <p className="text-slate-600 dark:text-slate-400">
                  Direct path to a custom file containing newline-separated words for nouns or adjectives.
                </p>
              </div>

              <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80">
                <div className="flex items-center justify-between mb-1">
                  <code className="font-mono font-bold text-orange-600 dark:text-orange-400">NOUN_FOLDER / ADJ_FOLDER</code>
                  <span className="text-slate-400">Defaults: ./nouns, ./adjectives</span>
                </div>
                <p className="text-slate-600 dark:text-slate-400">
                  Directory containing wordlist files. If specified, the tool randomly selects a file from the folder.
                </p>
              </div>
            </div>
          </div>

          {/* Add a New Language Guide */}
          <div className="p-6 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm flex flex-col justify-between">
            <div>
              <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2 mb-4">
                <GitPullRequest className="w-4 h-4 text-emerald-500" />
                Add a New Language in 3 Steps
              </h3>
              <div className="space-y-4 text-xs">
                <div className="flex gap-3">
                  <div className="w-6 h-6 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 flex items-center justify-center font-bold shrink-0">
                    1
                  </div>
                  <div>
                    <div className="font-semibold text-slate-900 dark:text-white mb-0.5">Scaffold with newLang.sh</div>
                    <p className="text-slate-500">
                      Run <code className="font-mono text-orange-500">./newLang.sh &lt;lang&gt;</code> to create the starter implementation.
                    </p>
                  </div>
                </div>

                <div className="flex gap-3">
                  <div className="w-6 h-6 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 flex items-center justify-center font-bold shrink-0">
                    2
                  </div>
                  <div>
                    <div className="font-semibold text-slate-900 dark:text-white mb-0.5">Follow Repository Conventions</div>
                    <p className="text-slate-500">
                      Output format must be <code className="font-mono">&lt;adjective&gt;&lt;separator&gt;&lt;noun&gt;</code> where nouns are
                      converted to lowercase and adjectives preserve case.
                    </p>
                  </div>
                </div>

                <div className="flex gap-3">
                  <div className="w-6 h-6 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 flex items-center justify-center font-bold shrink-0">
                    3
                  </div>
                  <div>
                    <div className="font-semibold text-slate-900 dark:text-white mb-0.5">Add BATS Tests & Submit PR</div>
                    <p className="text-slate-500">
                      Add 3 verification tests in <code className="font-mono">test/test.bats</code>, register in <code className="font-mono">langs.csv</code> and <code className="font-mono">Makefile</code>, and open a Pull Request!
                    </p>
                  </div>
                </div>
              </div>
            </div>

            <div className="mt-6 pt-4 border-t border-slate-100 dark:border-slate-800">
              <a
                href="https://github.com/joshuacox/name-generator/blob/main/CONVENTIONS.md"
                target="_blank"
                rel="noreferrer"
                className="text-xs font-semibold text-orange-500 hover:text-orange-600 flex items-center gap-1.5"
              >
                Read full CONVENTIONS.md specification &rarr;
              </a>
            </div>
          </div>
        </div>

        {/* GitHub Actions Automation Callout */}
        <div className="p-6 rounded-2xl bg-gradient-to-r from-orange-500/10 via-amber-500/10 to-yellow-500/10 border border-orange-500/20 shadow-sm flex flex-col md:flex-row items-start md:items-center justify-between gap-6">
          <div className="flex items-start gap-4">
            <div className="p-3 rounded-xl bg-orange-500 text-white shrink-0 shadow-md shadow-orange-500/30">
              <Cpu className="w-6 h-6" />
            </div>
            <div>
              <h4 className="text-base font-bold text-slate-900 dark:text-white">
                Automated On-the-Fly CI Benchmarks
              </h4>
              <p className="mt-1 text-xs sm:text-sm text-slate-600 dark:text-slate-300">
                Whenever new code or languages are merged to main, GitHub Actions compiles the targets, executes the hyperfine deathmatch, updates the datasets, and re-deploys this Next.js site automatically.
              </p>
            </div>
          </div>
          <a
            href="https://github.com/joshuacox/name-generator/actions"
            target="_blank"
            rel="noreferrer"
            className="px-4 py-2 rounded-xl bg-slate-900 text-white dark:bg-white dark:text-slate-900 text-xs font-semibold hover:opacity-90 transition-opacity shrink-0 shadow-sm"
          >
            View GitHub Actions
          </a>
        </div>
      </div>
    </section>
  );
};
