'use client';

import React, { useState, useEffect, useMemo, useTransition } from 'react';
import Link from 'next/link';
import Image from 'next/image';
import { Copy, Check, RefreshCw, Shuffle, Sliders, Database, Search, Sparkles, Flame, BarChart3, ArrowRight } from 'lucide-react';
import { DEFAULT_ADJECTIVES, DEFAULT_NOUNS } from '../data/sampleWords';

type CasingMode = 'standard' | 'lower' | 'upper' | 'title';

export const NameGeneratorHero: React.FC = () => {
  const [count, setCount] = useState<number>(12);
  const [separator, setSeparator] = useState<string>('-');
  const [casing, setCasing] = useState<CasingMode>('standard');
  const [searchQuery, setSearchQuery] = useState<string>('');

  const [adjectives, setAdjectives] = useState<string[]>(DEFAULT_ADJECTIVES);
  const [nouns, setNouns] = useState<string[]>(DEFAULT_NOUNS);
  const [isFullCorpusLoaded, setIsFullCorpusLoaded] = useState<boolean>(false);
  const [isLoadingCorpus, setIsLoadingCorpus] = useState<boolean>(false);

  const [generatedNames, setGeneratedNames] = useState<string[]>([]);
  const [copiedIndex, setCopiedIndex] = useState<number | null>(null);
  const [copiedAll, setCopiedAll] = useState<boolean>(false);
  const [, startTransition] = useTransition();

  // Format a single name based on casing rules
  const formatName = (adj: string, sep: string, noun: string, mode: CasingMode): string => {
    let formattedAdj = adj;
    let formattedNoun = noun.toLowerCase();

    switch (mode) {
      case 'lower':
        formattedAdj = adj.toLowerCase();
        formattedNoun = noun.toLowerCase();
        break;
      case 'upper':
        formattedAdj = adj.toUpperCase();
        formattedNoun = noun.toUpperCase();
        break;
      case 'title':
        formattedAdj = adj.charAt(0).toUpperCase() + adj.slice(1).toLowerCase();
        formattedNoun = noun.charAt(0).toUpperCase() + noun.slice(1).toLowerCase();
        break;
      case 'standard':
      default:
        // Repo convention: adjective preserved, noun lowercased
        formattedAdj = adj;
        formattedNoun = noun.toLowerCase();
        break;
    }
    return `${formattedAdj}${sep}${formattedNoun}`;
  };

  // Generate random names from currently active pools
  const generate = () => {
    startTransition(() => {
      const results: string[] = [];
      const adjLen = adjectives.length;
      const nounLen = nouns.length;
      if (adjLen === 0 || nounLen === 0) return;

      for (let i = 0; i < count; i++) {
        const randAdj = adjectives[Math.floor(Math.random() * adjLen)];
        const randNoun = nouns[Math.floor(Math.random() * nounLen)];
        results.push(formatName(randAdj, separator, randNoun, casing));
      }
      setGeneratedNames(results);
    });
  };

  // Initial generation on mount
  useEffect(() => {
    generate();
  }, [count, separator, casing, adjectives, nouns]);

  // Load full 27k words corpus from public data
  const loadFullCorpus = async () => {
    if (isFullCorpusLoaded || isLoadingCorpus) return;
    setIsLoadingCorpus(true);
    try {
      const basePath = process.env.NEXT_PUBLIC_BASE_PATH || '';
      const [adjRes, nounRes] = await Promise.all([
        fetch(`${basePath}/data/adjectives.txt`),
        fetch(`${basePath}/data/nouns.txt`),
      ]);
      if (adjRes.ok && nounRes.ok) {
        const [adjText, nounText] = await Promise.all([adjRes.text(), nounRes.text()]);
        const fullAdjs = adjText
          .split('\n')
          .map((w) => w.trim())
          .filter(Boolean);
        const fullNouns = nounText
          .split('\n')
          .map((w) => w.trim())
          .filter(Boolean);

        setAdjectives(fullAdjs);
        setNouns(fullNouns);
        setIsFullCorpusLoaded(true);
      }
    } catch (e) {
      console.error('Failed to load full corpus:', e);
    } finally {
      setIsLoadingCorpus(false);
    }
  };

  const copyToClipboard = (text: string, index?: number) => {
    navigator.clipboard.writeText(text);
    if (index !== undefined) {
      setCopiedIndex(index);
      setTimeout(() => setCopiedIndex(null), 2000);
    } else {
      setCopiedAll(true);
      setTimeout(() => setCopiedAll(false), 2000);
    }
  };

  const filteredNames = useMemo(() => {
    if (!searchQuery.trim()) return generatedNames;
    return generatedNames.filter((name) =>
      name.toLowerCase().includes(searchQuery.toLowerCase())
    );
  }, [generatedNames, searchQuery]);

  return (
    <section id="generator" className="relative py-12 md:py-20 overflow-hidden">
      {/* Background glow effects */}
      <div className="absolute top-1/4 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[600px] h-[350px] bg-gradient-to-tr from-orange-500/15 to-amber-500/10 blur-[100px] pointer-events-none rounded-full" />

      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 relative">
        <div className="text-center max-w-3xl mx-auto mb-10">
          <Link
            href="/article"
            className="inline-flex items-center gap-2 px-4 py-1.5 rounded-full bg-orange-500/10 hover:bg-orange-500/15 text-orange-600 dark:text-orange-400 border border-orange-500/25 text-xs font-semibold mb-4 transition-all hover:scale-105 group"
          >
            <Sparkles className="w-3.5 h-3.5 text-orange-500" />
            <span>Deep Dive: Inside the 49-Language Deathmatch</span>
            <span className="text-slate-400 group-hover:translate-x-0.5 transition-transform">&rarr;</span>
          </Link>
          <h1 className="text-4xl sm:text-5xl lg:text-6xl font-extrabold tracking-tight text-slate-900 dark:text-white leading-[1.15]">
            Give your servers{' '}
            <span className="text-transparent bg-clip-text bg-gradient-to-r from-orange-500 via-amber-500 to-yellow-500">
              personality.
            </span>
          </h1>
          <p className="mt-4 text-base sm:text-lg text-slate-600 dark:text-slate-300">
            Inspired by Docker container naming and engineered across 30+ programming languages.
            Generate memorable, battle-tested names with zero dependencies.
          </p>
        </div>

        {/* Generator Card */}
        <div className="bg-white dark:bg-[#131b2e] rounded-2xl shadow-xl shadow-slate-900/5 dark:shadow-black/40 border border-slate-200 dark:border-slate-800 p-6 sm:p-8">
          {/* Controls Bar */}
          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
            {/* Count Selector */}
            <div>
              <label className="block text-xs font-semibold uppercase tracking-wider text-slate-500 dark:text-slate-400 mb-2">
                Count (<code className="font-mono text-orange-500">counto={count}</code>)
              </label>
              <div className="flex items-center gap-1.5">
                {[1, 5, 12, 33, 100].map((preset) => (
                  <button
                    key={preset}
                    onClick={() => setCount(preset)}
                    className={`flex-1 py-1.5 text-xs font-medium rounded-lg border transition-all cursor-pointer ${
                      count === preset
                        ? 'bg-orange-500 text-white border-orange-500 shadow-sm'
                        : 'border-slate-200 dark:border-slate-800 hover:bg-slate-100 dark:hover:bg-slate-800 text-slate-700 dark:text-slate-300'
                    }`}
                  >
                    {preset}
                  </button>
                ))}
              </div>
            </div>

            {/* Separator Input */}
            <div>
              <label className="block text-xs font-semibold uppercase tracking-wider text-slate-500 dark:text-slate-400 mb-2">
                Separator (<code className="font-mono text-orange-500">SEPARATOR</code>)
              </label>
              <div className="flex items-center gap-1.5">
                {[
                  { label: 'Hyphen (-)', val: '-' },
                  { label: 'Underscore (_)', val: '_' },
                  { label: 'Dot (.)', val: '.' },
                  { label: 'Slash (/)', val: '/' },
                ].map(({ label, val }) => (
                  <button
                    key={val}
                    onClick={() => setSeparator(val)}
                    title={label}
                    className={`flex-1 py-1.5 text-xs font-mono font-medium rounded-lg border transition-all cursor-pointer ${
                      separator === val
                        ? 'bg-orange-500 text-white border-orange-500 shadow-sm'
                        : 'border-slate-200 dark:border-slate-800 hover:bg-slate-100 dark:hover:bg-slate-800 text-slate-700 dark:text-slate-300'
                    }`}
                  >
                    {val}
                  </button>
                ))}
              </div>
            </div>

            {/* Case Formatting */}
            <div>
              <label className="block text-xs font-semibold uppercase tracking-wider text-slate-500 dark:text-slate-400 mb-2">
                Casing Mode
              </label>
              <select
                value={casing}
                onChange={(e) => setCasing(e.target.value as CasingMode)}
                className="w-full py-1.5 px-3 text-xs rounded-lg border border-slate-200 dark:border-slate-800 bg-slate-50 dark:bg-slate-900 text-slate-800 dark:text-slate-200 focus:outline-none focus:ring-2 focus:ring-orange-500 cursor-pointer"
              >
                <option value="standard">Standard (Repo Convention)</option>
                <option value="lower">all-lowercase</option>
                <option value="upper">ALL-UPPERCASE</option>
                <option value="title">Title-Case</option>
              </select>
            </div>

            {/* Dictionary Mode */}
            <div>
              <label className="block text-xs font-semibold uppercase tracking-wider text-slate-500 dark:text-slate-400 mb-2">
                Word Pool
              </label>
              <button
                onClick={loadFullCorpus}
                disabled={isFullCorpusLoaded || isLoadingCorpus}
                className={`w-full py-1.5 px-3 text-xs font-medium rounded-lg border flex items-center justify-center gap-1.5 transition-all ${
                  isFullCorpusLoaded
                    ? 'bg-emerald-500/10 border-emerald-500/30 text-emerald-600 dark:text-emerald-400'
                    : 'border-slate-200 dark:border-slate-800 hover:bg-slate-100 dark:hover:bg-slate-800 text-slate-700 dark:text-slate-300 cursor-pointer'
                }`}
              >
                <Database className="w-3.5 h-3.5" />
                {isFullCorpusLoaded
                  ? `Full Corpus (${adjectives.length + nouns.length} words)`
                  : isLoadingCorpus
                  ? 'Loading 27k words...'
                  : 'Load Full Corpus (27k words)'}
              </button>
            </div>
          </div>

          {/* Action Row */}
          <div className="flex flex-col sm:flex-row items-stretch sm:items-center justify-between gap-3 pt-4 border-t border-slate-100 dark:border-slate-800/80 mb-6">
            <div className="flex items-center gap-2">
              <button
                onClick={generate}
                className="flex items-center justify-center gap-2 px-5 py-2.5 rounded-xl bg-gradient-to-r from-orange-500 to-amber-500 hover:from-orange-600 hover:to-amber-600 text-white font-semibold text-sm shadow-md shadow-orange-500/25 transition-all cursor-pointer hover:scale-[1.02] active:scale-[0.98]"
              >
                <Shuffle className="w-4 h-4" />
                Regenerate ({count})
              </button>
              <button
                onClick={() => copyToClipboard(generatedNames.join('\n'))}
                className="flex items-center justify-center gap-2 px-4 py-2.5 rounded-xl border border-slate-200 dark:border-slate-800 hover:bg-slate-100 dark:hover:bg-slate-800 text-slate-700 dark:text-slate-300 font-medium text-sm transition-colors cursor-pointer"
              >
                {copiedAll ? <Check className="w-4 h-4 text-emerald-500" /> : <Copy className="w-4 h-4" />}
                {copiedAll ? 'Copied All!' : 'Copy All'}
              </button>
            </div>

            {/* Filter in generated names */}
            <div className="relative w-full sm:w-64">
              <Search className="w-4 h-4 absolute left-3 top-1/2 -translate-y-1/2 text-slate-400" />
              <input
                type="text"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                placeholder="Filter names..."
                className="w-full pl-9 pr-3 py-1.5 text-xs rounded-xl border border-slate-200 dark:border-slate-800 bg-slate-50 dark:bg-slate-900/60 text-slate-800 dark:text-slate-200 placeholder:text-slate-400 focus:outline-none focus:ring-2 focus:ring-orange-500"
              />
            </div>
          </div>

          {/* Results Grid */}
          <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3 max-h-[460px] overflow-y-auto pr-1">
            {filteredNames.map((name, index) => (
              <div
                key={`${name}-${index}`}
                className="group relative flex items-center justify-between p-3 rounded-xl bg-slate-50 dark:bg-slate-900/40 border border-slate-200/80 dark:border-slate-800/80 hover:border-orange-500/50 dark:hover:border-orange-500/50 hover:bg-orange-500/[0.02] transition-all"
              >
                <span className="font-mono text-xs sm:text-sm font-medium text-slate-800 dark:text-slate-200 truncate pr-2">
                  {name}
                </span>
                <button
                  onClick={() => copyToClipboard(name, index)}
                  aria-label={`Copy ${name}`}
                  className="opacity-70 group-hover:opacity-100 p-1.5 rounded-lg text-slate-500 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 hover:bg-slate-200/60 dark:hover:bg-slate-800 transition-all cursor-pointer"
                >
                  {copiedIndex === index ? (
                    <Check className="w-3.5 h-3.5 text-emerald-500" />
                  ) : (
                    <Copy className="w-3.5 h-3.5" />
                  )}
                </button>
              </div>
            ))}
          </div>

          {filteredNames.length === 0 && (
            <div className="text-center py-12 text-slate-400 text-sm">
              No generated names match "{searchQuery}"
            </div>
          )}
        </div>

        {/* Polyglot Arena Feature Spotlight Banner */}
        <div className="mt-12 rounded-2xl overflow-hidden border border-slate-200 dark:border-slate-800 bg-white dark:bg-[#131b2e] shadow-xl shadow-slate-900/5 dark:shadow-black/40">
          <div className="grid grid-cols-1 lg:grid-cols-12 items-stretch">
            {/* Left Content Column */}
            <div className="lg:col-span-7 p-6 sm:p-8 lg:p-10 flex flex-col justify-between">
              <div>
                <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20 text-xs font-semibold mb-4">
                  <Flame className="w-3.5 h-3.5 text-orange-500" />
                  <span>The 49-Language Performance Arena</span>
                </div>
                <h3 className="text-2xl sm:text-3xl font-extrabold text-slate-900 dark:text-white tracking-tight leading-tight">
                  From Zig and C to COBOL, AWK, and Brainfuck.
                </h3>
                <p className="mt-3 text-sm sm:text-base text-slate-600 dark:text-slate-300 leading-relaxed">
                  We engineered and strictly benchmarked the exact same CLI name generation contract across five decades of computing languages. Discover cold runtime startup costs, zero-copy memory architectures, and automated CI verification.
                </p>
              </div>

              {/* Stat Badges Grid */}
              <div className="grid grid-cols-2 sm:grid-cols-4 gap-3 my-6 pt-6 border-t border-slate-100 dark:border-slate-800/80">
                <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/60 dark:border-slate-800">
                  <div className="text-xs text-slate-500 dark:text-slate-400 font-medium">Top Speed</div>
                  <div className="text-lg font-extrabold text-emerald-500 font-mono mt-0.5">1.2 ms</div>
                  <div className="text-[10px] text-slate-400">Zig ReleaseFast</div>
                </div>
                <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/60 dark:border-slate-800">
                  <div className="text-xs text-slate-500 dark:text-slate-400 font-medium">Throughput</div>
                  <div className="text-lg font-extrabold text-orange-500 font-mono mt-0.5">833K/s</div>
                  <div className="text-[10px] text-slate-400">Names / sec</div>
                </div>
                <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/60 dark:border-slate-800">
                  <div className="text-xs text-slate-500 dark:text-slate-400 font-medium">Languages</div>
                  <div className="text-lg font-extrabold text-sky-500 font-mono mt-0.5">49</div>
                  <div className="text-[10px] text-slate-400">Polyglot Roster</div>
                </div>
                <div className="p-3 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/60 dark:border-slate-800">
                  <div className="text-xs text-slate-500 dark:text-slate-400 font-medium">Dependencies</div>
                  <div className="text-lg font-extrabold text-purple-500 font-mono mt-0.5">0</div>
                  <div className="text-[10px] text-slate-400">Standard Libs</div>
                </div>
              </div>

              {/* Actions */}
              <div className="flex flex-wrap items-center gap-3">
                <a
                  href="#benchmarks"
                  className="inline-flex items-center gap-2 px-5 py-2.5 rounded-xl bg-orange-500 hover:bg-orange-600 text-white font-medium text-xs sm:text-sm shadow-md shadow-orange-500/20 transition-all hover:scale-[1.02] cursor-pointer"
                >
                  <BarChart3 className="w-4 h-4" />
                  <span>Explore Leaderboard</span>
                </a>
                <Link
                  href="/article"
                  className="inline-flex items-center gap-2 px-5 py-2.5 rounded-xl bg-slate-100 hover:bg-slate-200 dark:bg-slate-800 dark:hover:bg-slate-700 text-slate-800 dark:text-slate-200 font-medium text-xs sm:text-sm transition-all hover:scale-[1.02]"
                >
                  <span>Read 12-Min Technical Deep Dive</span>
                  <ArrowRight className="w-4 h-4 text-orange-500" />
                </Link>
              </div>
            </div>

            {/* Right Image Showcase Column */}
            <div className="lg:col-span-5 relative min-h-[280px] lg:min-h-full overflow-hidden bg-slate-950 group">
              <Image
                src={`${process.env.NEXT_PUBLIC_BASE_PATH || ''}/images/hero-banner.webp`}
                alt="49-Language Deathmatch Arena illustration"
                width={1376}
                height={768}
                className="w-full h-full object-cover transition-transform duration-700 group-hover:scale-105"
              />
              <div className="absolute inset-0 bg-gradient-to-t from-black/80 via-transparent to-transparent lg:bg-gradient-to-r lg:from-black/60 lg:via-transparent lg:to-transparent pointer-events-none" />
              <div className="absolute bottom-4 left-4 right-4 text-[11px] font-mono text-slate-300 flex items-center justify-between">
                <span className="flex items-center gap-1.5 bg-black/60 px-2.5 py-1 rounded-md backdrop-blur-sm border border-white/10">
                  <span className="w-2 h-2 rounded-full bg-emerald-400 animate-pulse" />
                  ARENA TELEMETRY LIVE
                </span>
                <span className="bg-black/60 px-2.5 py-1 rounded-md backdrop-blur-sm border border-white/10 text-orange-400">
                  HYPERFINE VALIDATED
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};
