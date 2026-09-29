'use client';

import React, { useState, useMemo } from 'react';
import {
  BarChart3,
  Trophy,
  Flame,
  Gauge,
  ArrowUpDown,
  Download,
  Check,
  ExternalLink,
  TrendingUp,
  Zap,
  BookOpen,
  Activity,
  Cpu,
  Layers,
  Database,
} from 'lucide-react';
import benchmarksData from '../data/benchmarks.json';
import { BenchmarkData } from '../types';

const data = benchmarksData as unknown as BenchmarkData;

export const BenchmarkDashboard: React.FC = () => {
  const [tab, setTab] = useState<'compiled' | 'scripting' | 'vm' | 'memory' | 'scaling' | 'throughput' | 'scanner'>('compiled');
  const [sortField, setSortField] = useState<'mean' | 'name' | 'relative' | 'peakRssMb'>('mean');
  const [sortAsc, setSortAsc] = useState<boolean>(true);
  const [search, setSearch] = useState<string>('');
  const [downloadedJson, setDownloadedJson] = useState<boolean>(false);
  const [downloadedCsv, setDownloadedCsv] = useState<boolean>(false);

  // Selected languages for the scaling curve chart
  const [selectedLangs, setSelectedLangs] = useState<string[]>([
    'Zig',
    'C',
    'Go',
    'Crystal',
    'Nim',
    'Dart',
    'Ada',
    'COBOL',
    'AWK',
    'Node.js',
    'Python',
    'Bash',
  ]);

  const compiledList = useMemo(() => {
    return [...data.deathmatchCompiled]
      .filter((item) => item.name.toLowerCase().includes(search.toLowerCase()))
      .sort((a, b) => {
        let diff = 0;
        if (sortField === 'mean') diff = a.mean - b.mean;
        else if (sortField === 'relative') diff = a.relative - b.relative;
        else if (sortField === 'peakRssMb') diff = (a.peakRssMb || 0) - (b.peakRssMb || 0);
        else diff = a.name.localeCompare(b.name);
        return sortAsc ? diff : -diff;
      });
  }, [search, sortField, sortAsc]);

  const scriptingList = useMemo(() => {
    const list = [...(data.deathmatchScripting || []), ...(data.deathmatchShells || [])];
    return list
      .filter((item) => item.name.toLowerCase().includes(search.toLowerCase()))
      .sort((a, b) => {
        let diff = 0;
        if (sortField === 'mean') diff = a.mean - b.mean;
        else if (sortField === 'relative') diff = a.relative - b.relative;
        else if (sortField === 'peakRssMb') diff = (a.peakRssMb || 0) - (b.peakRssMb || 0);
        else diff = a.name.localeCompare(b.name);
        return sortAsc ? diff : -diff;
      });
  }, [search, sortField, sortAsc]);

  const vmList = useMemo(() => {
    return [...(data.deathmatchVm || [])]
      .filter((item) => item.name.toLowerCase().includes(search.toLowerCase()))
      .sort((a, b) => (sortAsc ? a.mean - b.mean : b.mean - a.mean));
  }, [search, sortAsc]);

  const memoryList = useMemo(() => {
    return [...(data.memoryLeaderboard || [])]
      .filter((item) => item.name.toLowerCase().includes(search.toLowerCase()))
      .sort((a, b) => (sortAsc ? a.peakRssMb - b.peakRssMb : b.peakRssMb - a.peakRssMb));
  }, [search, sortAsc]);

  const maxCompiledMean = Math.max(...data.deathmatchCompiled.map((d) => d.mean), 1);
  const maxScriptingMean = Math.max(...(data.deathmatchScripting || []).map((d) => d.mean), 1);
  const maxMemoryMb = Math.max(...(data.memoryLeaderboard || []).map((d) => d.peakRssMb), 1);
  const maxThroughput = Math.max(...(data.throughputLeaderboard || []).map((d) => d.namesPerSecond), 1);

  const toggleSort = (field: 'mean' | 'name' | 'relative' | 'peakRssMb') => {
    if (sortField === field) {
      setSortAsc(!sortAsc);
    } else {
      setSortField(field);
      setSortAsc(true);
    }
  };

  const toggleLangSelection = (lang: string) => {
    if (selectedLangs.includes(lang)) {
      if (selectedLangs.length > 1) {
        setSelectedLangs(selectedLangs.filter((l) => l !== lang));
      }
    } else {
      setSelectedLangs([...selectedLangs, lang]);
    }
  };

  const downloadJson = () => {
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'name-generator-benchmarks.json';
    a.click();
    URL.revokeObjectURL(url);
    setDownloadedJson(true);
    setTimeout(() => setDownloadedJson(false), 2000);
  };

  const downloadCsv = () => {
    const items = data.overallLeaderboard || [...data.deathmatchCompiled, ...(data.deathmatchScripting || [])];
    const headers = ['Name', 'Category', 'Paradigm', 'Mean (ms)', 'Min (ms)', 'Max (ms)', 'StdDev (ms)', 'Peak RSS (MB)', 'Startup (ms)', 'Marginal (us/name)', 'Sustained (names/s)', 'Command'];
    const rows = items.map((r) => [
      `"${r.name}"`,
      `"${r.category || ''}"`,
      `"${('paradigm' in r ? r.paradigm : '') || ''}"`,
      r.mean,
      r.min,
      r.max,
      r.stddev || '',
      r.peakRssMb || '',
      r.startupMs || '',
      r.marginalUsPerName || '',
      r.sustainedNamesPerSec || '',
      `"${r.command}"`,
    ]);
    const csvContent = [headers.join(','), ...rows.map((row) => row.join(','))].join('\n');
    const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'name-generator-benchmarks.csv';
    a.click();
    URL.revokeObjectURL(url);
    setDownloadedCsv(true);
    setTimeout(() => setDownloadedCsv(false), 2000);
  };

  const colors: Record<string, string> = {
    Zig: '#10b981', // emerald
    C: '#06b6d4', // cyan
    'C++': '#6366f1', // indigo
    Go: '#0ea5e9', // sky
    Rust: '#f97316', // orange
    Crystal: '#8b5cf6', // purple
    Nim: '#f59e0b', // amber
    Odin: '#14b8a6', // teal
    Pascal: '#a855f7', // purple
    D: '#ef4444', // red
    Dart: '#3b82f6', // blue
    Ada: '#ec4899', // pink
    COBOL: '#2563eb', // royal blue
    Fortran: '#059669', // green
    V: '#84cc16', // lime
    Java: '#d97706', // amber-600
    AWK: '#f97316', // orange
    'Node.js': '#84cc16', // lime
    TypeScript: '#3b82f6', // blue
    Python: '#38bdf8', // light blue
    Ruby: '#dc2626', // red
    Perl: '#4f46e5', // indigo
    PHP: '#7c3aed', // violet
    Lua: '#0284c7', // sky
    Tcl: '#ea580c', // orange
    'POSIX sh': '#64748b', // slate
    Bash: '#ef4444', // red
    Zsh: '#f43f5e', // rose
    Fish: '#059669', // emerald
    'KornShell (ksh)': '#d97706', // amber
    Nushell: '#10b981', // teal
  };

  return (
    <section id="benchmarks" className="py-16 md:py-24 bg-slate-50/50 dark:bg-[#0b101d]/50 border-t border-slate-200/80 dark:border-slate-800/80">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        {/* Section Header */}
        <div className="flex flex-col md:flex-row md:items-end justify-between gap-6 mb-10">
          <div>
            <div className="inline-flex items-center gap-1.5 px-3 py-1 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20 text-xs font-semibold mb-3">
              <BarChart3 className="w-3.5 h-3.5" />
              Verified Hyperfine & GNU Time Telemetry
            </div>
            <h2 className="text-3xl sm:text-4xl font-extrabold text-slate-900 dark:text-white tracking-tight">
              Performance & Memory Arena
            </h2>
            <p className="mt-2 text-slate-600 dark:text-slate-400 text-sm sm:text-base max-w-2xl">
              Rigorous benchmarking across <span className="font-semibold text-slate-900 dark:text-white">{data.stats.totalLanguages} implementations</span>.
              Measured via <code className="font-mono text-orange-500">hyperfine</code> statistical execution,
              peak RSS memory capture via <code className="font-mono text-orange-500">GNU time</code>, and OLS scaling regression.
            </p>
          </div>

          <div className="flex flex-wrap items-center gap-2">
            <a
              href="/name-generator/article/"
              className="flex items-center gap-2 px-3.5 py-2 text-xs font-medium rounded-lg bg-orange-500 hover:bg-orange-600 text-white transition-colors shadow-sm cursor-pointer"
            >
              <BookOpen className="w-3.5 h-3.5" />
              Architectural Article
            </a>
            <button
              onClick={downloadCsv}
              className="flex items-center gap-2 px-3.5 py-2 text-xs font-medium rounded-lg border border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900 hover:bg-slate-50 dark:hover:bg-slate-800 text-slate-700 dark:text-slate-300 transition-colors shadow-sm cursor-pointer"
            >
              {downloadedCsv ? <Check className="w-3.5 h-3.5 text-emerald-500" /> : <Download className="w-3.5 h-3.5" />}
              {downloadedCsv ? 'CSV Ready!' : 'Export CSV'}
            </button>
            <button
              onClick={downloadJson}
              className="flex items-center gap-2 px-3.5 py-2 text-xs font-medium rounded-lg border border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900 hover:bg-slate-50 dark:hover:bg-slate-800 text-slate-700 dark:text-slate-300 transition-colors shadow-sm cursor-pointer"
            >
              {downloadedJson ? <Check className="w-3.5 h-3.5 text-emerald-500" /> : <Download className="w-3.5 h-3.5" />}
              {downloadedJson ? 'JSON Ready!' : 'Export JSON'}
            </button>
          </div>
        </div>

        {/* Highlight Stats Banner */}
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-5 gap-4 mb-8">
          <div className="p-4 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Speed Champion</span>
              <Trophy className="w-4 h-4 text-amber-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white flex items-baseline gap-2">
              {data.stats.fastestLanguage}
              <span className="text-xs font-mono font-medium text-emerald-600 dark:text-emerald-400">
                {data.stats.fastestMeanMs} ms
              </span>
            </div>
            <div className="mt-1 text-xs text-slate-500">ReleaseFast native machine binary</div>
          </div>

          <div className="p-4 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Leanest Memory</span>
              <Cpu className="w-4 h-4 text-emerald-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white flex items-baseline gap-2">
              {data.stats.leanestMemoryLanguage || 'C'}
              <span className="text-xs font-mono font-medium text-emerald-600 dark:text-emerald-400">
                {data.stats.leanestMemoryMb || 1.57} MB RSS
              </span>
            </div>
            <div className="mt-1 text-xs text-slate-500">35x leaner than modern JVM/V8</div>
          </div>

          <div className="p-4 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Scripting Leader</span>
              <Flame className="w-4 h-4 text-orange-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white flex items-baseline gap-2">
              {data.stats.fastestScripting}
              <span className="text-xs font-mono font-medium text-orange-600 dark:text-orange-400">
                {data.stats.fastestScriptingMs} ms
              </span>
            </div>
            <div className="mt-1 text-xs text-slate-500">High-efficiency interpreted execution</div>
          </div>

          <div className="p-4 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Peak Throughput</span>
              <Zap className="w-4 h-4 text-yellow-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white">
              {(data.stats.maxThroughputPerSec ? Math.round(data.stats.maxThroughputPerSec / 1000) : 833)}K
              <span className="text-xs font-normal text-slate-500 ml-1">names/sec</span>
            </div>
            <div className="mt-1 text-xs text-slate-500">In-memory vectorized lookup</div>
          </div>

          <div className="p-4 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Tested Contenders</span>
              <Gauge className="w-4 h-4 text-sky-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white">
              {data.stats.activeBenchmarked || 29}
              <span className="text-xs font-normal text-slate-500 ml-1">/ {data.stats.totalLanguages}</span>
            </div>
            <div className="mt-1 text-xs text-slate-500">Compiled, VMs, scripts, and shells</div>
          </div>
        </div>

        {/* Tab Switcher & Filters */}
        <div className="flex flex-col lg:flex-row items-stretch lg:items-center justify-between gap-4 mb-6">
          <div className="flex flex-wrap items-center p-1 rounded-xl bg-slate-200/70 dark:bg-slate-900 border border-slate-300/60 dark:border-slate-800">
            <button
              onClick={() => setTab('compiled')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer flex items-center gap-1.5 ${
                tab === 'compiled'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              <Cpu className="w-3.5 h-3.5 text-orange-500" />
              Compiled Systems ({data.deathmatchCompiled.length})
            </button>
            <button
              onClick={() => setTab('scripting')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer flex items-center gap-1.5 ${
                tab === 'scripting'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              <Flame className="w-3.5 h-3.5 text-amber-500" />
              Scripting & Shells ({((data.deathmatchScripting?.length || 0) + (data.deathmatchShells?.length || 0))})
            </button>
            {data.deathmatchVm && data.deathmatchVm.length > 0 && (
              <button
                onClick={() => setTab('vm')}
                className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer flex items-center gap-1.5 ${
                  tab === 'vm'
                    ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                    : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
                }`}
              >
                <Layers className="w-3.5 h-3.5 text-sky-500" />
                VM Runtimes ({data.deathmatchVm.length})
              </button>
            )}
            <button
              onClick={() => setTab('memory')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer flex items-center gap-1.5 ${
                tab === 'memory'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              <Database className="w-3.5 h-3.5 text-emerald-500" />
              Peak Memory (RSS)
            </button>
            <button
              onClick={() => setTab('scaling')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer flex items-center gap-1.5 ${
                tab === 'scaling'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              <TrendingUp className="w-3.5 h-3.5 text-indigo-500" />
              Scaling Curves
            </button>
            <button
              onClick={() => setTab('throughput')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer flex items-center gap-1.5 ${
                tab === 'throughput'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              <Activity className="w-3.5 h-3.5 text-emerald-500" />
              Throughput & Scaling
            </button>
            <button
              onClick={() => setTab('scanner')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer ${
                tab === 'scanner'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              Scanner Scale
            </button>
          </div>

          <input
            type="text"
            value={search}
            onChange={(e) => setSearch(e.target.value)}
            placeholder="Search language..."
            className="px-3.5 py-1.5 text-xs rounded-xl border border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900 text-slate-800 dark:text-slate-200 placeholder:text-slate-400 focus:outline-none focus:ring-2 focus:ring-orange-500"
          />
        </div>

        {/* 1. Compiled Systems Deathmatch View */}
        {tab === 'compiled' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm overflow-hidden">
            <div className="flex items-center justify-between mb-6">
              <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2">
                Compiled Systems Leaderboard
                <span className="text-xs font-normal text-slate-500">
                  (Mean runtime across verified runs, counto=1000, lower is better)
                </span>
              </h3>
            </div>

            {/* Visual Bar Chart */}
            <div className="space-y-3 mb-8">
              {compiledList.map((item, idx) => {
                const percent = Math.max(3, (item.mean / maxCompiledMean) * 100);
                const isWinner = idx === 0 && sortField === 'mean' && sortAsc;
                return (
                  <div key={item.name} className="flex items-center gap-3 text-xs">
                    <span className="w-24 font-bold font-mono text-slate-800 dark:text-slate-200 text-right truncate">
                      {item.name}
                    </span>
                    <div className="flex-1 bg-slate-100 dark:bg-slate-900 rounded-lg h-7 p-1 overflow-hidden relative flex items-center">
                      <div
                        style={{ width: `${percent}%` }}
                        className={`h-full rounded-md transition-all duration-500 flex items-center justify-end px-2 ${
                          isWinner
                            ? 'bg-gradient-to-r from-emerald-500 to-teal-400 text-white'
                            : item.relative < 5
                            ? 'bg-gradient-to-r from-orange-500 to-amber-500 text-white'
                            : 'bg-gradient-to-r from-slate-400 to-slate-500 text-white'
                        }`}
                      >
                        <span className="font-mono text-[11px] font-bold">
                          {item.mean.toFixed(2)} ms
                        </span>
                      </div>
                    </div>
                    <span className="w-16 font-mono text-right text-slate-500 dark:text-slate-400">
                      {item.relative === 1.0 ? 'Fastest' : `${item.relative.toFixed(1)}x`}
                    </span>
                  </div>
                );
              })}
            </div>

            {/* Detailed Table */}
            <div className="overflow-x-auto">
              <table className="w-full text-left text-xs">
                <thead>
                  <tr className="border-b border-slate-200 dark:border-slate-800 text-slate-400 font-semibold uppercase tracking-wider">
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('name')}>
                      <div className="flex items-center gap-1">Language <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('mean')}>
                      <div className="flex items-center gap-1">Mean Time <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3">Min / Max</th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('peakRssMb')}>
                      <div className="flex items-center gap-1">Peak RAM <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('relative')}>
                      <div className="flex items-center gap-1">Relative <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3">Command</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-slate-100 dark:divide-slate-800/60 font-mono">
                  {compiledList.map((row, idx) => (
                    <tr key={row.name} className="hover:bg-slate-50 dark:hover:bg-slate-900/40 transition-colors">
                      <td className="py-2.5 font-bold text-slate-900 dark:text-white flex items-center gap-2">
                        {idx === 0 && <Trophy className="w-3.5 h-3.5 text-amber-500 inline" />}
                        {row.name}
                      </td>
                      <td className="py-2.5 font-semibold text-orange-600 dark:text-orange-400">
                        {row.mean.toFixed(2)} ms
                      </td>
                      <td className="py-2.5 text-slate-500">
                        {row.min.toFixed(2)} ms – {row.max.toFixed(2)} ms
                      </td>
                      <td className="py-2.5 text-slate-700 dark:text-slate-300">
                        {row.peakRssMb ? `${row.peakRssMb.toFixed(1)} MB` : '–'}
                      </td>
                      <td className="py-2.5">
                        <span className={`px-2 py-0.5 rounded-full text-[10px] font-semibold ${
                          row.relative === 1.0
                            ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400'
                            : 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400'
                        }`}>
                          {row.relative === 1.0 ? '1.00x (Baseline)' : `${row.relative.toFixed(1)}x slower`}
                        </span>
                      </td>
                      <td className="py-2.5 text-slate-400 text-[11px] truncate max-w-xs">
                        {row.command}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {/* 2. Scripting & Shells Deathmatch View */}
        {tab === 'scripting' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm overflow-hidden">
            <div className="flex items-center justify-between mb-6">
              <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2">
                Scripting & Shells Leaderboard
                <span className="text-xs font-normal text-slate-500">
                  (Measured via hyperfine, counto=100)
                </span>
              </h3>
            </div>

            {/* Visual Bar Chart */}
            <div className="space-y-3 mb-8">
              {scriptingList.map((item, idx) => {
                const percent = Math.max(3, (item.mean / maxScriptingMean) * 100);
                const isWinner = idx === 0 && sortField === 'mean' && sortAsc;
                return (
                  <div key={item.name} className="flex items-center gap-3 text-xs">
                    <span className="w-28 font-bold font-mono text-slate-800 dark:text-slate-200 text-right truncate">
                      {item.name}
                    </span>
                    <div className="flex-1 bg-slate-100 dark:bg-slate-900 rounded-lg h-7 p-1 overflow-hidden relative flex items-center">
                      <div
                        style={{ width: `${percent}%` }}
                        className={`h-full rounded-md transition-all duration-500 flex items-center justify-end px-2 ${
                          isWinner
                            ? 'bg-gradient-to-r from-emerald-500 to-teal-400 text-white'
                            : item.relative < 3
                            ? 'bg-gradient-to-r from-orange-500 to-amber-500 text-white'
                            : 'bg-gradient-to-r from-slate-400 to-slate-500 text-white'
                        }`}
                      >
                        <span className="font-mono text-[11px] font-bold">
                          {item.mean.toFixed(1)} ms
                        </span>
                      </div>
                    </div>
                    <span className="w-16 font-mono text-right text-slate-500 dark:text-slate-400">
                      {item.relative === 1.0 ? 'Fastest' : `${item.relative.toFixed(1)}x`}
                    </span>
                  </div>
                );
              })}
            </div>

            {/* Detailed Table */}
            <div className="overflow-x-auto">
              <table className="w-full text-left text-xs">
                <thead>
                  <tr className="border-b border-slate-200 dark:border-slate-800 text-slate-400 font-semibold uppercase tracking-wider">
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('name')}>
                      <div className="flex items-center gap-1">Language <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('mean')}>
                      <div className="flex items-center gap-1">Mean Time <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3">Category</th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('peakRssMb')}>
                      <div className="flex items-center gap-1">Peak RAM <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('relative')}>
                      <div className="flex items-center gap-1">Relative <ArrowUpDown className="w-3 h-3" /></div>
                    </th>
                    <th className="pb-3">Command</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-slate-100 dark:divide-slate-800/60 font-mono">
                  {scriptingList.map((row, idx) => (
                    <tr key={row.name} className="hover:bg-slate-50 dark:hover:bg-slate-900/40 transition-colors">
                      <td className="py-2.5 font-bold text-slate-900 dark:text-white flex items-center gap-2">
                        {idx === 0 && <Trophy className="w-3.5 h-3.5 text-amber-500 inline" />}
                        {row.name}
                      </td>
                      <td className="py-2.5 font-semibold text-orange-600 dark:text-orange-400">
                        {row.mean.toFixed(1)} ms
                      </td>
                      <td className="py-2.5 text-slate-500">
                        <span className="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800 text-[10px]">
                          {row.category || row.type || 'Scripting'}
                        </span>
                      </td>
                      <td className="py-2.5 text-slate-700 dark:text-slate-300">
                        {row.peakRssMb ? `${row.peakRssMb.toFixed(1)} MB` : '–'}
                      </td>
                      <td className="py-2.5">
                        <span className={`px-2 py-0.5 rounded-full text-[10px] font-semibold ${
                          row.relative === 1.0
                            ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400'
                            : 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400'
                        }`}>
                          {row.relative === 1.0 ? '1.00x' : `${row.relative.toFixed(1)}x slower`}
                        </span>
                      </td>
                      <td className="py-2.5 text-slate-400 text-[11px] truncate max-w-xs">
                        {row.command}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {/* 3. VM Runtimes View */}
        {tab === 'vm' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm overflow-hidden">
            <h3 className="text-base font-bold text-slate-900 dark:text-white mb-2">
              Virtual Machine Runtimes Leaderboard
            </h3>
            <p className="text-xs text-slate-500 mb-6">
              Bytecode execution and JIT startup overhead (JVM, BEAM, etc., counto=500).
            </p>

            <div className="overflow-x-auto">
              <table className="w-full text-left text-xs font-mono">
                <thead>
                  <tr className="border-b border-slate-200 dark:border-slate-800 text-slate-400 font-semibold uppercase">
                    <th className="pb-3">Language</th>
                    <th className="pb-3">Mean Runtime</th>
                    <th className="pb-3">Min / Max</th>
                    <th className="pb-3">Peak Memory RSS</th>
                    <th className="pb-3">Paradigm</th>
                    <th className="pb-3">Command</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-slate-100 dark:divide-slate-800/60">
                  {vmList.map((row) => (
                    <tr key={row.name} className="hover:bg-slate-50 dark:hover:bg-slate-900/40">
                      <td className="py-3 font-bold text-slate-900 dark:text-white">{row.name}</td>
                      <td className="py-3 font-bold text-orange-600 dark:text-orange-400">{row.mean.toFixed(1)} ms</td>
                      <td className="py-3 text-slate-500">{row.min.toFixed(1)} ms – {row.max.toFixed(1)} ms</td>
                      <td className="py-3 text-slate-700 dark:text-slate-300 font-semibold">{row.peakRssMb ? `${row.peakRssMb.toFixed(1)} MB` : '–'}</td>
                      <td className="py-3 text-slate-500">{row.paradigm}</td>
                      <td className="py-3 text-slate-400 text-[11px] truncate max-w-xs">{row.command}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {/* 4. Peak Memory RSS Leaderboard View */}
        {tab === 'memory' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm overflow-hidden">
            <div className="flex items-center justify-between mb-4">
              <div>
                <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2">
                  Memory Footprint Leaderboard (Peak RSS)
                </h3>
                <p className="text-xs text-slate-500 mt-1">
                  Peak resident set size measured directly from kernel page tables via <code className="font-mono text-orange-500">GNU time</code>. Lower is leaner.
                </p>
              </div>
            </div>

            {/* Visual Bar Chart */}
            <div className="space-y-2.5 mb-8">
              {memoryList.map((item, idx) => {
                const percent = Math.max(3, (item.peakRssMb / maxMemoryMb) * 100);
                const isLeanest = idx === 0 && sortAsc;
                return (
                  <div key={item.name} className="flex items-center gap-3 text-xs">
                    <span className="w-28 font-bold font-mono text-slate-800 dark:text-slate-200 text-right truncate">
                      {item.name}
                    </span>
                    <div className="flex-1 bg-slate-100 dark:bg-slate-900 rounded-lg h-6 p-0.5 overflow-hidden relative flex items-center">
                      <div
                        style={{ width: `${percent}%` }}
                        className={`h-full rounded-md transition-all duration-500 flex items-center justify-end px-2 ${
                          isLeanest
                            ? 'bg-gradient-to-r from-emerald-500 to-teal-400 text-white'
                            : item.peakRssMb < 10
                            ? 'bg-gradient-to-r from-sky-500 to-cyan-400 text-white'
                            : item.peakRssMb < 30
                            ? 'bg-gradient-to-r from-amber-500 to-orange-400 text-white'
                            : 'bg-gradient-to-r from-rose-500 to-pink-500 text-white'
                        }`}
                      >
                        <span className="font-mono text-[10px] font-bold">
                          {item.peakRssMb.toFixed(1)} MB
                        </span>
                      </div>
                    </div>
                    <span className="w-20 font-mono text-right text-slate-500 dark:text-slate-400">
                      {item.peakRssKb.toLocaleString()} KB
                    </span>
                  </div>
                );
              })}
            </div>

            {/* Explanatory Callout */}
            <div className="p-4 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80 text-xs text-slate-600 dark:text-slate-300 flex items-start gap-3">
              <Cpu className="w-4 h-4 text-emerald-500 shrink-0 mt-0.5" />
              <div>
                <strong className="text-slate-900 dark:text-white">Why memory consumption varies from 1.5 MB to 76 MB:</strong>
                <p className="mt-1">
                  Native languages with zero-cost runtimes (C, Zig, Nim, POSIX sh) map only essential static dictionary buffers and glibc runtime structures. In contrast, dynamic JIT engines (Node.js V8, TypeScript, JVM) spin up garbage collector heaps, JIT compilation caches, and isolate thread pools that require tens of megabytes of resident memory regardless of workload size.
                </p>
              </div>
            </div>
          </div>
        )}

        {/* 5. Scaling Curves View */}
        {tab === 'scaling' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm">
            <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-4 mb-6">
              <div>
                <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2">
                  Multi-Scale Scaling Curves (N = 1, 10, 100, 1000)
                </h3>
                <p className="text-xs text-slate-500 mt-1">
                  How execution time evolves as output count increases. Systems languages stay flat; process-spawning shells spike exponentially.
                </p>
              </div>

              {/* Language Selector Chips */}
              <div className="flex flex-wrap items-center gap-1.5 max-w-xl">
                {(data.scalingCurves || []).map((c) => {
                  const isSelected = selectedLangs.includes(c.language);
                  const color = colors[c.language] || '#94a3b8';
                  return (
                    <button
                      key={c.language}
                      onClick={() => toggleLangSelection(c.language)}
                      className={`px-2.5 py-1 rounded-lg text-xs font-mono font-medium transition-all cursor-pointer flex items-center gap-1.5 border ${
                        isSelected
                          ? 'border-transparent text-white shadow-sm'
                          : 'border-slate-200 dark:border-slate-800 text-slate-500 hover:text-slate-800 dark:hover:text-slate-200'
                      }`}
                      style={{
                        backgroundColor: isSelected ? color : 'transparent',
                      }}
                    >
                      <span
                        className="w-2 h-2 rounded-full inline-block"
                        style={{ backgroundColor: isSelected ? '#fff' : color }}
                      />
                      {c.language}
                    </button>
                  );
                })}
              </div>
            </div>

            {/* Matrix comparison table across N */}
            <div className="overflow-x-auto">
              <table className="w-full text-left text-xs font-mono">
                <thead>
                  <tr className="border-b border-slate-200 dark:border-slate-800 text-slate-400 font-semibold uppercase">
                    <th className="pb-3 font-sans">Language</th>
                    <th className="pb-3 text-right">count=1</th>
                    <th className="pb-3 text-right">count=10</th>
                    <th className="pb-3 text-right">count=100</th>
                    <th className="pb-3 text-right">count=1,000</th>
                    <th className="pb-3 text-right font-sans">Scaling Behavior</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-slate-100 dark:divide-slate-800/60">
                  {(data.scalingCurves || [])
                    .filter((c) => selectedLangs.includes(c.language))
                    .map((curve) => {
                      const p1 = curve.points.find((p) => p.count === 1)?.meanMs || 0;
                      const p10 = curve.points.find((p) => p.count === 10)?.meanMs || 0;
                      const p100 = curve.points.find((p) => p.count === 100)?.meanMs || 0;
                      const p1000 = curve.points.find((p) => p.count === 1000)?.meanMs || 0;
                      const ratio = p1 > 0 ? (p1000 / p1).toFixed(1) : '1.0';
                      const color = colors[curve.language] || '#94a3b8';

                      return (
                        <tr key={curve.language} className="hover:bg-slate-50 dark:hover:bg-slate-900/40">
                          <td className="py-3 font-bold text-slate-900 dark:text-white flex items-center gap-2">
                            <span className="w-2.5 h-2.5 rounded-full" style={{ backgroundColor: color }} />
                            {curve.language}
                          </td>
                          <td className="py-3 text-right text-slate-700 dark:text-slate-300">{p1.toFixed(2)} ms</td>
                          <td className="py-3 text-right text-slate-700 dark:text-slate-300">{p10.toFixed(2)} ms</td>
                          <td className="py-3 text-right text-slate-700 dark:text-slate-300">{p100.toFixed(2)} ms</td>
                          <td className="py-3 text-right font-bold" style={{ color }}>{p1000.toFixed(2)} ms</td>
                          <td className="py-3 text-right font-sans">
                            <span className={`px-2 py-0.5 rounded text-[10px] font-semibold ${
                              Number(ratio) < 2
                                ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400'
                                : Number(ratio) < 10
                                ? 'bg-amber-500/10 text-amber-600 dark:text-amber-400'
                                : 'bg-rose-500/10 text-rose-600 dark:text-rose-400'
                            }`}>
                              {Number(ratio) < 1.5 ? 'Flat O(1) in-memory' : `${ratio}x growth from N=1 to 1000`}
                            </span>
                          </td>
                        </tr>
                      );
                    })}
                </tbody>
              </table>
            </div>

            <div className="mt-6 p-4 rounded-xl bg-orange-500/5 border border-orange-500/20 text-xs text-slate-600 dark:text-slate-300 flex items-start gap-3">
              <Zap className="w-4 h-4 text-orange-500 shrink-0 mt-0.5" />
              <div>
                <strong className="text-slate-900 dark:text-white">Why the dramatic divergence?</strong> Compiled systems pre-load dictionary vectors into memory once and generate 1,000 names via random index lookups in fractions of a millisecond. Traditional shell loops invoke subshells or pipeline forks per name, multiplying system call and fork/exec overhead.
              </div>
            </div>
          </div>
        )}

        {/* 6. Throughput & Sustained Scaling View */}
        {tab === 'throughput' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm">
            <h3 className="text-base font-bold text-slate-900 dark:text-white mb-2">
              Throughput & Scaling Decomposition (OLS Linear Regression)
            </h3>
            <p className="text-xs text-slate-500 mb-6">
              Decomposes total runtime into <span className="font-semibold text-slate-900 dark:text-white">Cold Startup Overhead (T_startup)</span> and <span className="font-semibold text-slate-900 dark:text-white">Marginal Incremental Cost (t_marginal per name)</span> via OLS regression: <code className="font-mono text-orange-500">T(N) = T_startup + N × t_marginal</code>.
            </p>

            <div className="overflow-x-auto">
              <table className="w-full text-left text-xs font-mono">
                <thead>
                  <tr className="border-b border-slate-200 dark:border-slate-800 text-slate-400 font-semibold uppercase">
                    <th className="pb-3 font-sans">Language</th>
                    <th className="pb-3 font-sans">Category</th>
                    <th className="pb-3 text-right">Throughput (1k batch)</th>
                    <th className="pb-3 text-right">Startup Overhead</th>
                    <th className="pb-3 text-right">Marginal Cost</th>
                    <th className="pb-3 text-right">Peak RAM</th>
                    <th className="pb-3 text-right font-sans">Relative</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-slate-100 dark:divide-slate-800/60">
                  {(data.throughputLeaderboard || []).map((item, idx) => {
                    return (
                      <tr key={item.name} className="hover:bg-slate-50 dark:hover:bg-slate-900/40">
                        <td className="py-3 font-bold text-slate-900 dark:text-white flex items-center gap-2">
                          {idx === 0 && <Trophy className="w-3.5 h-3.5 text-amber-500 inline" />}
                          {item.name}
                        </td>
                        <td className="py-3 text-slate-500 font-sans">
                          <span className="px-2 py-0.5 rounded bg-slate-100 dark:bg-slate-800 text-[10px]">
                            {item.category}
                          </span>
                        </td>
                        <td className="py-3 text-right font-bold text-emerald-600 dark:text-emerald-400">
                          {item.namesPerSecond.toLocaleString()} names/s
                        </td>
                        <td className="py-3 text-right text-slate-700 dark:text-slate-300">
                          {item.startupMs !== undefined ? `${item.startupMs.toFixed(2)} ms` : '–'}
                        </td>
                        <td className="py-3 text-right text-slate-700 dark:text-slate-300">
                          {item.marginalUsPerName !== undefined ? `${item.marginalUsPerName.toFixed(3)} µs/name` : '–'}
                        </td>
                        <td className="py-3 text-right text-slate-700 dark:text-slate-300">
                          {item.peakRssMb ? `${item.peakRssMb.toFixed(1)} MB` : '–'}
                        </td>
                        <td className="py-3 text-right font-sans">
                          <span className={`px-2 py-0.5 rounded-full text-[10px] font-semibold ${
                            item.relativeToFastest === 1.0
                              ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400'
                              : 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400'
                          }`}>
                            {item.relativeToFastest === 1.0 ? 'Fastest' : `${item.relativeToFastest.toFixed(1)}x slower`}
                          </span>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {/* 7. Scanner Scale View */}
        {tab === 'scanner' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm">
            <h3 className="text-base font-bold text-slate-900 dark:text-white mb-2">
              Multi-Scale Exponential Scaling (2<sup>1</sup> to 2<sup>24</sup>)
            </h3>
            <p className="text-xs text-slate-500 mb-6">
              Evaluation of in-memory caching vs re-opening wordlists across exponential batch counts.
              Compiled binaries with pre-loaded vectors maintain sub-millisecond per-name throughput.
            </p>

            <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
              <div className="p-4 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80">
                <div className="text-xs font-semibold text-slate-500 mb-1">Small Batches (2<sup>1</sup> - 2<sup>5</sup>)</div>
                <div className="text-sm font-bold text-slate-900 dark:text-white">Startup Bound</div>
                <p className="text-[11px] text-slate-500 mt-1">Zig, C, Go, and Crystal lead with startup times under 2ms.</p>
              </div>
              <div className="p-4 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80">
                <div className="text-xs font-semibold text-slate-500 mb-1">Medium Batches (2<sup>6</sup> - 2<sup>12</sup>)</div>
                <div className="text-sm font-bold text-slate-900 dark:text-white">Memory Allocation Bound</div>
                <p className="text-[11px] text-slate-500 mt-1">Implementations with bulk buffered stdout outpace line-by-line writers.</p>
              </div>
              <div className="p-4 rounded-xl bg-slate-50 dark:bg-slate-900/60 border border-slate-200/80 dark:border-slate-800/80">
                <div className="text-xs font-semibold text-slate-500 mb-1">Massive Batches (2<sup>15</sup> - 2<sup>24</sup>)</div>
                <div className="text-sm font-bold text-slate-900 dark:text-white">I/O Pipe Saturation</div>
                <p className="text-[11px] text-slate-500 mt-1">Up to 16M names generated per run; CPU registers saturate pipe buffer.</p>
              </div>
            </div>

            <div className="flex items-center justify-between text-xs text-slate-500 pt-4 border-t border-slate-100 dark:border-slate-800">
              <span>Historical CSV files available in <code className="font-mono text-orange-500">public/data/*.csv</code></span>
              <a
                href="https://github.com/joshuacox/name-generator/tree/main/docs"
                target="_blank"
                rel="noreferrer"
                className="flex items-center gap-1 text-orange-500 hover:underline"
              >
                View full dataset on GitHub <ExternalLink className="w-3 h-3" />
              </a>
            </div>
          </div>
        )}
      </div>
    </section>
  );
};
