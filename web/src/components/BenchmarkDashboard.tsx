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
} from 'lucide-react';
import benchmarksData from '../data/benchmarks.json';
import { BenchmarkData } from '../types';

const data = benchmarksData as unknown as BenchmarkData;

export const BenchmarkDashboard: React.FC = () => {
  const [tab, setTab] = useState<'compiled' | 'scripting' | 'scaling' | 'throughput' | 'scanner'>('compiled');
  const [sortField, setSortField] = useState<'mean' | 'name' | 'relative'>('mean');
  const [sortAsc, setSortAsc] = useState<boolean>(true);
  const [search, setSearch] = useState<string>('');
  const [downloaded, setDownloaded] = useState<boolean>(false);

  // Selected languages for the scaling curve chart
  const [selectedLangs, setSelectedLangs] = useState<string[]>([
    'Zig',
    'Go',
    'Crystal',
    'Ada',
    'AWK',
    'Bash',
  ]);

  const compiledList = useMemo(() => {
    return [...data.deathmatchCompiled]
      .filter((item) => item.name.toLowerCase().includes(search.toLowerCase()))
      .sort((a, b) => {
        let diff = 0;
        if (sortField === 'mean') diff = a.mean - b.mean;
        else if (sortField === 'relative') diff = a.relative - b.relative;
        else diff = a.name.localeCompare(b.name);
        return sortAsc ? diff : -diff;
      });
  }, [search, sortField, sortAsc]);

  const scriptingList = useMemo(() => {
    return [...data.deathmatchScripting]
      .filter((item) => item.name.toLowerCase().includes(search.toLowerCase()))
      .sort((a, b) => {
        let diff = 0;
        if (sortField === 'mean') diff = a.mean - b.mean;
        else if (sortField === 'relative') diff = a.relative - b.relative;
        else diff = a.name.localeCompare(b.name);
        return sortAsc ? diff : -diff;
      });
  }, [search, sortField, sortAsc]);

  const maxCompiledMean = Math.max(...data.deathmatchCompiled.map((d) => d.mean), 1);
  const maxScriptingMean = Math.max(...data.deathmatchScripting.map((d) => d.mean), 1);
  const maxThroughput = Math.max(...(data.throughputLeaderboard || []).map((d) => d.namesPerSecond), 1);

  const toggleSort = (field: 'mean' | 'name' | 'relative') => {
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
    setDownloaded(true);
    setTimeout(() => setDownloaded(false), 2000);
  };

  const colors: Record<string, string> = {
    Zig: '#10b981', // emerald
    Go: '#0ea5e9', // sky
    Crystal: '#8b5cf6', // purple
    Nim: '#f59e0b', // amber
    Odin: '#06b6d4', // cyan
    Ada: '#ec4899', // pink
    COBOL: '#3b82f6', // blue
    AWK: '#f97316', // orange
    'Node.js': '#84cc16', // lime
    Bash: '#ef4444', // red
  };

  return (
    <section id="benchmarks" className="py-16 md:py-24 bg-slate-50/50 dark:bg-[#0b101d]/50 border-t border-slate-200/80 dark:border-slate-800/80">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        {/* Section Header */}
        <div className="flex flex-col md:flex-row md:items-end justify-between gap-6 mb-10">
          <div>
            <div className="inline-flex items-center gap-1.5 px-3 py-1 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20 text-xs font-semibold mb-3">
              <BarChart3 className="w-3.5 h-3.5" />
              Verified Hyperfine Deathmatch
            </div>
            <h2 className="text-3xl sm:text-4xl font-extrabold text-slate-900 dark:text-white tracking-tight">
              Performance Deathmatch
            </h2>
            <p className="mt-2 text-slate-600 dark:text-slate-400 text-sm sm:text-base max-w-2xl">
              Strictly measured on identical hardware using <code className="font-mono text-orange-500">hyperfine</code> with
              cache warmups, statistical outlier detection, and zero shell overhead.
            </p>
          </div>

          <div className="flex items-center gap-3">
            <a
              href="/name-generator/article/"
              className="flex items-center gap-2 px-3.5 py-2 text-xs font-medium rounded-lg bg-orange-500 hover:bg-orange-600 text-white transition-colors shadow-sm cursor-pointer"
            >
              <BookOpen className="w-3.5 h-3.5" />
              Read Architectural Deep Dive
            </a>
            <button
              onClick={downloadJson}
              className="flex items-center gap-2 px-3.5 py-2 text-xs font-medium rounded-lg border border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900 hover:bg-slate-50 dark:hover:bg-slate-800 text-slate-700 dark:text-slate-300 transition-colors shadow-sm cursor-pointer"
            >
              {downloaded ? <Check className="w-3.5 h-3.5 text-emerald-500" /> : <Download className="w-3.5 h-3.5" />}
              {downloaded ? 'Downloaded!' : 'Export JSON'}
            </button>
          </div>
        </div>

        {/* Highlight Stats Banner */}
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
          <div className="p-5 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Compiled Champion</span>
              <Trophy className="w-4 h-4 text-amber-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white flex items-baseline gap-2">
              Zig
              <span className="text-xs font-mono font-medium text-emerald-600 dark:text-emerald-400">
                1.2 ms (1.00x)
              </span>
            </div>
            <div className="mt-1 text-xs text-slate-500">ReleaseFast zero-overhead allocator</div>
          </div>

          <div className="p-5 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Scripting Champion</span>
              <Flame className="w-4 h-4 text-orange-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white flex items-baseline gap-2">
              AWK
              <span className="text-xs font-mono font-medium text-emerald-600 dark:text-emerald-400">
                19.3 ms
              </span>
            </div>
            <div className="mt-1 text-xs text-slate-500">10x faster than Bash, beats Node.js</div>
          </div>

          <div className="p-5 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Peak Throughput</span>
              <Zap className="w-4 h-4 text-yellow-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white">
              833K <span className="text-sm font-normal text-slate-500">names/sec</span>
            </div>
            <div className="mt-1 text-xs text-slate-500">2,000x faster than Bash process loops</div>
          </div>

          <div className="p-5 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm">
            <div className="flex items-center justify-between text-slate-500 dark:text-slate-400 text-xs font-semibold uppercase tracking-wider mb-2">
              <span>Total Contenders</span>
              <Gauge className="w-4 h-4 text-sky-500" />
            </div>
            <div className="text-2xl font-black text-slate-900 dark:text-white">
              {data.stats.totalLanguages} Languages
            </div>
            <div className="mt-1 text-xs text-slate-500">Systems, scripts, shells, & VMs</div>
          </div>
        </div>

        {/* Tab Switcher & Filters */}
        <div className="flex flex-col lg:flex-row items-stretch lg:items-center justify-between gap-4 mb-6">
          <div className="flex flex-wrap items-center p-1 rounded-xl bg-slate-200/70 dark:bg-slate-900 border border-slate-300/60 dark:border-slate-800">
            <button
              onClick={() => setTab('compiled')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer ${
                tab === 'compiled'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              Compiled Systems
            </button>
            <button
              onClick={() => setTab('scripting')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer ${
                tab === 'scripting'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              Scripting & Shells
            </button>
            <button
              onClick={() => setTab('scaling')}
              className={`px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all cursor-pointer flex items-center gap-1.5 ${
                tab === 'scaling'
                  ? 'bg-white dark:bg-[#131b2e] text-slate-900 dark:text-white shadow-sm'
                  : 'text-slate-600 dark:text-slate-400 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              <TrendingUp className="w-3.5 h-3.5 text-orange-500" />
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
              Throughput (Names/s)
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

          {(tab === 'compiled' || tab === 'scripting') && (
            <input
              type="text"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Search language..."
              className="px-3.5 py-1.5 text-xs rounded-xl border border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900 text-slate-800 dark:text-slate-200 placeholder:text-slate-400 focus:outline-none focus:ring-2 focus:ring-orange-500"
            />
          )}
        </div>

        {/* Compiled Systems Deathmatch View */}
        {tab === 'compiled' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm overflow-hidden">
            <div className="flex items-center justify-between mb-6">
              <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2">
                Compiled Systems Leaderboard
                <span className="text-xs font-normal text-slate-500">
                  (Mean runtime across 10 runs, counto=1000, lower is better)
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
                    <span className="w-20 font-bold font-mono text-slate-800 dark:text-slate-200 text-right truncate">
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
                    <th className="pb-3">Min / Max</th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('relative')}>
                      <div className="flex items-center gap-1">Relative to #1 <ArrowUpDown className="w-3 h-3" /></div>
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
                      <td className="py-2.5">
                        <span className={`px-2 py-0.5 rounded-full text-[10px] font-semibold ${
                          row.relative === 1.0
                            ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400'
                            : 'bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400'
                        }`}>
                          {row.relative === 1.0 ? '1.00x (Baseline)' : `${row.relative.toFixed(2)}x slower`}
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

        {/* Scripting & Shells Deathmatch View */}
        {tab === 'scripting' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm overflow-hidden">
            <div className="flex items-center justify-between mb-6">
              <h3 className="text-base font-bold text-slate-900 dark:text-white flex items-center gap-2">
                Scripting & Shells Leaderboard
                <span className="text-xs font-normal text-slate-500">
                  (Mean runtime across 5 runs, counto=100)
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
                    <span className="w-24 font-bold font-mono text-slate-800 dark:text-slate-200 text-right truncate">
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
                    <th className="pb-3">Type</th>
                    <th className="pb-3 cursor-pointer" onClick={() => toggleSort('relative')}>
                      <div className="flex items-center gap-1">Relative to AWK <ArrowUpDown className="w-3 h-3" /></div>
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
                          {row.type}
                        </span>
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

        {/* Scaling Curves View */}
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
              <div className="flex flex-wrap items-center gap-1.5">
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

        {/* Throughput Leaderboard View */}
        {tab === 'throughput' && (
          <div className="bg-white dark:bg-[#131b2e] rounded-2xl border border-slate-200 dark:border-slate-800 p-6 shadow-sm">
            <h3 className="text-base font-bold text-slate-900 dark:text-white mb-2">
              Throughput Leaderboard (Names Generated Per Second)
            </h3>
            <p className="text-xs text-slate-500 mb-6">
              Calculated at batch size <code className="font-mono text-orange-500">counto=1000</code>. Higher is better.
            </p>

            <div className="space-y-4 mb-8">
              {(data.throughputLeaderboard || []).map((item, idx) => {
                const percent = Math.max(2, (item.namesPerSecond / maxThroughput) * 100);
                return (
                  <div key={item.name} className="flex items-center gap-3 text-xs">
                    <span className="w-24 font-bold font-mono text-slate-800 dark:text-slate-200 text-right truncate">
                      {item.name}
                    </span>
                    <div className="flex-1 bg-slate-100 dark:bg-slate-900 rounded-lg h-8 p-1 overflow-hidden relative flex items-center">
                      <div
                        style={{ width: `${percent}%` }}
                        className={`h-full rounded-md transition-all duration-500 flex items-center justify-end px-3 ${
                          idx === 0
                            ? 'bg-gradient-to-r from-emerald-500 to-teal-400 text-white font-bold'
                            : item.namesPerSecond > 100000
                            ? 'bg-gradient-to-r from-orange-500 to-amber-500 text-white font-semibold'
                            : 'bg-gradient-to-r from-slate-400 to-slate-500 text-white font-medium'
                        }`}
                      >
                        <span className="font-mono text-xs">
                          {item.namesPerSecond.toLocaleString()} names/s
                        </span>
                      </div>
                    </div>
                    <span className="w-24 font-mono text-right text-slate-500 dark:text-slate-400">
                      {item.relativeToFastest === 1 ? 'Peak Speed' : `${item.relativeToFastest.toFixed(1)}x slower`}
                    </span>
                  </div>
                );
              })}
            </div>
          </div>
        )}

        {/* Scanner Scale View */}
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
