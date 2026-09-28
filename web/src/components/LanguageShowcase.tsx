'use client';

import React, { useState, useMemo } from 'react';
import { Code2, ExternalLink, ChevronDown, ChevronUp, FileCode, Layers, Search } from 'lucide-react';
import benchmarksData from '../data/benchmarks.json';
import { LanguageMeta } from '../types';

const allLanguages = (benchmarksData as unknown as { languages: LanguageMeta[] }).languages;

export const LanguageShowcase: React.FC = () => {
  const [selectedCategory, setSelectedCategory] = useState<string>('All');
  const [search, setSearch] = useState<string>('');
  const [expandedId, setExpandedId] = useState<string | null>(null);

  const categories = ['All', 'Compiled', 'Shell', 'Scripting', 'VM', 'WIP', 'Esoteric'];

  const filteredLanguages = useMemo(() => {
    return allLanguages.filter((lang) => {
      const matchesCategory =
        selectedCategory === 'All' || lang.category === selectedCategory;
      const matchesSearch =
        lang.name.toLowerCase().includes(search.toLowerCase()) ||
        lang.paradigm.toLowerCase().includes(search.toLowerCase()) ||
        lang.file.toLowerCase().includes(search.toLowerCase());
      return matchesCategory && matchesSearch;
    });
  }, [selectedCategory, search]);

  const toggleExpand = (id: string) => {
    setExpandedId(expandedId === id ? null : id);
  };

  return (
    <section id="languages" className="py-16 md:py-24 border-t border-slate-200 dark:border-slate-800">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex flex-col md:flex-row md:items-end justify-between gap-6 mb-10">
          <div>
            <div className="inline-flex items-center gap-1.5 px-3 py-1 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20 text-xs font-semibold mb-3">
              <Layers className="w-3.5 h-3.5" />
              Polyglot Repository
            </div>
            <h2 className="text-3xl sm:text-4xl font-extrabold text-slate-900 dark:text-white tracking-tight">
              The Language Roster ({allLanguages.length} Implementations)
            </h2>
            <p className="mt-2 text-slate-600 dark:text-slate-400 text-sm sm:text-base max-w-2xl">
              From Fortran 90 and Ada to modern systems languages like Zig and Odin, and unix shells to esoteric brainfuck.
              Each implementation adheres strictly to the same environment and case conventions.
            </p>
          </div>

          {/* Search Box */}
          <div className="relative w-full md:w-72">
            <Search className="w-4 h-4 absolute left-3.5 top-1/2 -translate-y-1/2 text-slate-400" />
            <input
              type="text"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Search language, paradigm, file..."
              className="w-full pl-10 pr-3 py-2 text-xs rounded-xl border border-slate-200 dark:border-slate-800 bg-white dark:bg-slate-900 text-slate-800 dark:text-slate-200 placeholder:text-slate-400 focus:outline-none focus:ring-2 focus:ring-orange-500"
            />
          </div>
        </div>

        {/* Filter Pills */}
        <div className="flex flex-wrap items-center gap-2 mb-8">
          {categories.map((cat) => (
            <button
              key={cat}
              onClick={() => setSelectedCategory(cat)}
              className={`px-3.5 py-1.5 rounded-full text-xs font-medium transition-all cursor-pointer ${
                selectedCategory === cat
                  ? 'bg-orange-500 text-white shadow-sm shadow-orange-500/20'
                  : 'bg-slate-100 dark:bg-slate-800/80 text-slate-600 dark:text-slate-400 hover:bg-slate-200 dark:hover:bg-slate-800 hover:text-slate-900 dark:hover:text-white'
              }`}
            >
              {cat}
              {cat === 'All' ? ` (${allLanguages.length})` : ''}
            </button>
          ))}
        </div>

        {/* Grid of Language Cards */}
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
          {filteredLanguages.map((lang) => {
            const isExpanded = expandedId === lang.id;
            return (
              <div
                key={lang.id}
                className="rounded-2xl border border-slate-200 dark:border-slate-800 bg-white dark:bg-[#131b2e] shadow-sm hover:shadow-md transition-shadow overflow-hidden flex flex-col"
              >
                <div className="p-5 flex-1">
                  <div className="flex items-start justify-between gap-3 mb-3">
                    <div>
                      <h3 className="text-lg font-bold text-slate-900 dark:text-white flex items-center gap-2">
                        {lang.name}
                        <span className="text-[10px] font-mono px-2 py-0.5 rounded-md bg-slate-100 dark:bg-slate-800 text-slate-600 dark:text-slate-400 font-normal">
                          {lang.ext}
                        </span>
                      </h3>
                      <p className="text-xs text-slate-500 dark:text-slate-400 mt-0.5">
                        {lang.paradigm}
                      </p>
                    </div>
                    <span
                      className={`px-2.5 py-1 rounded-full text-[10px] font-semibold tracking-wide uppercase ${
                        lang.category === 'Compiled'
                          ? 'bg-emerald-500/10 text-emerald-600 dark:text-emerald-400 border border-emerald-500/20'
                          : lang.category === 'Shell'
                          ? 'bg-sky-500/10 text-sky-600 dark:text-sky-400 border border-sky-500/20'
                          : lang.category === 'Scripting'
                          ? 'bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20'
                          : lang.category === 'VM'
                          ? 'bg-purple-500/10 text-purple-600 dark:text-purple-400 border border-purple-500/20'
                          : 'bg-slate-200/60 dark:bg-slate-800 text-slate-600 dark:text-slate-400'
                      }`}
                    >
                      {lang.category}
                    </span>
                  </div>

                  <div className="flex items-center gap-4 text-xs font-mono text-slate-500 dark:text-slate-400 pt-3 border-t border-slate-100 dark:border-slate-800/80">
                    <span className="flex items-center gap-1.5 truncate">
                      <FileCode className="w-3.5 h-3.5 text-slate-400" />
                      {lang.file}
                    </span>
                    {lang.sloc ? (
                      <span className="ml-auto text-[11px] font-medium text-slate-400">
                        {lang.sloc} lines
                      </span>
                    ) : null}
                  </div>
                </div>

                {/* Card Footer / Drawer Trigger */}
                <div className="px-5 py-2.5 bg-slate-50/80 dark:bg-slate-900/60 border-t border-slate-100 dark:border-slate-800/80 flex items-center justify-between text-xs">
                  <button
                    onClick={() => toggleExpand(lang.id)}
                    className="flex items-center gap-1 text-slate-600 dark:text-slate-400 hover:text-orange-500 dark:hover:text-orange-400 font-medium transition-colors cursor-pointer"
                  >
                    <Code2 className="w-3.5 h-3.5" />
                    {isExpanded ? 'Hide Code' : 'Peek Code'}
                    {isExpanded ? (
                      <ChevronUp className="w-3.5 h-3.5" />
                    ) : (
                      <ChevronDown className="w-3.5 h-3.5" />
                    )}
                  </button>

                  <a
                    href={lang.githubUrl}
                    target="_blank"
                    rel="noreferrer"
                    className="flex items-center gap-1 text-orange-500 hover:text-orange-600 font-medium"
                  >
                    GitHub <ExternalLink className="w-3 h-3" />
                  </a>
                </div>

                {/* Expandable Code Drawer */}
                {isExpanded && lang.codeSample && (
                  <div className="p-4 bg-slate-950 text-slate-200 font-mono text-[11px] overflow-x-auto border-t border-slate-800 max-h-60">
                    <pre className="leading-relaxed">
                      <code>{lang.codeSample}</code>
                    </pre>
                  </div>
                )}
              </div>
            );
          })}
        </div>

        {filteredLanguages.length === 0 && (
          <div className="text-center py-16 text-slate-400 text-sm">
            No languages found matching "{search}" in category "{selectedCategory}"
          </div>
        )}
      </div>
    </section>
  );
};
