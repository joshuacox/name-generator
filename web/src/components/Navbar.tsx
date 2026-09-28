'use client';

import React, { useEffect, useState } from 'react';
import { Sun, Moon, Zap, BarChart2, BookOpen, Code2 } from 'lucide-react';
import { GithubIcon } from './icons';

export const Navbar: React.FC = () => {
  const [theme, setTheme] = useState<'light' | 'dark'>('dark');
  const [mounted, setMounted] = useState(false);

  useEffect(() => {
    setMounted(true);
    const currentTheme =
      document.documentElement.getAttribute('data-theme') === 'light' ? 'light' : 'dark';
    setTheme(currentTheme);
  }, []);

  const toggleTheme = () => {
    const nextTheme = theme === 'dark' ? 'light' : 'dark';
    setTheme(nextTheme);
    document.documentElement.setAttribute('data-theme', nextTheme);
    try {
      localStorage.setItem('theme', nextTheme);
    } catch (_) {}
  };

  return (
    <header className="sticky top-0 z-50 backdrop-blur-md bg-white/70 dark:bg-[#090d16]/70 border-b border-slate-200 dark:border-slate-800 transition-colors">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 h-16 flex items-center justify-between">
        <a href="#" className="flex items-center gap-2 group">
          <div className="w-9 h-9 rounded-lg bg-gradient-to-tr from-orange-600 to-amber-500 flex items-center justify-center text-white shadow-md shadow-orange-500/20 group-hover:scale-105 transition-transform">
            <Zap className="w-5 h-5 fill-current" />
          </div>
          <div className="flex flex-col">
            <span className="font-bold text-lg leading-tight tracking-tight text-slate-900 dark:text-white flex items-center gap-1.5">
              name-generator
              <span className="text-[10px] uppercase font-mono px-1.5 py-0.5 rounded bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20">
                v2.0
              </span>
            </span>
            <span className="text-[11px] text-slate-500 dark:text-slate-400 font-mono">
              30+ language deathmatch
            </span>
          </div>
        </a>

        <nav className="hidden md:flex items-center gap-6 text-sm font-medium text-slate-600 dark:text-slate-300">
          <a
            href="#generator"
            className="hover:text-orange-600 dark:hover:text-orange-400 transition-colors flex items-center gap-1.5"
          >
            <Zap className="w-4 h-4" />
            Generator
          </a>
          <a
            href="#benchmarks"
            className="hover:text-orange-600 dark:hover:text-orange-400 transition-colors flex items-center gap-1.5"
          >
            <BarChart2 className="w-4 h-4" />
            Benchmarks
          </a>
          <a
            href="#languages"
            className="hover:text-orange-600 dark:hover:text-orange-400 transition-colors flex items-center gap-1.5"
          >
            <Code2 className="w-4 h-4" />
            Languages
          </a>
          <a
            href="#docs"
            className="hover:text-orange-600 dark:hover:text-orange-400 transition-colors flex items-center gap-1.5"
          >
            <BookOpen className="w-4 h-4" />
            Docs & CLI
          </a>
        </nav>

        <div className="flex items-center gap-3">
          {mounted && (
            <button
              onClick={toggleTheme}
              aria-label="Toggle color theme"
              className="p-2 rounded-lg border border-slate-200 dark:border-slate-800 text-slate-600 dark:text-slate-300 hover:bg-slate-100 dark:hover:bg-slate-800 transition-colors cursor-pointer"
            >
              {theme === 'dark' ? (
                <Sun className="w-4 h-4 text-amber-400" />
              ) : (
                <Moon className="w-4 h-4 text-slate-700" />
              )}
            </button>
          )}

          <a
            href="https://github.com/joshuacox/name-generator"
            target="_blank"
            rel="noreferrer"
            className="flex items-center gap-2 px-3.5 py-1.5 text-sm font-medium rounded-lg bg-slate-900 text-white dark:bg-white dark:text-slate-900 hover:opacity-90 transition-opacity shadow-sm"
          >
            <GithubIcon className="w-4 h-4" />
            <span className="hidden sm:inline">GitHub</span>
          </a>
        </div>
      </div>
    </header>
  );
};
