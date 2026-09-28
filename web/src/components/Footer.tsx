import React from 'react';
import { Zap } from 'lucide-react';
import { GithubIcon } from './icons';

export const Footer: React.FC = () => {
  return (
    <footer className="border-t border-slate-200 dark:border-slate-800 bg-white dark:bg-[#090d16] text-xs text-slate-500 py-12 transition-colors">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex flex-col md:flex-row items-center justify-between gap-6">
          <div className="flex items-center gap-3">
            <div className="w-7 h-7 rounded-lg bg-orange-500 flex items-center justify-center text-white shadow-sm">
              <Zap className="w-4 h-4 fill-current" />
            </div>
            <div>
              <span className="font-bold text-slate-800 dark:text-slate-200">
                name-generator
              </span>
              <span className="mx-2 text-slate-400">•</span>
              <span>An open source multi-language benchmark initiative</span>
            </div>
          </div>

          <div className="flex items-center gap-6">
            <a
              href="https://github.com/joshuacox/name-generator"
              target="_blank"
              rel="noreferrer"
              className="hover:text-slate-900 dark:hover:text-white transition-colors flex items-center gap-1.5"
            >
              <GithubIcon className="w-3.5 h-3.5" />
              GitHub
            </a>
            <a
              href="https://github.com/joshuacox/name-generator/issues"
              target="_blank"
              rel="noreferrer"
              className="hover:text-slate-900 dark:hover:text-white transition-colors"
            >
              Issues & PRs
            </a>
            <a
              href="https://github.com/joshuacox/name-generator/blob/main/LICENSE"
              target="_blank"
              rel="noreferrer"
              className="hover:text-slate-900 dark:hover:text-white transition-colors"
            >
              License
            </a>
          </div>
        </div>

        <div className="mt-8 pt-6 border-t border-slate-100 dark:border-slate-800/80 flex flex-col sm:flex-row items-center justify-between gap-3 text-slate-400">
          <div>
            Created by{' '}
            <a
              href="https://github.com/joshuacox"
              target="_blank"
              rel="noreferrer"
              className="font-medium text-slate-700 dark:text-slate-300 hover:text-orange-500"
            >
              Joshua Cox
            </a>{' '}
            & the open-source community.
          </div>
          <div className="flex items-center gap-1">
            Built with Next.js, React, Tailwind CSS, & Hyperfine.
          </div>
        </div>
      </div>
    </footer>
  );
};
