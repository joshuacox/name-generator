import React from 'react';
import Link from 'next/link';
import Image from 'next/image';
import { Navbar } from '@/components/Navbar';
import { Footer } from '@/components/Footer';
import {
  Trophy,
  Zap,
  Flame,
  ArrowLeft,
  Clock,
  Calendar,
  Layers,
  Terminal,
  CheckCircle2,
  Code2,
  ExternalLink,
} from 'lucide-react';

export const metadata = {
  title: 'Inside the Polyglot Deathmatch: Benchmarking 48 Languages | Name Generator',
  description:
    'A deep dive into runtime ergonomics, memory management, process spawning vs in-memory caching, and automated CI benchmarking across 48 programming languages.',
};

export default function ArticlePage() {
  const basePath = process.env.NEXT_PUBLIC_BASE_PATH || '';

  return (
    <div className="min-h-screen flex flex-col bg-slate-50 dark:bg-[#090d16] text-slate-900 dark:text-slate-100 transition-colors">
      <Navbar />

      <main className="flex-1 py-12 md:py-20">
        <article className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8">
          {/* Back to Home Link */}
          <div className="mb-8">
            <Link
              href="/"
              className="inline-flex items-center gap-1.5 text-xs font-semibold text-orange-600 dark:text-orange-400 hover:text-orange-700 dark:hover:text-orange-300 transition-colors"
            >
              <ArrowLeft className="w-3.5 h-3.5" /> Back to Generator & Leaderboard
            </Link>
          </div>

          {/* Article Header */}
          <header className="mb-12 border-b border-slate-200 dark:border-slate-800 pb-10">
            <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full bg-orange-500/10 text-orange-600 dark:text-orange-400 border border-orange-500/20 text-xs font-semibold mb-4">
              <Layers className="w-3.5 h-3.5" />
              Technical Deep Dive & Architecture
            </div>

            <h1
              className="text-3xl sm:text-4xl lg:text-5xl font-extrabold text-slate-900 dark:text-white tracking-tight leading-[1.2] mb-6"
              style={{ textWrap: 'balance' }}
            >
              Inside the Polyglot Deathmatch: Benchmarking 48 Languages on a Deceptively Simple Problem
            </h1>

            <p
              className="text-lg sm:text-xl text-slate-600 dark:text-slate-300 leading-relaxed mb-6"
              style={{ textWrap: 'pretty' }}
            >
              What happens when you write the exact same CLI utility across 48 programming languages
              spanning five decades of computing history? You uncover surprising truths about runtime startup,
              memory models, and the immense cost of UNIX process spawning.
            </p>

            <div className="flex flex-wrap items-center gap-4 text-xs font-mono text-slate-500 dark:text-slate-400">
              <span className="flex items-center gap-1.5">
                <Calendar className="w-3.5 h-3.5 text-orange-500" />
                September 2026
              </span>
              <span>•</span>
              <span className="flex items-center gap-1.5">
                <Clock className="w-3.5 h-3.5 text-orange-500" />
                12 min read
              </span>
              <span>•</span>
              <span>By Joshua Cox & Open Source Contributors</span>
            </div>
          </header>

          {/* Article Editorial Hero Cover */}
          <div className="relative rounded-2xl overflow-hidden mb-12 border border-slate-200 dark:border-slate-800 shadow-xl shadow-slate-900/5 dark:shadow-black/40 group">
            <Image
              src={`${basePath}/images/article-cover.webp`}
              alt="Programming Language Data Race - 48 Polyglot Implementations"
              width={1376}
              height={768}
              priority
              className="w-full h-auto object-cover transition-transform duration-700 group-hover:scale-[1.01]"
            />
            <div className="absolute inset-0 bg-gradient-to-t from-black/85 via-black/20 to-transparent pointer-events-none" />
            <div className="absolute bottom-3 left-4 right-4 flex items-center justify-between text-[11px] text-slate-300 font-mono">
              <span className="flex items-center gap-2">
                <span className="inline-block w-2 h-2 rounded-full bg-cyan-400 animate-pulse" />
                FIG 1.0 &mdash; THE POLYGLOT PERFORMANCE ARENA
              </span>
              <span className="hidden sm:inline text-slate-400">
                FIBER OPTIC CIRCUITS &bull; 48 RUNTIMES IN CONCURRENT EXECUTION
              </span>
            </div>
          </div>

          {/* Table of Contents */}
          <nav className="p-6 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800 shadow-sm mb-12">
            <h2 className="text-sm font-bold uppercase tracking-wider text-slate-900 dark:text-white mb-3">
              Table of Contents
            </h2>
            <ol className="space-y-1.5 text-xs sm:text-sm text-slate-600 dark:text-slate-400 font-medium list-decimal list-inside">
              <li>
                <a href="#the-origin" className="hover:text-orange-500 transition-colors">
                  The Origin: From Fleet Naming to a 48-Language Arena
                </a>
              </li>
              <li>
                <a href="#the-specification" className="hover:text-orange-500 transition-colors">
                  The Specification: Why "Random Names" is Surprisingly Brutal
                </a>
              </li>
              <li>
                <a href="#memory-models" className="hover:text-orange-500 transition-colors">
                  The Architectural Divide: $O(1)$ In-Memory Caching vs $O(N)$ Disk Churn
                </a>
              </li>
              <li>
                <a href="#compiled-champions" className="hover:text-orange-500 transition-colors">
                  The Compiled Systems Shootout: How Zig Took the Crown
                </a>
              </li>
              <li>
                <a href="#awk-miracle" className="hover:text-orange-500 transition-colors">
                  The Scripting Miracle: The AWK Revelation
                </a>
              </li>
              <li>
                <a href="#shell-fork-bomb" className="hover:text-orange-500 transition-colors">
                  The Process Fork Penalty: Why Shells Crawl Under Scale
                </a>
              </li>
              <li>
                <a href="#automated-ci" className="hover:text-orange-500 transition-colors">
                  Continuous Deathmatch: Automated Hyperfine Benchmarks in CI
                </a>
              </li>
              <li>
                <a href="#takeaways" className="hover:text-orange-500 transition-colors">
                  Lessons for CLI & Systems Designers
                </a>
              </li>
            </ol>
          </nav>

          {/* Article Body Content */}
          <div className="space-y-12 text-slate-700 dark:text-slate-300 text-sm sm:text-base leading-relaxed">
            {/* Section 1 */}
            <section id="the-origin">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                1. The Origin: From Fleet Naming to a 48-Language Arena
              </h2>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                Every infrastructure engineer knows the mantra: <em>"Treat your servers like cattle, not pets."</em> Yet, staring at
                unidentifiable hostnames like <code className="px-1.5 py-0.5 rounded bg-slate-100 dark:bg-slate-800 font-mono text-xs">dal2dc3c38r67</code> in
                a terminal buffer is soul-crushing. Docker solved this years ago by assigning friendly combinations like{' '}
                <code className="px-1.5 py-0.5 rounded bg-slate-100 dark:bg-slate-800 font-mono text-xs">bold_torvalds</code> and{' '}
                <code className="px-1.5 py-0.5 rounded bg-slate-100 dark:bg-slate-800 font-mono text-xs">loving_curie</code> to nameless containers.
              </p>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                Years ago, Joshua Cox wrote a short Bash script to generate similar memorable names for cloud instances.
                Naturally, that led to a curiosity: <em>Is POSIX sh faster than Bash? How does Zsh compare? What about Python?</em>
              </p>
              <p style={{ textWrap: 'pretty' }}>
                What began as a localized shell benchmark snowball-rolled into a multi-year polyglot initiative. Today, this repository houses{' '}
                <strong className="text-slate-900 dark:text-white font-semibold">48 distinct implementations</strong>—from ancient mainstays like
                Fortran 90 and Ada 2012 to modern systems contenders like Zig, Odin, Nim, Crystal, and Rust, as well as shells, JVM languages, and esoteric
                dialects like Brainfuck.
              </p>
            </section>

            {/* Section 2 */}
            <section id="the-specification">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                2. The Specification: Why "Random Names" is Surprisingly Brutal
              </h2>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                At a glance, picking a random adjective and joining it with a random noun seems like an introductory programming exercise.
                However, to make the comparison valid across 48 languages, every implementation must adhere strictly to identical runtime contracts:
              </p>

              <div className="grid grid-cols-1 md:grid-cols-2 gap-4 my-6 not-prose">
                <div className="p-4 rounded-xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800">
                  <div className="font-mono text-xs font-bold text-orange-600 dark:text-orange-400 mb-1">counto</div>
                  <div className="text-xs text-slate-500">
                    Controls how many names to generate. If unset, must gracefully fallback to terminal line count via <code>tput lines</code> or default to 1.
                  </div>
                </div>

                <div className="p-4 rounded-xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800">
                  <div className="font-mono text-xs font-bold text-orange-600 dark:text-orange-400">SEPARATOR</div>
                  <div className="text-xs text-slate-500">
                    The delimiter between words. Defaults to hyphen (<code>-</code>), but customizable to underscore (<code>_</code>), dot, or empty string.
                  </div>
                </div>

                <div className="p-4 rounded-xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800">
                  <div className="font-mono text-xs font-bold text-orange-600 dark:text-orange-400">Casing Invariant</div>
                  <div className="text-xs text-slate-500">
                    Adjectives must retain their original dictionary casing. Nouns <em>must be strictly lowercased</em> (e.g. <code>Turing</code> &rarr; <code>turing</code>).
                  </div>
                </div>

                <div className="p-4 rounded-xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800">
                  <div className="font-mono text-xs font-bold text-orange-600 dark:text-orange-400">Wordlist Hierarchy</div>
                  <div className="text-xs text-slate-500">
                    Check <code>NOUN_FILE</code> / <code>ADJ_FILE</code> first. If unset, inspect <code>NOUN_FOLDER</code> / <code>ADJ_FOLDER</code> and pick a file at random.
                  </div>
                </div>
              </div>

              <p style={{ textWrap: 'pretty' }}>
                Because of these rules, the benchmark tests three critical performance axes simultaneously:
              </p>
              <ul className="list-disc list-inside space-y-2 mt-3 text-slate-600 dark:text-slate-300">
                <li><strong className="text-slate-900 dark:text-white">Cold Runtime Initialization:</strong> How long does the binary, VM, or interpreter take to boot before executing user code?</li>
                <li><strong className="text-slate-900 dark:text-white">Memory Allocation & Caching:</strong> Does the program parse the 27,000-word corpus into memory once, or churn disk descriptors?</li>
                <li><strong className="text-slate-900 dark:text-white">I/O Throughput:</strong> How does the runtime handle string concatenation and buffered writes to standard output when batch size reaches 10,000+?</li>
              </ul>
            </section>

            {/* Section 3 */}
            <section id="memory-models">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                3. The Architectural Divide: $O(1)$ In-Memory Caching vs $O(N)$ Disk Churn
              </h2>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                One of the earliest discoveries in this project came from an unexpected source: the original C implementation (<code className="font-mono text-xs">name-generator.c</code>).
                When benchmarking small batch counts (<code className="font-mono text-xs">counto=1</code>), C was near-instant. But when scaling to thousands of names, C slowed down dramatically.
              </p>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                Why? In naive implementations, developers write a loop that re-opens the directory or re-scans file lines for every single name.
                At <code className="font-mono text-xs">counto=10000</code>, that means 20,000 file open, read, seek, and close system calls!
              </p>

              <div className="p-5 rounded-2xl bg-orange-500/10 border border-orange-500/20 text-xs sm:text-sm my-6">
                <div className="font-bold text-orange-600 dark:text-orange-400 mb-1 flex items-center gap-2">
                  <Zap className="w-4 h-4" /> The Golden Architectural Rule
                </div>
                <p className="text-slate-700 dark:text-slate-300">
                  Every high-performance contender in our leaderboard reads the wordlist into a contiguous dynamic buffer (vector / slice) <strong>exactly once</strong> during startup.
                  Subsequent name generation is a pure in-memory <code className="font-mono text-xs">O(1)</code> pseudo-random index lookup followed by buffered stdout emission.
                </p>
              </div>

              {/* Architecture Diagram */}
              <div className="relative rounded-2xl overflow-hidden my-8 border border-slate-200 dark:border-slate-800 shadow-xl shadow-slate-900/5 dark:shadow-black/40 group">
                <Image
                  src={`${basePath}/images/architecture-diagram.webp`}
                  alt="High-Performance In-Memory Caching vs High-Overhead Disk-Churn Process Architecture"
                  width={1376}
                  height={768}
                  className="w-full h-auto object-cover transition-transform duration-700 group-hover:scale-[1.01]"
                />
                <div className="absolute inset-0 bg-gradient-to-t from-black/85 via-black/20 to-transparent pointer-events-none" />
                <div className="absolute bottom-3 left-4 right-4 flex items-center justify-between text-[11px] text-slate-300 font-mono">
                  <span className="flex items-center gap-2">
                    <span className="inline-block w-2 h-2 rounded-full bg-amber-400" />
                    FIG 2.0 &mdash; MEMORY &amp; PROCESS TOPOLOGY
                  </span>
                  <span className="hidden sm:inline text-slate-400">
                    ZERO-COPY RAM ACCESS VS KERNEL FORK() / EXEC() CONTEXT SWITCHES
                  </span>
                </div>
              </div>
            </section>

            {/* Section 4 */}
            <section id="compiled-champions">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                4. The Compiled Systems Shootout: How Zig Took the Crown
              </h2>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                In our deathmatch across compiled systems contenders generating 1,000 names, <strong className="text-slate-900 dark:text-white">Zig</strong> emerged
                as the undisputed speed champion at an astonishing <strong className="text-emerald-500 font-mono">1.2 ms</strong>.
              </p>

              <div className="overflow-x-auto my-6">
                <table className="w-full text-left text-xs sm:text-sm font-mono border border-slate-200 dark:border-slate-800 rounded-xl overflow-hidden">
                  <thead className="bg-slate-100 dark:bg-slate-900 text-slate-500 font-sans">
                    <tr>
                      <th className="p-3">Rank</th>
                      <th className="p-3">Language</th>
                      <th className="p-3">Mean Runtime</th>
                      <th className="p-3">Throughput (Names/sec)</th>
                      <th className="p-3">Architectural Highlights</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-slate-100 dark:divide-slate-800">
                    <tr className="bg-emerald-500/5">
                      <td className="p-3 font-bold text-emerald-600">#1</td>
                      <td className="p-3 font-bold text-slate-900 dark:text-white">Zig</td>
                      <td className="p-3 text-emerald-600 font-bold">1.2 ms</td>
                      <td className="p-3">833,000 /s</td>
                      <td className="p-3 font-sans text-xs">ReleaseFast strip, arena allocator, zero runtime startup</td>
                    </tr>
                    <tr>
                      <td className="p-3 font-bold text-slate-400">#2</td>
                      <td className="p-3 font-bold text-slate-900 dark:text-white">Go</td>
                      <td className="p-3 font-semibold text-orange-500">2.0 ms</td>
                      <td className="p-3">456,000 /s</td>
                      <td className="p-3 font-sans text-xs">Concurrent GC runtime, fast slice indexing, buffered bufio.Writer</td>
                    </tr>
                    <tr>
                      <td className="p-3 font-bold text-slate-400">#3</td>
                      <td className="p-3 font-bold text-slate-900 dark:text-white">Crystal</td>
                      <td className="p-3 font-semibold text-orange-500">2.5 ms</td>
                      <td className="p-3">396,000 /s</td>
                      <td className="p-3 font-sans text-xs">Ruby-like elegance compiled directly to optimized LLVM bitcode</td>
                    </tr>
                    <tr>
                      <td className="p-3 font-bold text-slate-400">#4</td>
                      <td className="p-3 font-bold text-slate-900 dark:text-white">Nim</td>
                      <td className="p-3">3.8 ms</td>
                      <td className="p-3">266,000 /s</td>
                      <td className="p-3 font-sans text-xs">Transpiles to C with ORC ARC memory management</td>
                    </tr>
                    <tr>
                      <td className="p-3 font-bold text-slate-400">#5</td>
                      <td className="p-3 font-bold text-slate-900 dark:text-white">Odin</td>
                      <td className="p-3">4.6 ms</td>
                      <td className="p-3">217,000 /s</td>
                      <td className="p-3 font-sans text-xs">Custom context allocator, data-oriented memory layouts</td>
                    </tr>
                    <tr>
                      <td className="p-3 font-bold text-slate-400">#6</td>
                      <td className="p-3 font-bold text-slate-900 dark:text-white">Ada 2012</td>
                      <td className="p-3">7.3 ms</td>
                      <td className="p-3">80,000 /s</td>
                      <td className="p-3 font-sans text-xs">GNAT -O3, strong type safety, unbounded string vectors</td>
                    </tr>
                  </tbody>
                </table>
              </div>

              <p style={{ textWrap: 'pretty' }}>
                Zig achieves its staggering performance because it incurs zero runtime initialization penalty. Unlike Go, it does not initialize a green-thread scheduler or garbage collector.
                Unlike C++ with standard streams, Zig's <code className="font-mono text-xs">std.fs.File.Writer</code> performs direct buffered writes without locale bloat or synchronization locks.
              </p>
            </section>

            {/* Section 5 */}
            <section id="awk-miracle">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                5. The Scripting Miracle: The AWK Revelation
              </h2>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                When people think of high-performance scripting today, they think of Node.js with Google's V8 JIT compiler or PyPy.
                Yet when we benchmarked interpreted languages, <strong className="text-slate-900 dark:text-white">AWK</strong> stunned the entire deathmatch.
              </p>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                AWK completed 100 iterations in <strong className="text-orange-500 font-mono">19.3 ms</strong>, beating Node.js (25.2 ms) and running{' '}
                <strong className="text-slate-900 dark:text-white">10 times faster than Bash (203 ms)</strong>.
              </p>
              <p style={{ textWrap: 'pretty' }}>
                Why? AWK was designed by Aho, Weinberger, and Kernighan in 1977 for one specific purpose: lightning-fast text record scanning.
                It has virtually zero interpreter boot overhead (less than 1.5 ms), uses associative arrays natively, and parses files line-by-line in pure C.
                In CLI scenarios where long-lived JIT warmup cannot amortize, AWK's spartan design outperforms modern multi-megabyte runtimes.
              </p>
            </section>

            {/* Section 6 */}
            <section id="shell-fork-bomb">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                6. The Process Fork Penalty: Why Shells Crawl Under Scale
              </h2>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                Our multi-scale benchmarks revealed a massive disparity in how traditional shell scripts scale compared to compiled programs:
              </p>

              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4 my-6 not-prose">
                <div className="p-5 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800">
                  <div className="text-xs font-mono font-bold text-emerald-500 mb-1">Zig Scaling (In-Memory)</div>
                  <ul className="text-xs font-mono space-y-1 text-slate-600 dark:text-slate-400">
                    <li>count=1: <strong>1.11 ms</strong></li>
                    <li>count=10: <strong>1.12 ms</strong></li>
                    <li>count=100: <strong>1.14 ms</strong></li>
                    <li>count=1,000: <strong>1.20 ms</strong></li>
                  </ul>
                  <div className="mt-3 text-xs text-slate-500 font-sans">Growth factor: <strong>1.08x</strong> across 1,000 items!</div>
                </div>

                <div className="p-5 rounded-2xl bg-white dark:bg-[#131b2e] border border-slate-200 dark:border-slate-800">
                  <div className="text-xs font-mono font-bold text-rose-500 mb-1">Bash Scaling (Subprocess Loop)</div>
                  <ul className="text-xs font-mono space-y-1 text-slate-600 dark:text-slate-400">
                    <li>count=1: <strong>5.40 ms</strong></li>
                    <li>count=10: <strong>20.02 ms</strong></li>
                    <li>count=100: <strong>269.17 ms</strong></li>
                    <li>count=1,000: <strong>2,464.61 ms</strong></li>
                  </ul>
                  <div className="mt-3 text-xs text-slate-500 font-sans">Growth factor: <strong>456x</strong> across 1,000 items!</div>
                </div>
              </div>

              <p style={{ textWrap: 'pretty' }}>
                When a Bash script runs a loop invoking <code className="font-mono text-xs">shuf -n 1</code> or pipelines, Linux must clone the process table, setup file descriptors, load binary symbols via ld.so, and tear down page tables.
                At 1,000 iterations, the operating system is spending 98% of its CPU cycles inside the Linux kernel scheduler fork subsystem rather than generating names.
              </p>
            </section>

            {/* Section 7 */}
            <section id="automated-ci">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                7. Continuous Deathmatch: Automated Hyperfine Benchmarks in CI
              </h2>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                Historically, benchmarks go stale the moment they are committed to a README. Compilers update, kernel versions advance, and performance shifts.
              </p>
              <p className="mb-4" style={{ textWrap: 'pretty' }}>
                To solve this permanently, we built an end-to-end automated benchmarking pipeline powered by GitHub Actions:
              </p>
              <ol className="list-decimal list-inside space-y-2 text-slate-600 dark:text-slate-300">
                <li>Whenever a PR or commit lands on <code className="font-mono text-xs">main</code>, an Ubuntu runner boots with GCC, Go, Rust, Zig, GNAT, and GFortran.</li>
                <li>The script <code className="font-mono text-xs">scripts/ci-benchmark.sh</code> compiles every available target in release mode.</li>
                <li><code className="font-mono text-xs">hyperfine</code> executes statistical benchmarks with cache warmups and exports JSON datasets.</li>
                <li><code className="font-mono text-xs">scripts/generate-data.mjs</code> dynamically ingests the timings and computes real-time relative speedups.</li>
                <li>The Next.js static site is built and deployed directly to GitHub Pages with zero manual intervention.</li>
              </ol>
            </section>

            {/* Section 8 */}
            <section id="takeaways">
              <h2
                className="text-2xl sm:text-3xl font-bold text-slate-900 dark:text-white mb-4 tracking-tight"
                style={{ textWrap: 'balance' }}
              >
                8. Lessons for CLI & Systems Designers
              </h2>
              <div className="space-y-4">
                <div className="flex gap-3">
                  <CheckCircle2 className="w-5 h-5 text-emerald-500 shrink-0 mt-0.5" />
                  <div>
                    <strong className="text-slate-900 dark:text-white">Preload & Buffer Aggressively:</strong> If your utility processes batches of items, never re-touch disk storage inside loops. Read once into memory, generate in-place, and flush stdout in blocks.
                  </div>
                </div>

                <div className="flex gap-3">
                  <CheckCircle2 className="w-5 h-5 text-emerald-500 shrink-0 mt-0.5" />
                  <div>
                    <strong className="text-slate-900 dark:text-white">Startup Latency Trumps Peak Throughput in CLI:</strong> A JIT that generates 10M ops/sec is useless for command-line tools if it takes 80ms to boot. For short tasks, low-startup runtimes (Zig, Go, AWK) dominate.
                  </div>
                </div>

                <div className="flex gap-3">
                  <CheckCircle2 className="w-5 h-5 text-emerald-500 shrink-0 mt-0.5" />
                  <div>
                    <strong className="text-slate-900 dark:text-white">Beware Shell Process Multiplication:</strong> Shell scripts are unrivaled for glue, but inner loops spawning subprocesses will hit a wall at scale. When loops exceed dozens of iterations, reach for AWK or compiled tools.
                  </div>
                </div>
              </div>
            </section>
          </div>

          {/* Article Footer & CTA */}
          <div className="mt-16 pt-8 border-t border-slate-200 dark:border-slate-800 flex flex-col sm:flex-row items-center justify-between gap-4">
            <Link
              href="/"
              className="px-5 py-2.5 rounded-xl bg-gradient-to-r from-orange-500 to-amber-500 hover:from-orange-600 hover:to-amber-600 text-white font-semibold text-sm shadow-md shadow-orange-500/25 transition-all cursor-pointer"
            >
              Explore Live Leaderboard & Generator &rarr;
            </Link>

            <a
              href="https://github.com/joshuacox/name-generator"
              target="_blank"
              rel="noreferrer"
              className="flex items-center gap-1.5 text-xs text-slate-500 hover:text-slate-900 dark:hover:text-white font-medium transition-colors"
            >
              Contribute a Language on GitHub <ExternalLink className="w-3.5 h-3.5" />
            </a>
          </div>
        </article>
      </main>

      <Footer />
    </div>
  );
}
