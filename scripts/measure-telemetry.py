#!/usr/bin/env python3
"""
scripts/measure-telemetry.py
Measures:
1. Peak RSS memory (KB & MB) using GNU /usr/bin/time or /proc sampling.
2. Startup overhead vs sustained throughput via Ordinary Least Squares (OLS) regression:
   T(N) = T_startup + N * t_marginal
3. Exports structured telemetry to log/ci-telemetry.json and log/ci-memory.json
"""

import os
import sys
import json
import time
import shutil
import subprocess
from pathlib import Path

ROOT_DIR = Path(__file__).resolve().parent.parent
LOG_DIR = ROOT_DIR / "log"
LOG_DIR.mkdir(parents=True, exist_ok=True)

# Candidate definitions with metadata
CANDIDATES = [
    # Systems / Compiled
    {"id": "zig", "name": "Zig", "category": "Compiled", "cmd": "./name-generator_zig"},
    {"id": "c", "name": "C", "category": "Compiled", "cmd": "./name-generator"},
    {"id": "cpp", "name": "C++", "category": "Compiled", "cmd": "./name-generator_cpp"},
    {"id": "rust", "name": "Rust", "category": "Compiled", "cmd": "rust/target/release/name-generator"},
    {"id": "go", "name": "Go", "category": "Compiled", "cmd": "./name-generator_go"},
    {"id": "crystal", "name": "Crystal", "category": "Compiled", "cmd": "./name-generator_crystal"},
    {"id": "nim", "name": "Nim", "category": "Compiled", "cmd": "./name-generator_nim"},
    {"id": "odin", "name": "Odin", "category": "Compiled", "cmd": "./name-generator_odin"},
    {"id": "ada", "name": "Ada", "category": "Compiled", "cmd": "./name-generator_ada"},
    {"id": "cobol", "name": "COBOL", "category": "Compiled", "cmd": "./name-generator_cobol"},
    {"id": "v", "name": "V", "category": "Compiled", "cmd": "./name-generator_v"},
    {"id": "fortran", "name": "Fortran", "category": "Compiled", "cmd": "./name-generator_fortran"},
    {"id": "pascal", "name": "Pascal", "category": "Compiled", "cmd": "./name-generator_pascal"},
    {"id": "d", "name": "D", "category": "Compiled", "cmd": "./name-generator_d"},
    {"id": "dart", "name": "Dart", "category": "Compiled", "cmd": "./name-generator_dart"},

    # VM / JIT
    {"id": "java", "name": "Java", "category": "VM", "cmd": "java NameGenerator"},

    # Scripting
    {"id": "awk", "name": "AWK", "category": "Scripting", "cmd": "./name-generator.awk"},
    {"id": "javascript", "name": "Node.js", "category": "Scripting", "cmd": "node ./name-generator.js"},
    {"id": "typescript", "name": "TypeScript", "category": "Scripting", "cmd": "./name-generator.ts"},
    {"id": "python", "name": "Python", "category": "Scripting", "cmd": "python3 ./name-generator.py"},
    {"id": "ruby", "name": "Ruby", "category": "Scripting", "cmd": "ruby ./name-generator.rb"},
    {"id": "perl", "name": "Perl", "category": "Scripting", "cmd": "perl ./name-generator.pl"},
    {"id": "php", "name": "PHP", "category": "Scripting", "cmd": "php ./name-generator.php"},
    {"id": "lua", "name": "Lua", "category": "Scripting", "cmd": "lua ./name-generator.lua"},
    {"id": "tcl", "name": "Tcl", "category": "Scripting", "cmd": "./name-generator.tcl"},

    # Shells
    {"id": "sh", "name": "POSIX sh", "category": "Shell", "cmd": "./name-generator.sh"},
    {"id": "bash", "name": "Bash", "category": "Shell", "cmd": "./name-generator.bash"},
    {"id": "zsh", "name": "Zsh", "category": "Shell", "cmd": "./name-generator.zsh"},
    {"id": "fish", "name": "Fish", "category": "Shell", "cmd": "./name-generator.fish"},
    {"id": "ksh", "name": "KornShell", "category": "Shell", "cmd": "./name-generator.ksh"},
    {"id": "nu", "name": "Nushell", "category": "Shell", "cmd": "./name-generator.nu"},
]

def is_runnable(cmd_str):
    env = os.environ.copy()
    env["counto"] = "1"
    try:
        res = subprocess.run(
            cmd_str,
            shell=True,
            cwd=str(ROOT_DIR),
            env=env,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=5
        )
        return res.returncode == 0
    except Exception:
        return False

def measure_peak_rss_kb(cmd_str, counto=1000, runs=3):
    """Measures peak RSS in KB using /usr/bin/time or falls back to subprocess."""
    time_bin = shutil.which("time") or "/usr/bin/time"
    has_gnu_time = os.path.exists(time_bin) and os.access(time_bin, os.X_OK)

    samples = []
    env = os.environ.copy()
    env["counto"] = str(counto)

    tmp_out = LOG_DIR / "time_rss.tmp"

    for _ in range(runs):
        if has_gnu_time:
            run_cmd = f"{time_bin} -o '{tmp_out}' -f '%M' sh -c '{cmd_str}' >/dev/null 2>&1"
            res = subprocess.run(run_cmd, shell=True, cwd=str(ROOT_DIR), env=env)
            if res.returncode == 0 and tmp_out.exists():
                try:
                    val = int(tmp_out.read_text().strip())
                    if val > 0:
                        samples.append(val)
                except Exception:
                    pass
                finally:
                    if tmp_out.exists():
                        tmp_out.unlink()
        else:
            # Fallback using python child measurement
            import resource
            p = subprocess.Popen(
                cmd_str,
                shell=True,
                cwd=str(ROOT_DIR),
                env=env,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )
            _, _, rusage = os.wait4(p.pid, 0)
            if rusage.ru_maxrss > 0:
                samples.append(rusage.ru_maxrss)

    if samples:
        samples.sort()
        return samples[len(samples) // 2]  # median
    return 0

def linear_regression(points):
    """
    points: list of (count, mean_ms)
    Fits T(N) = T_startup + N * t_marginal
    Returns: startup_ms, marginal_us_per_name, sustained_names_per_sec, r2
    """
    if len(points) < 2:
        return None

    xs = [float(p[0]) for p in points]
    ys = [float(p[1]) for p in points]
    n = len(xs)

    mean_x = sum(xs) / n
    mean_y = sum(ys) / n

    numerator = sum((xs[i] - mean_x) * (ys[i] - mean_y) for i in range(n))
    denominator = sum((xs[i] - mean_x) ** 2 for i in range(n))

    if denominator == 0:
        return None

    slope = numerator / denominator  # ms per name
    intercept = mean_y - slope * mean_x  # ms startup

    # Ensure reasonable bounds
    slope = max(0.0, slope)
    intercept = max(0.01, intercept)

    # R^2 calculation
    ss_tot = sum((y - mean_y) ** 2 for y in ys)
    ss_res = sum((ys[i] - (intercept + slope * xs[i])) ** 2 for i in range(n))
    r2 = 1.0 - (ss_res / ss_tot) if ss_tot > 0 else 1.0
    r2 = max(0.0, min(1.0, r2))

    marginal_us = slope * 1000.0  # microseconds per name
    sustained_rate = int(1000.0 / slope) if slope > 1e-7 else int(1e7)

    return {
        "startupMs": round(intercept, 2),
        "marginalUsPerName": round(marginal_us, 3),
        "sustainedNamesPerSec": sustained_rate,
        "r2": round(r2, 4)
    }

def main():
    print("=" * 60)
    print("  Name Generator Advanced Telemetry & Memory Profiler")
    print("=" * 60)

    # 1. Discover runnable contenders
    active = []
    for cand in CANDIDATES:
        print(f"--> Probing {cand['name']:<14} ({cand['cmd']})...", end=" ", flush=True)
        if is_runnable(cand["cmd"]):
            print("ACTIVE")
            active.append(cand)
        else:
            print("SKIPPED (not available)")

    print(f"\nDiscovered {len(active)} active contenders out of {len(CANDIDATES)} candidates.")

    # 2. Measure Peak RSS Memory
    print("\n==> Measuring Peak RSS Memory (counto=1000, 3 runs)...")
    memory_results = []
    for c in active:
        # Determine appropriate counto for memory measurement
        test_count = 1000 if c["category"] in ["Compiled", "VM"] else 200
        rss_kb = measure_peak_rss_kb(c["cmd"], counto=test_count, runs=3)
        rss_mb = round(rss_kb / 1024.0, 2)
        print(f"    {c['name']:<14} [{c['category']:<8}]: {rss_mb:>6.2f} MB ({rss_kb:>7} KB)")
        memory_results.append({
            "id": c["id"],
            "name": c["name"],
            "category": c["category"],
            "command": c["cmd"],
            "peakRssKb": rss_kb,
            "peakRssMb": rss_mb,
            "testedCount": test_count,
        })

    # Sort by lowest memory consumption
    memory_results.sort(key=lambda x: x["peakRssKb"])

    # Write ci-memory.json
    mem_file = LOG_DIR / "ci-memory.json"
    with open(mem_file, "w") as f:
        json.dump(memory_results, f, indent=2)
    print(f"\n[OK] Peak RSS data written to {mem_file}")

    # 3. Analyze Scaling Regression (if scaling-benchmarks.json exists)
    scaling_file = LOG_DIR / "scaling-benchmarks.json"
    regression_results = []
    if scaling_file.exists():
        print("\n==> Performing OLS Linear Regression on scaling benchmarks...")
        try:
            with open(scaling_file) as f:
                data = json.load(f)
            grouped = {}
            for r in data.get("results", []):
                cmd_raw = r["command"]
                # strip counto=...
                import re
                clean_cmd = re.sub(r"^counto=\S+\s+", "", cmd_raw)
                cnt = int(r.get("parameters", {}).get("counto", 0))
                mean_ms = r.get("mean", 0.0) * 1000.0
                if cnt > 0:
                    grouped.setdefault(clean_cmd, []).append((cnt, mean_ms))

            for cmd, pts in grouped.items():
                pts.sort(key=lambda x: x[0])
                reg = linear_regression(pts)
                cand = next((c for c in active if c["cmd"] == cmd or cmd.endswith(c["cmd"])), None)
                name = cand["name"] if cand else Path(cmd).name
                category = cand["category"] if cand else "Compiled"
                if reg:
                    print(f"    {name:<14}: Startup={reg['startupMs']:>6.2f} ms | Marginal={reg['marginalUsPerName']:>7.3f} µs/name | Rate={reg['sustainedNamesPerSec']:>9,} names/s (R²={reg['r2']})")
                    regression_results.append({
                        "name": name,
                        "category": category,
                        "command": cmd,
                        "points": [{"count": p[0], "meanMs": round(p[1], 2)} for p in pts],
                        **reg
                    })
        except Exception as e:
            print(f"Warning: could not process scaling data: {e}")

    # 4. Generate combined telemetry bundle
    telemetry = {
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "totalActive": len(active),
        "memory": memory_results,
        "regression": regression_results
    }
    telemetry_file = LOG_DIR / "ci-telemetry.json"
    with open(telemetry_file, "w") as f:
        json.dump(telemetry, f, indent=2)
    print(f"[OK] Full telemetry written to {telemetry_file}")

if __name__ == "__main__":
    main()
