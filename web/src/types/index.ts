export interface LanguageMeta {
  id: string;
  name: string;
  file: string;
  category: 'Compiled' | 'Scripting' | 'Shell' | 'VM' | 'WIP' | 'Esoteric';
  paradigm: string;
  ext: string;
  compiledBin?: string;
  sloc?: number;
  codeSample?: string;
  githubUrl: string;
}

export interface MemoryBenchmarkItem {
  name: string;
  category: string;
  peakRssMb: number;
  peakRssKb: number;
  namesPerMb?: number;
}

export interface CompiledBenchmarkItem {
  name: string;
  command: string;
  mean: number;
  min: number;
  max: number;
  stddev: number;
  relative: number;
  paradigm: string;
  category?: string;
  peakRssMb?: number;
  peakRssKb?: number;
  startupMs?: number;
  marginalUsPerName?: number;
  sustainedNamesPerSec?: number;
}

export interface ScriptingBenchmarkItem {
  name: string;
  command: string;
  mean: number;
  min: number;
  max: number;
  stddev?: number;
  relative: number;
  type?: string;
  category?: string;
  paradigm?: string;
  peakRssMb?: number;
  peakRssKb?: number;
  startupMs?: number;
  marginalUsPerName?: number;
  sustainedNamesPerSec?: number;
}

export interface ScalingPoint {
  count: number;
  meanMs: number;
}

export interface ScalingCurve {
  language: string;
  command: string;
  category: 'Compiled' | 'Scripting' | 'Shell' | 'VM';
  points: ScalingPoint[];
}

export interface ThroughputItem {
  name: string;
  category: string;
  namesPerSecond: number;
  meanMsAt1000: number;
  relativeToFastest: number;
  peakRssMb?: number;
  startupMs?: number;
  marginalUsPerName?: number;
  sustainedRate?: number;
}

export interface BenchmarkData {
  generatedAt: string;
  environment: {
    os: string;
    cpu: string;
    tool: string;
  };
  stats: {
    totalLanguages: number;
    activeBenchmarked?: number;
    compiledContenders: number;
    vmContenders?: number;
    scriptingContenders: number;
    shellContenders?: number;
    fastestLanguage: string;
    fastestMeanMs: number;
    fastestScripting: string;
    fastestScriptingMs: number;
    leanestMemoryLanguage?: string;
    leanestMemoryMb?: number;
    maxThroughputPerSec?: number;
  };
  deathmatchCompiled: CompiledBenchmarkItem[];
  deathmatchVm?: CompiledBenchmarkItem[];
  deathmatchScripting: ScriptingBenchmarkItem[];
  deathmatchShells?: ScriptingBenchmarkItem[];
  overallLeaderboard?: Array<CompiledBenchmarkItem | ScriptingBenchmarkItem>;
  memoryLeaderboard?: MemoryBenchmarkItem[];
  scalingCurves: ScalingCurve[];
  throughputLeaderboard: ThroughputItem[];
  languages: LanguageMeta[];
  scanners?: {
    fastest_24_summary?: Array<{
      command: string;
      cleanCmd: string;
      mean: number;
      parameter_num_count: number;
      min?: number;
      max?: number;
    }>;
  };
}

