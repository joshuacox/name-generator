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

export interface CompiledBenchmarkItem {
  name: string;
  command: string;
  mean: number;
  min: number;
  max: number;
  stddev: number;
  relative: number;
  paradigm: string;
}

export interface ScriptingBenchmarkItem {
  name: string;
  command: string;
  mean: number;
  min: number;
  max: number;
  relative: number;
  type: string;
}

export interface ScalingPoint {
  count: number;
  meanMs: number;
}

export interface ScalingCurve {
  language: string;
  command: string;
  category: 'Compiled' | 'Scripting' | 'Shell';
  points: ScalingPoint[];
}

export interface ThroughputItem {
  name: string;
  category: string;
  namesPerSecond: number;
  meanMsAt1000: number;
  relativeToFastest: number;
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
    compiledContenders: number;
    scriptingContenders: number;
    fastestLanguage: string;
    fastestMeanMs: number;
    fastestScripting: string;
    fastestScriptingMs: number;
    maxThroughputPerSec?: number;
  };
  deathmatchCompiled: CompiledBenchmarkItem[];
  deathmatchScripting: ScriptingBenchmarkItem[];
  scalingCurves: ScalingCurve[];
  throughputLeaderboard: ThroughputItem[];
  languages: LanguageMeta[];
  scanners: {
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
