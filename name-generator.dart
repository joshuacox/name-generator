#!/usr/bin/env dart
import 'dart:io';
import 'dart:math';

/// Returns the value of an environment variable or [fallback] if not set.
String envOr(String name, String fallback) =>
    Platform.environment[name] ?? fallback;

/// Returns the integer value of an environment variable or null if not set or
/// not a valid integer.
int? envInt(String name) {
  final val = Platform.environment[name];
  if (val == null) return null;
  return int.tryParse(val);
}

/// Executes `tput lines` to obtain the terminal height. Returns null on any
/// failure.
int? _tputLines() {
  try {
    final result = Process.runSync('tput', ['lines']);
    if (result.exitCode == 0) {
      return int.tryParse(result.stdout.toString().trim());
    }
  } catch (_) {
    // ignore – fall back to default
  }
  return null;
}

/// Determines how many lines to emit. Order:
///   1. Environment variable `counto`
///   2. `tput lines`
///   3. Default 24
int getCountO() {
  return envInt('counto') ??
      _tputLines() ??
      24;
}

/// Picks a random regular file from [folderPath]. Throws if none found.
File pickRandomFile(String folderPath) {
  final dir = Directory(folderPath);
  if (!dir.existsSync()) {
    throw StateError('Folder does not exist: $folderPath');
  }
  final files = dir
      .listSync()
      .whereType<File>()
      .toList();

  if (files.isEmpty) {
    throw StateError('Folder "$folderPath" contains no regular files.');
  }
  final random = Random();
  return files[random.nextInt(files.length)];
}

/// Returns a [File] based on an environment variable or a random file from
/// [folderPath] if the variable is not set or empty.
File resolveFile(String envVar, String folderPath) {
  final envVal = Platform.environment[envVar];
  if (envVal != null && envVal.trim().isNotEmpty) {
    final f = File(envVal);
    if (!f.existsSync() || f.statSync().type != FileSystemEntityType.file) {
      throw StateError('Environment variable $envVar points to a non‑regular file: $envVal');
    }
    return f;
  }
  return pickRandomFile(folderPath);
}

/// Reads all non‑empty, trimmed lines from [file].
List<String> readNonEmptyLines(File file) {
  final content = file.readAsStringSync();
  return content
      .split('\n')
      .map((l) => l.trim())
      .where((l) => l.isNotEmpty)
      .toList();
}

/// Prints debug information to stderr when DEBUG=true.
void debugPrintInfo({
  required String adjective,
  required String noun,
  required File nounFile,
  required File adjFile,
  required Directory nounFolder,
  required Directory adjFolder,
  required int iteration,
  required int counto,
}) {
  if (Platform.environment['DEBUG'] == 'true') {
    final stderrSink = stderr;
    stderrSink.writeln('DEBUG iteration $iteration/$counto');
    stderrSink.writeln('  adjective : $adjective');
    stderrSink.writeln('  noun      : $noun');
    stderrSink.writeln('  NOUN_FILE : ${nounFile.path}');
    stderrSink.writeln('  ADJ_FILE  : ${adjFile.path}');
    stderrSink.writeln('  NOUN_FOLDER : ${nounFolder.path}');
    stderrSink.writeln('  ADJ_FOLDER  : ${adjFolder.path}');
  }
}

void main() {
  // Configuration
  final separator = envOr('SEPARATOR', '-');

  final cwd = Directory.current.path;
  final nounFolderPath = envOr('NOUN_FOLDER', '$cwd/nouns');
  final adjFolderPath = envOr('ADJ_FOLDER', '$cwd/adjectives');

  final nounFolder = Directory(nounFolderPath);
  final adjFolder = Directory(adjFolderPath);

  final nounFile = resolveFile('NOUN_FILE', nounFolderPath);
  final adjFile = resolveFile('ADJ_FILE', adjFolderPath);

  final nounLines = readNonEmptyLines(nounFile);
  final adjLines = readNonEmptyLines(adjFile);

  if (nounLines.isEmpty) {
    stderr.writeln('Error: noun list is empty (${nounFile.path})');
    exit(1);
  }
  if (adjLines.isEmpty) {
    stderr.writeln('Error: adjective list is empty (${adjFile.path})');
    exit(1);
  }

  final counto = getCountO();
  final random = Random();

  for (var i = 0; i < counto; i++) {
    final nounRaw = nounLines[random.nextInt(nounLines.length)];
    final adjRaw = adjLines[random.nextInt(adjLines.length)];

    final noun = nounRaw.toLowerCase();
    final adjective = adjRaw; // preserve case

    debugPrintInfo(
      adjective: adjective,
      noun: noun,
      nounFile: nounFile,
      adjFile: adjFile,
      nounFolder: nounFolder,
      adjFolder: adjFolder,
      iteration: i + 1,
      counto: counto,
    );

    stdout.writeln('$adjective$separator$noun');
  }
}
