#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <random>
#include <string>
#include <vector>

namespace fs = std::filesystem;

/* Helper: get environment variable or fallback */
std::string getEnv(const std::string& var, const std::string& fallback) {
    const char* val = std::getenv(var.c_str());
    return (val && *val) ? std::string(val) : fallback;
}

/* Helper: read all non‑empty lines from a file */
std::vector<std::string> readLines(const fs::path& filePath) {
    std::vector<std::string> lines;
    std::ifstream in(filePath);
    if (!in) {
        std::cerr << "Error: cannot open file " << filePath << "\n";
        std::exit(1);
    }
    std::string line;
    while (std::getline(in, line)) {
        if (!line.empty())
            lines.push_back(line);
    }
    return lines;
}

/* Helper: recursively collect all regular files under a directory */
std::vector<fs::path> collectFiles(const fs::path& folder) {
    std::vector<fs::path> files;
    if (!fs::exists(folder) || !fs::is_directory(folder)) {
        std::cerr << "Error: folder does not exist or is not a directory: " << folder << "\n";
        std::exit(1);
    }
    for (auto const& entry : fs::recursive_directory_iterator(folder)) {
        if (fs::is_regular_file(entry.path())) {
            files.push_back(entry.path());
        }
    }
    if (files.empty()) {
        std::cerr << "Error: no regular files found in folder " << folder << "\n";
        std::exit(1);
    }
    return files;
}

/* Helper: pick a random element from a vector */
template <typename T>
const T& randomChoice(const std::vector<T>& vec, std::mt19937& rng) {
    std::uniform_int_distribution<std::size_t> dist(0, vec.size() - 1);
    return vec[dist(rng)];
}

/* Helper: convert a string to lower case */
std::string toLower(const std::string& s) {
    std::string out;
    out.reserve(s.size());
    std::transform(s.begin(), s.end(), std::back_inserter(out),
                   [](unsigned char c) { return std::tolower(c); });
    return out;
}

/* Debug printer – mirrors the shell script's debugger function */
void debugger(const std::string& adjective,
              const std::string& noun,
              const fs::path& adjFile,
              const fs::path& adjFolder,
              const fs::path& nounFile,
              const fs::path& nounFolder,
              std::size_t countzero,
              std::size_t counto) {
    const char* dbg = std::getenv("DEBUG");
    if (!dbg || std::string(dbg) != "true")
        return;

    std::cerr << "DEBUG:\n";
    std::cerr << "  adjective : " << adjective << "\n";
    std::cerr << "  noun      : " << noun << "\n";
    std::cerr << "  ADJ_FILE  : " << adjFile << "\n";
    std::cerr << "  ADJ_FOLDER: " << adjFolder << "\n";
    std::cerr << "  NOUN_FILE : " << nounFile << "\n";
    std::cerr << "  NOUN_FOLDER: " << nounFolder << "\n";
    std::cerr << "  " << countzero << " > " << counto << "\n";
}

/* Determine terminal height – fallback to 24 if we cannot query it */
std::size_t terminalLines() {
    // Simple fallback; more sophisticated approaches would use ioctl or termsize libs.
    return 24;
}

int main() {
    // Seed RNG
    std::random_device rd;
    std::mt19937 rng(rd());

    // Resolve configuration (environment variables with defaults)
    const std::string separator = getEnv("SEPARATOR", "-");
    const std::string countoEnv = getEnv("counto", "");
    std::size_t counto = 0;
    if (!countoEnv.empty()) {
        try {
            counto = static_cast<std::size_t>(std::stoul(countoEnv));
        } catch (...) {
            counto = terminalLines();
        }
    } else {
        counto = terminalLines();
    }

    const fs::path here = fs::current_path();
    const fs::path nounFolder = fs::path(getEnv("NOUN_FOLDER", (here / "nouns").string()));
    const fs::path adjFolder  = fs::path(getEnv("ADJ_FOLDER",  (here / "adjectives").string()));

    // Pick random files from each folder (recursively)
    const std::vector<fs::path> nounFiles = collectFiles(nounFolder);
    const std::vector<fs::path> adjFiles  = collectFiles(adjFolder);
    const fs::path nounFile = randomChoice(nounFiles, rng);
    const fs::path adjFile  = randomChoice(adjFiles, rng);

    // Load all lines from the selected files
    const std::vector<std::string> nounLines = readLines(nounFile);
    const std::vector<std::string> adjLines  = readLines(adjFile);

    if (nounLines.empty() || adjLines.empty()) {
        std::cerr << "Error: selected noun or adjective file is empty.\n";
        return 1;
    }

    // Main loop – generate names
    std::size_t countzero = 0;
    while (countzero < counto) {
        std::string rawNoun = randomChoice(nounLines, rng);
        std::string rawAdj  = randomChoice(adjLines, rng);

        std::string noun = toLower(rawNoun);
        std::string adjective = rawAdj; // keep original case

        debugger(adjective, noun, adjFile, adjFolder, nounFile, nounFolder, countzero, counto);

        std::cout << adjective << separator << noun << "\n";

        ++countzero;
    }

    return 0;
}
