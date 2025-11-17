import Foundation

func getEnvOrFallback(_ varName: String, _ defaultValue: String) -> String {
    guard let envVal = ProcessInfo.processInfo.environment[varName] else {
        return defaultValue
    }
    if envVal.isEmpty {
        return defaultValue
    }
    return envVal
}

func resolveFile(envVar: String, defaultFolder: String) throws -> URL {
    guard let envVal = ProcessInfo.processInfo.environment[envVar] else {
        // Use default folder and pick random file from there
        return try pickRandomFileInFolder(defaultFolder)
    }
    
    if envVal.isEmpty {
        throw NSError(domain: "Environment variable \(envVar) is empty", code: 1)
    }
    
    guard let url = URL(string: envVal) else {
        throw NSError(domain: "Invalid URL from environment variable \(envVar)", code: 2)
    }
    
    if !url.isFileURL {
        throw NSError(domain: "URL \(url) is not a file", code: 3)
    }
    
    return url
}

func pickRandomFileInFolder(_ folderPath: String) throws -> URL {
    let folderUrl = URL(fileURLWithPath: folderPath)
    let fileManager = FileManager.default
    
    guard let contents = try? fileManager.contentsOfDirectory(at: folderUrl, includingPropertiesForKeys: nil) else {
        throw NSError(domain: "Could not read directory \(folderUrl.path)", code: 4)
    }
    
    let files = contents.filter { $0.isFileURL }
    if files.isEmpty {
        throw NSError(domain: "No files found in directory \(folderUrl.path)", code: 5)
    }
    
    return files[Int.random(in: 0..<files.count)]
}

func readNonEmptyLines(from url: URL) throws -> [String] {
    do {
        let content = try String(contentsOf: url, encoding: .utf8)
        return content.components(separatedBy: .newlines).filter { !$0.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty }
    } catch {
        throw NSError(domain: "Could not read file \(url.path)", code: 6)
    }
}

func maybeDebug(_ adjective: String, _ noun: String, nounFile: URL, adjFile: URL) {
    if ProcessInfo.processInfo.environment["DEBUG"] == "true" {
        print("Adjective:", adjective)
        print("Noun:", noun)
        print("Adj File:", adjFile.path)
        print("Noun File:", nounFile.path)
    }
}

func main() {
    let separator = getEnvOrFallback("SEPARATOR", "-")
    
    do {
        // Resolve files
        guard var nounFile = try resolveFile(envVar: "NOUN_FILE", defaultFolder: "nouns") else {
            throw NSError(domain: "Could not resolve NOUN_FILE", code: 7)
        }
        
        guard var adjFile = try resolveFile(envVar: "ADJ_FILE", defaultFolder: "adjectives") else {
            throw NSError(domain: "Could not resolve ADJ_FILE", code: 8)
        }
        
        // Read lines
        let adjectives = try readNonEmptyLines(from: adjFile)
        let nouns = try readNonEmptyLines(from: nounFile)
        
        if adjectives.isEmpty || nouns.isEmpty {
            print("No valid entries in files")
            return
        }
        
        // Determine count of lines to emit
        let counto = Int(getEnvOrFallback("counto", (ProcessInfo.processInfo.environment["LINES"] ?? "24")))
            ?? 24
        
        for _ in 0..<counto {
            let randomAdjective = adjectives.randomElement()!
            let randomNoun = nouns.randomElement()!.lowercased()
            
            maybeDebug(randomAdjective, randomNoun, nounFile: nounFile, adjFile: adjFile)
            
            print("\(randomAdjective)\(separator)\(randomNoun)")
        }
    } catch {
        print("Error: \(error.localizedDescription)")
    }
}

main()
