// name-generator.pony - A Pony language version of the robust name generator

use "files"
use "random"
use "arrays"
use "env"

class Config
  let noun_folder: String
  let adj_folder: String
  let separator: String
  let count_o: U32
  
  new envVar(name: String, default: String) =>
    // Default values
    noun_folder := env_or_default("NOUN_FOLDER", "nouns")
    adj_folder := env_or_default("ADJ_FOLDER", "adjectives")
    separator := env_or_default("SEPARATOR", "-")
    
    // Number of lines to generate - try tput lines, fallback to 24
    let tput_lines = Process.env().get("LINES") 
    count_o := match tput_lines:
      | Some(val) => U32.from(String(val))
      | None => 24u32

class FileHandler
  let noun_folder: String
  let adj_folder: String
  
  new noun_folder': String, adj_folder': String =>
    noun_folder = noun_folder'
    adj_folder = adj_folder'

  fun pick_random_file(folder: String): String =>
    // Pick a random regular file from the folder
    let files = Files.list(folder)
    let filtered_files = Array.filter(files, { (f) => f.find(".DS_Store") == -1 })
    if filtered_files.size > 0 then
      filtered_files.random()
    else
      ""

class NameGenerator
  let config: Config
  let file_handler: FileHandler
  
  new config' =>
    config = config'
    file_handler = FileHandler(config.noun_folder, config.adj_folder)

  fun generate_name(): String =>
    // Pick random adjective and noun files
    let adj_file = file_handler.pick_random_file(config.adj_folder)
    let noun_file = file_handler.pick_random_file(config.noun_folder)
    
    // Read lines from files
    let adj_lines = FileLines.read_and_filter(adj_file, { (l) => l != "" })
    let noun_lines = FileLines.read_and_filter(noun_file, { (l) => l != "" })
    
    // Pick random entries preserving case for adjectives, lowercasing nouns
    let adjective = adj_lines.random()
    let noun = noun_lines.random().lower()
    
    // Debug output if needed
    if Process.env().get("DEBUG") == "true" then
      Debug.out("Adjective: " + adjective)
      Debug.out("Noun: " + noun)
  
    return adjective + config.separator + noun

// Main function
actor Main
  new create(env: Env) =>
    let config = Config(env_or_default<String>("counto", "24"))
    let name_generator = NameGenerator(config)
    
    // Generate names count_o times
    for count in 0..(config.count_o - 1) do
      let name = name_generator.generate_name()
      env.out.print(name + "\n")
