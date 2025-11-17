// name-generator.pony - A Pony language version of the robust name generator

use "env"
use "files"
use "random"
use "term"
use "logger"

class EnvHelper
  fun env_or_default(var_name: String, default: String): String =>
    env(var_name) | Some(v) => v else default end

class Config
  let noun_folder: String
  let adj_folder: String
  let separator: String
  let count_o: U32
  
  new create(noun_folder': String = "nouns", adj_folder': String = "adjectives", separator': String = "-") =>
    noun_folder = noun_folder'
    adj_folder = adj_folder'
    separator = separator'
    
    // Number of lines to generate - try tput lines, fallback to 24
    let count_o' : U32 = try
      let lines_env = env("LINES")
      let lines = if lines_env != "" then U32.from(lines_env) else _tput_lines() end
      lines
    else
      24
    end
    count_o = count_o'

  fun _tput_lines(): U32 =>
    try
        let height = terminal_height()
        if height.gt(0) then
            height.U32()
        else
            24
    catch
        24

class FileHandler
  let noun_folder: String
  let adj_folder: String
  
  new create(noun_folder': String, adj_folder': String) =>
    noun_folder = noun_folder'
    adj_folder = adj_folder'

  fun pick_random_file(folder: String): String =>
    // Pick a random regular file from the folder
    let files = File.list(folder)
    let filtered_files = Array.filter(files, { (f) => f.find(".DS_Store") == -1 })
    if filtered_files.size > 0 then
      filtered_files.random()
    else
      ""
    end

class NameGenerator
  let config: Config
  let file_handler: FileHandler
  
  new create(config': Config) =>
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
    var adjective: String = ""
    match adj_lines.random() ? (
        { (line: String) => adjective = line }) | None =>
        ""
    end
    
    var noun: String = ""
    match noun_lines.random() ? (
        { (line: String) => noun = line.lower() }) | None =>
        ""
    end

    let full_name = adjective + config.separator + noun
  
    // Debug output if needed
    if env("DEBUG") == "true" then
      logger.log("Adjective: " + adjective)
      logger.log("Noun: " + noun)
      logger.log("Full Name: " + full_name)
    end
  
    return full_name

actor Main
  new create(env: Env) =>
    let config = Config(
      noun_folder' = EnvHelper.env_or_default("noun_folder", "nouns"),
      adj_folder' = EnvHelper.env_or_default("adj_folder", "adjectives"),
      separator' = EnvHelper.env_or_default("separator", "-")
    )
    
    let name_generator = NameGenerator(config)
    
    // Generate names count_o times
    var name_count: U32 = 0
    while name_count < config.count_o do
      let name = name_generator.generate_name()
      env.out.print(name + "\n")
      name_count = name_count + 1
    end
