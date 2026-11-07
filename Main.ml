
open Lexer
open Parser.ContextFreeGrammar
open Parser.FirstSets

let () = print_endline "Hello, World!"


let test_cfg_production_list = [
  ["Goal"; "List"];
  ["List"; "List"; "Pair"];
  ["List"; "Pair"];
  ["Pair"; "("; "Pair"; ")"];
  ["Pair"; "("; ")"];
]

let terminal_list = ["("; ")"]
let nonterminal_list = ["Goal"; "List"; "Pair"]
let start_symbol = "Goal"

let test_cfg = build_cfg start_symbol terminal_list nonterminal_list test_cfg_production_list

let first_sets = compute_first_sets test_cfg

let () = print_endline (string_of_symbol_to_set_map first_sets)