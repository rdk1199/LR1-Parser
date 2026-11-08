
open Lexer
open Lexer.Tokenizer
open Parser.ContextFreeGrammar
open Parser.FirstSets
open Parser.CanonicalCollection
open Parser.ParseTables
open Parser.ParseTree

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

let init_item = {
  from_symbol = "Goal";
  prev_symbols = [];
  next_symbols = ["List"];
  lookahead = eof;
}

let init_item_set = LR1ItemSet.add init_item LR1ItemSet.empty

let closure = cc_closure init_item_set test_cfg first_sets

let () = print_endline "Start closure: "
let () = print_endline (string_of_lr1_item_set closure)
let () = print_endline "Goto: "
let () = print_endline (string_of_lr1_item_set (cc_goto closure "(" test_cfg first_sets))

let canonical_collection = construct_canonical_collection test_cfg
let () = print_endline (string_of_canonical_collection canonical_collection)

let parse_table = build_parse_table test_cfg canonical_collection
let () = print_endline ("PARSE TABLE:\n" ^ (string_of_parse_table parse_table))

let paren_regex = "\\(|\\)"
let tokenizer = build_tokenizer [("(", "("); (")", ")")] [("PAREN",paren_regex)]

let test_string_1 = "(((())))(())((()))()((()))()"
let token_list_1 = tokenize_string tokenizer test_string_1
let () = print_endline (token_list_to_string token_list_1)

let _ = parse token_list_1 parse_table


