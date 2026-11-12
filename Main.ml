
open Lexer
open Lexer.Tokenizer
open Parser.ContextFreeGrammar
open Parser.FirstSets
open Parser.CanonicalCollection
open Parser.ParseTables
open Parser.ParseTree
open ParserIO.CFGReader
open ParserIO.ParseTableIO


let paren_cfg = read_cfg_from_file "test/paren_cfg_file"

let paren_parse_tables = build_parse_table paren_cfg Fail

let () = write_action_table_to_file "test/new_paren_action" paren_parse_tables.action_table
let () = write_goto_table_to_file "test/new_paren_goto" paren_parse_tables.goto_table

let ocaml_cfg = read_cfg_from_file "test/OCamlGrammar"

let ocaml_parse_tables = build_parse_table ocaml_cfg PreferShift
let () = write_action_table_to_file "test/ocaml_action" ocaml_parse_tables.action_table
let () = write_goto_table_to_file "test/ocaml_goto" ocaml_parse_tables.goto_table







