
open Lexer
open Lexer.Tokenizer
open Parser.ContextFreeGrammar
open Parser.FirstSets
open Parser.CanonicalCollection
open Parser.ParseTables
open Parser.ParseTree
open ParserIO.CFGReader
open ParserIO.ParseTableIO


let ocaml_cfg = read_cfg_from_file "test/OCamlGrammar"

let ocaml_canonical_collection = construct_canonical_collection ocaml_cfg
let () = print_endline (string_of_canonical_collection ocaml_canonical_collection)







