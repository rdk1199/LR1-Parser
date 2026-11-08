An LR1 Parser implementation in OCaml.

The main interface is:
    
ParserIO.CFGReader.read_cfg_from_file file_name : Reads a CFG in Backus-Naur form from a text file and constructs a CFG data structure
(see test folder for example file format)


Parser.ParseTables.build_parse_table cfg conflict_resolution_strategy : Takes cfg and conflict_resolution_strategy enum and builds LR1 action/goto tables


Parser.ParseTree.parse token_list parse_table : Takes token list and parse_table and constructs a parse tree.


Action/Goto tables can be written and read from text files with ParseIO.ParseTableIO.ml.(read/write)_(action/goto)\_table\_(from/to)_file, so lengthy
table constructions do not have to be done more than once.

Depends on the Lexer library: https://github.com/rdk1199/Lexer
