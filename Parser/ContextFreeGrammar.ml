
(*Both terminal and non-terminal symbols will be represented by strings*)

module ProductionMap = Map.Make(String)
module SymbolSet = Set.Make(String)
module SymbolMap = Map.Make(String)

(**Represents an epsilon (empty)*)
(**We will use $ to denote "special" symbols,*)
(**and forbid them in normal symbols*)
let epsilon = "$EPSILON$"
let eof = "$EOF$"

type production = {
  from_symbol : string;
  to_symbols : string list;
}

type context_free_grammar = {
  start_symbol : string;

  terminals : SymbolSet.t;
  nonterminals : SymbolSet.t;

  productions : (production list) ProductionMap.t;
}

(**Check that two maps Symbol -> SymbolSet are equal*)
let symbol_to_set_maps_equal map_1 map_2 =
  SymbolMap.equal SymbolSet.equal map_1 map_2




