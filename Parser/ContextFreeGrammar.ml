
(*Both terminal and non-terminal symbols will be represented by strings*)

module ProductionMap = Map.Make(String)

type production = {
  from_symbol : string;
  to_symbols : string list;
}

type context_free_grammar = {
  start_symbol : string;
  productions : (production list) ProductionMap.t;
}