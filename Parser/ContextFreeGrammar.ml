
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

let print_symbol_to_set_map map =
  SymbolMap.iter (fun symbol symbol_set ->
    let set_str = SymbolSet.fold(fun symbol acc_str ->
      acc_str ^ " " ^ symbol
    ) symbol_set ""
    in
    print_endline (symbol ^ ": " ^ set_str)
  ) map 

(*production_list is a list of list of symbol - the head is the lhs, remaining symbols are rhs*)
let build_cfg start_symbol terminal_list nonterminal_list production_list = 
    let terminal_set = SymbolSet.of_list terminal_list in
    let nonterminal_set = SymbolSet.of_list nonterminal_list in
    let production_map = List.fold_left (fun acc_map symbol_list ->
      match symbol_list with
      | [] -> acc_map
      | hd :: tail ->
        let new_production = {
          from_symbol = hd;
          to_symbols = tail;
        } in
        let old_entry = ProductionMap.find_opt hd acc_map in
        match old_entry with
        | None -> ProductionMap.add hd [new_production] acc_map
        | Some old_production_list ->
          ProductionMap.add hd (new_production :: old_production_list) acc_map
    ) ProductionMap.empty production_list
    in
    {
      start_symbol = start_symbol;
      terminals = terminal_set;
      nonterminals = nonterminal_set;
      productions = production_map;
    }


