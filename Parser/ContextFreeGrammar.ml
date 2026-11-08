
(*Both terminal and non-terminal symbols will be represented by strings*)

module ProductionMap = Map.Make(String)
module SymbolSet = Set.Make(String)
module SymbolMap = Map.Make(String)


(**We will use $ to denote "special" symbols,*)
(**and forbid them in normal symbols*)
(**Represents an epsilon (empty)*)
let epsilon = "$EPSILON$"
(**Represents end of file*)
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

let string_of_symbol_list symbol_list =
  List.fold_left (fun acc_str symbol ->
      acc_str ^ " " ^ symbol
  ) "" symbol_list

let string_of_production production =
  let expansion = string_of_symbol_list production.to_symbols in
  production.from_symbol ^ " ::= " ^ expansion

let string_of_symbol_set symbol_set =
  SymbolSet.fold (fun symbol acc_str ->
    acc_str ^ " " ^ symbol  
  ) symbol_set ""

let string_of_symbol_to_set_map map =
  SymbolMap.fold (fun symbol symbol_set acc_str ->
    let set_str = SymbolSet.fold(fun symbol acc_str ->
      acc_str ^ " " ^ symbol
    ) symbol_set ""
    in
    acc_str ^ "\n" ^ symbol ^ ": " ^ set_str
  ) map ""

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


