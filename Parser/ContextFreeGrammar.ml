
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

let get_to_symbols_after_n production n = 
  if n >= (List.length production.to_symbols) then
    []
  else
    List.drop n production.to_symbols

let production_equal prod1 prod2 =
  (prod1.from_symbol = prod2.from_symbol) &&
  (prod1.to_symbols = prod2.to_symbols)

type context_free_grammar = {
  start_symbol : string;

  terminals : SymbolSet.t;
  nonterminals : SymbolSet.t;

  (**Store the actual string -> string list productions*)
  production_array : production Iarray.t;

  (**Map nonterminal -> indices of productions starting with it*)
  nt_to_production_map : (int list) ProductionMap.t;
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

let string_of_cfg cfg =
  "Start symbol: " ^ cfg.start_symbol ^ "\n" ^
  "Terminals: " ^ (string_of_symbol_set cfg.terminals) ^ "\n" ^
  "Nonterminals: " ^ (string_of_symbol_set cfg.nonterminals) ^ "\n" ^
  "Productions: " ^ (Iarray.fold_left (fun  acc_str production ->
    acc_str ^ " " ^ (string_of_production production) 
  ) "" cfg.production_array)


let string_of_symbol_to_set_map map =
  SymbolMap.fold (fun symbol symbol_set acc_str ->
    let set_str = SymbolSet.fold(fun symbol acc_str ->
      acc_str ^ " " ^ symbol
    ) symbol_set ""
    in
    acc_str ^ "\n" ^ symbol ^ ": " ^ set_str
  ) map ""

(*production_str_list is a list of list of symbol - the head is the lhs, remaining symbols are rhs*)
let build_cfg start_symbol production_str_list = 
    let production_list = List.fold_left (fun acc_list production_strs ->
      match production_strs with
      | [] -> acc_list (**remove empty productions*)
      | from_symbol :: to_symbols ->
        {from_symbol = from_symbol; to_symbols = to_symbols;} :: acc_list
    ) [] production_str_list
    in
    let production_array = Iarray.of_list production_list in
    let production_map, nonterminal_symbols, all_symbols, _ = Iarray.fold_left (fun (acc_map, acc_nts, acc_symbols, acc_index) production ->
      let new_nt_set = (SymbolSet.add production.from_symbol acc_nts) in
      let new_acc_symbols = SymbolSet.add production.from_symbol (SymbolSet.union (SymbolSet.of_list production.to_symbols) acc_symbols) in
      let old_entry = ProductionMap.find_opt production.from_symbol acc_map in
      match old_entry with
      | None -> (ProductionMap.add production.from_symbol [acc_index] acc_map, new_nt_set, new_acc_symbols, acc_index + 1)
      | Some old_production_list ->
        (ProductionMap.add production.from_symbol (acc_index :: old_production_list) acc_map, new_nt_set, new_acc_symbols, acc_index + 1)
    ) (ProductionMap.empty, SymbolSet.empty, (SymbolSet.add eof SymbolSet.empty), 0) production_array
    in
    {
      start_symbol = start_symbol;
      terminals = SymbolSet.diff all_symbols nonterminal_symbols;
      nonterminals = nonterminal_symbols;
      production_array = production_array;
      nt_to_production_map = production_map;
    }


