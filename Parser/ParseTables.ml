
type action = 
  | Shift of int
  | Reduce of ContextFreeGrammar.production
  | Accept

let string_of_action action =
  match action with
  | Shift s -> "Shift " ^ (string_of_int s)
  | Reduce production -> "Reduce " ^ (ContextFreeGrammar.string_of_production production)
  | Accept -> "Accept"

type 'a state_symbol_table = {
  entries : 'a ContextFreeGrammar.SymbolMap.t Iarray.t;
}

let string_of_action_map action_map =
  ContextFreeGrammar.SymbolMap.fold (fun symbol action acc_string ->
    acc_string ^ "\n" ^ symbol ^ " " ^ (string_of_action action)
  ) action_map ""

let string_of_goto_map goto_map = 
  ContextFreeGrammar.SymbolMap.fold (fun symbol to_state acc_string ->
    acc_string ^ "\n" ^ symbol ^ " " ^ (string_of_int to_state)
  ) goto_map ""

let string_of_action_table action_table =
  let action_table_str, _ =
  Iarray.fold_left (fun (acc_string, index) action_map ->
    (acc_string ^ "\n" ^ (string_of_int index) ^ ":" ^ (string_of_action_map action_map), index + 1)
  ) ("", 0) action_table.entries
  in
  action_table_str

let string_of_goto_table goto_table =
  let goto_table_str, _ =
  Iarray.fold_left (fun (acc_string, index) goto_map ->
    (acc_string ^ "\n" ^ (string_of_int index) ^ ":" ^ (string_of_goto_map goto_map), index + 1)
  ) ("", 0) goto_table.entries
  in
  goto_table_str

type parse_table = {
  action_table : action state_symbol_table;
  goto_table : int state_symbol_table;
}

let string_of_parse_table parse_table =
  "ACTION: \n" ^ (string_of_action_table parse_table.action_table) ^"\nGOTO: \n" ^ (string_of_goto_table parse_table.goto_table) ^ "\n"

let build_action_table cfg canonical_collection = 
  let open ContextFreeGrammar in
  let open CanonicalCollection in
  let action_entry_map_list, _ =
  List.fold_left (fun (acc_action_map_list, cc_index) canonical_set  ->
    let map_for_this_cc = 
    LR1ItemSet.fold ( fun item acc_action_map ->
      (**TODO: detect conflicts*)
      match item.next_symbols with
      | [] ->
        if item.from_symbol = cfg.start_symbol && item.lookahead = eof then
          SymbolMap.add eof Accept acc_action_map
        else 
          SymbolMap.add item.lookahead (Reduce {from_symbol = item.from_symbol; to_symbols = item.prev_symbols;}) acc_action_map
      | next_symbol :: remaining_symbols ->
        if SymbolSet.mem next_symbol cfg.terminals then
          match find_goto canonical_collection cc_index next_symbol with
          | None -> assert(false) (*Should not happen*)
          | Some j ->  SymbolMap.add next_symbol (Shift j) acc_action_map
        else
          (**ignore nonterminals*)
          acc_action_map
    ) canonical_set SymbolMap.empty
    in
    (map_for_this_cc :: acc_action_map_list, cc_index + 1)
  ) ([], 0) canonical_collection.canonical_sets
  in
  (**reverse the list because we are prepending in the above loop*)
  {entries = Iarray.of_list (List.rev action_entry_map_list); }

(**assume transitions are sorted by start state*)
let build_goto_table cfg canonical_collection =
  let open ContextFreeGrammar in
  let open CanonicalCollection in
  let map_list, last_state_processed =
  List.fold_left (fun (acc_goto_map_list, prev_state) transition ->
    let from_state = transition.from_state in
    let symbol = transition.symbol in
    let to_state = transition.to_state in
    if not (prev_state = from_state) then
      let padding = List.init (from_state - prev_state - 1) (fun _ -> SymbolMap.empty) in
      if SymbolSet.mem symbol cfg.nonterminals then
        (**new state - create new map with one entry and attach it to the list*)
        (**There may be states with no transitions from them at all, so from_state > prev_state + 1 in that case, so pad the list as well*)
        
        ((SymbolMap.add symbol to_state SymbolMap.empty) :: padding @ acc_goto_map_list, from_state)
      else
        (**Create a new empty map for this state and attach it to the list*)
        (SymbolMap.empty :: padding @ acc_goto_map_list, from_state)
    else
      if SymbolSet.mem symbol cfg.nonterminals then
        match acc_goto_map_list with
        | [] -> assert(false) (*This should not happen as we should have created a map for this state when first entering it*)
        | existing_map :: tail -> ((SymbolMap.add symbol to_state existing_map) :: tail, from_state)
      else
        (**ignore terminals, and there is already a map for this state*)
        (acc_goto_map_list, from_state)     
  ) ([], -1) canonical_collection.transitions
  in
  (**Pad the end of the list for trailing states with no outward transitions*)
  let padding = List.init (((List.length canonical_collection.canonical_sets) - 1) - last_state_processed) (fun _ -> SymbolMap.empty) in
  (**Reverse the list because we were prepending*)
  {entries = Iarray.of_list (List.rev (padding @ map_list));}

let build_parse_table cfg canonical_collection = {
  action_table = build_action_table cfg canonical_collection;
  goto_table = build_goto_table cfg canonical_collection;
}