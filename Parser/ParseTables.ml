
type action = 
  | Shift of int * CanonicalCollection.lr1_item (**Include the LR1 item for debug output*)
  | Reduce of ContextFreeGrammar.production
  | Accept of ContextFreeGrammar.production

let string_of_action action =
  match action with
  | Shift (s, item) -> "Shift " ^ (string_of_int s) ^ ", " ^ (CanonicalCollection.string_of_lr1_item item)
  | Reduce production -> "Reduce " ^ (ContextFreeGrammar.string_of_production production)
  | Accept production -> "Accept " ^ (ContextFreeGrammar.string_of_production production)

let action_equal action1 action2 =
  match (action1, action2) with
  | Shift (s1, _), Shift(s2, _) -> s1 = s2 (**the item is only needed for debug purposes - the shift is the only thing that actually matters*)
  | Reduce prod1, Reduce prod2 -> ContextFreeGrammar.production_equal prod1 prod2
  | Accept prod1, Accept prod2 -> ContextFreeGrammar.production_equal prod1 prod2
  | _, _ -> false

type conflict_resolution_opt =
  | Fail
  | PreferShift
  | PreferReduce

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

let action_table_get action_table state symbol =
  let opt_action = ContextFreeGrammar.SymbolMap.find_opt symbol (Iarray.get action_table.entries state) in
  match opt_action with
  | None -> None
  | Some action -> Some action

let goto_table_get goto_table state symbol =
  let opt_state = ContextFreeGrammar.SymbolMap.find_opt symbol (Iarray.get goto_table.entries state) in
  match opt_state with
  | None -> None
  | Some to_state -> Some to_state

let string_of_parse_table parse_table =
  "ACTION: \n" ^ (string_of_action_table parse_table.action_table) ^"\nGOTO: \n" ^ (string_of_goto_table parse_table.goto_table) ^ "\n"



let string_of_shift_reduce_conflict_message existing_action new_action conflict_resolution_opt cc_index symbol =
   ("Shift/Reduce conflict for state " ^ (string_of_int cc_index) ^ ", symbol " ^ symbol ^ ":\n") ^ 
   ((string_of_action existing_action) ^ "  |  " ^ (string_of_action new_action) ^ "\n") ^
   (
      match conflict_resolution_opt with
      | PreferShift -> "Preferring shift\n"
      | PreferReduce -> "Preferring reduce\n"
      | Fail -> "Failing\n"
   )

let insert_action action_map symbol action conflict_resolution_opt cc_index =
  let existing_action_opt = ContextFreeGrammar.SymbolMap.find_opt symbol action_map in
  let new_map = ContextFreeGrammar.SymbolMap.add symbol action action_map in
  match existing_action_opt with
  | None -> new_map (**No conflict*)
  | Some existing_action ->
    if action_equal existing_action action then
      new_map (**No conflict because actions are the same*)
    else
      match existing_action, action, conflict_resolution_opt with
      | _, _, Fail ->                                    (**If conflict resolution strategy is to fail, then always fail*)
        let () = print_endline ("Unresolvable conflict for state " ^ (string_of_int cc_index) ^ ", symbol " ^ symbol ^ " :\n") in
        let () = print_endline ((string_of_action existing_action) ^ "  |  " ^ (string_of_action action) ^ "\n") in
        assert(false)        (**Don't resolve reduce-reduce conflicts*)
      | Accept _, _, _                                (**Don't resolve accept conflicts*)
      | _, Accept _, _      
      | Shift _, Shift _, _                           (**Don't resolve shift-shift conflicts*)
      | Reduce _, Reduce _, _ ->                      (**Don't resolve reduce-reduce conflicts*)
        let () = print_endline ("Unresolvable conflict for state " ^ (string_of_int cc_index) ^ ", symbol " ^ symbol ^ " :\n") in
        let () = print_endline ((string_of_action existing_action) ^ "  |  " ^ (string_of_action action) ^ "\n") in
        new_map        (**Report, overwrite, and continue*)
      | Shift _, Reduce _, PreferShift ->                
        (**Don't override shift if we prefer shifts*)
        let () = print_endline (string_of_shift_reduce_conflict_message existing_action action conflict_resolution_opt cc_index symbol) in 
        action_map
      | Reduce _, Shift _, PreferReduce ->  
        (**Don't override reduce if we prefer reduce*)
        let () = print_endline (string_of_shift_reduce_conflict_message existing_action action conflict_resolution_opt cc_index symbol) in
        action_map
      | Shift _, Reduce _, PreferReduce ->               
        (**Override shift with reduce if reduce preferred*)
        let () = print_endline (string_of_shift_reduce_conflict_message existing_action action conflict_resolution_opt cc_index symbol) in
        new_map  
      | Reduce _, Shift _, PreferShift -> 
        let () = print_endline (string_of_shift_reduce_conflict_message existing_action action conflict_resolution_opt cc_index symbol) in
        new_map      


 


let build_action_table cfg canonical_collection conflict_resolution_opt = 
  let open ContextFreeGrammar in
  let open CanonicalCollection in
  let action_entry_map_list, _ =
  List.fold_left (fun (acc_action_map_list, cc_index) canonical_set  ->
    let map_for_this_cc = 
    LR1ItemSet.fold ( fun item acc_action_map ->
      match item.next_symbols with
      | [] ->
        if item.from_symbol = cfg.start_symbol && item.lookahead = eof then
          let new_action = Accept {from_symbol = item.from_symbol; to_symbols = item.prev_symbols;} in
          insert_action acc_action_map eof new_action conflict_resolution_opt cc_index
        else 
          let new_action = Reduce {from_symbol = item.from_symbol; to_symbols = item.prev_symbols;} in
          insert_action acc_action_map item.lookahead new_action conflict_resolution_opt cc_index
      | next_symbol :: remaining_symbols ->
        if SymbolSet.mem next_symbol cfg.terminals then
          match find_goto canonical_collection cc_index next_symbol with
          | None -> assert(false) (*Should not happen*)
          | Some j ->  
            let new_action = Shift (j, item) in
            insert_action acc_action_map next_symbol new_action conflict_resolution_opt cc_index
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

let build_parse_table cfg conflict_resolution_strategy = 
  let canonical_collection = CanonicalCollection.construct_canonical_collection cfg in
  let () = print_endline ("Canonical collection constructed: " ^ (string_of_int (List.length canonical_collection.canonical_sets)) ^ " states,\n"
    ^ (string_of_int (List.length canonical_collection.transitions)) ^ "\n"
  )
  in
  {
    action_table = build_action_table cfg canonical_collection conflict_resolution_strategy;
    goto_table = build_goto_table cfg canonical_collection;
  }