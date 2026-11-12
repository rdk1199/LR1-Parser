

(**represents an LR1 item [A -> B . C, a ]*)
(**A = from_symbol, B = prev_symbols, C = next_symbols, a = lookahead*)
type lr1_item = {
  from_symbol : string;

  production : int;

  (**position of the dot in the rhs of the production -> dot is in front of the dot_pos'th symbol*)
  dot_pos : int; 
  lookahead : string;
}

let empty_lr1_item = {
  from_symbol = "";
  production = -1;
  dot_pos = -1;
  lookahead = "";
}

let string_of_lr1_item lr1_item = 
  "From symbol: " ^ lr1_item.from_symbol ^"\n" ^
  "Production: " ^ (string_of_int lr1_item.production) ^"\n" ^
  "Dot pos: " ^ (string_of_int lr1_item.dot_pos) ^"\n" ^
  "Lookahead symbol: " ^ lr1_item.lookahead

let lr1_item_is_start_item lr1_item cfg =
  lr1_item.from_symbol = cfg.ContextFreeGrammar.start_symbol &&
  lr1_item.dot_pos = 0 &&
  lr1_item.lookahead = ContextFreeGrammar.eof 


let lr1_item_is_kernel_item lr1_item cfg =
  lr1_item.dot_pos > 0 ||
  lr1_item_is_start_item lr1_item cfg

module LR1ItemOrder = struct
  type t = lr1_item
  (**This compare would be more efficient if we compared the ints before the strings*)
  let compare item_a item_b = 
    if not (item_a.from_symbol = item_b.from_symbol) then
      compare item_a.from_symbol item_b.from_symbol
    else if not (item_a.production = item_b.production) then
      compare item_a.production item_b.production
    else if not (item_a.dot_pos = item_b.dot_pos) then
      compare item_a.dot_pos item_b.dot_pos
    else
      compare item_a.lookahead item_b.lookahead
end

module LR1ItemSet = Set.Make(LR1ItemOrder)
module LR1ItemMap = Map.Make(LR1ItemOrder)

let kernelize lr1_item_set cfg = 
  LR1ItemSet.fold (fun item acc_set ->
    if lr1_item_is_kernel_item item cfg then
      LR1ItemSet.add item acc_set
    else
      acc_set
  ) lr1_item_set LR1ItemSet.empty

type sized_lr1_item_set = {
  lr1_item_set : LR1ItemSet.t;
  size : int;
}

let sized_lr1_item_set_equal s1 s2 =
  if not (s1.size = s2.size) then
    false
  else
    LR1ItemSet.equal s1.lr1_item_set s2.lr1_item_set

let lr1_item_equal item1 item2 =
  LR1ItemOrder.compare item1 item2 = 0

let string_of_lr1_item_set item_set = 
  LR1ItemSet.fold (fun item acc_str ->
    (string_of_lr1_item item) ^ "\n" ^ acc_str
  ) item_set ""

type cc_transition = {
  from_state : int;
  to_state : int;
  symbol : string;
}

let compare_cc_transitions t1 t2 =
  if not (t1.from_state = t2.from_state) then
    compare t1.from_state t2.from_state
  else if not (t1.symbol = t2.symbol) then
    compare t1.symbol t2.symbol
  else
    compare t1.to_state t2.to_state

let string_of_cc_transition transition =
  (string_of_int transition.from_state) ^ " --" ^
  transition.symbol ^ "-->" ^
  (string_of_int transition.to_state)

let string_of_cc_transition_list transition_list = 
  List.fold_left (fun acc_str transition -> 
    acc_str ^ "\n" ^ (string_of_cc_transition transition)  
  ) "" transition_list

(**transitions will be sorted by from_state*)
type canonical_collection = {
  canonical_sets : sized_lr1_item_set list;
  transitions : cc_transition list;
}

let string_of_canonical_collection cc =
  "Canonical sets:\n" ^
  (List.fold_left (fun acc_str canonical_set ->
    acc_str ^ (string_of_lr1_item_set canonical_set.lr1_item_set) ^ "\n ============ \n"  
  ) "" cc.canonical_sets) ^
  "\nTransitions: \n" ^ (string_of_cc_transition_list cc.transitions)

(**This might have to be optimized into some kind of lookup table*)
let find_goto canonical_collection i symbol =
  match List.find_opt (fun elt -> elt.from_state = i && elt.symbol = symbol) canonical_collection.transitions with
  | None -> None
  | Some transition -> Some transition.to_state

let memoized_cc_item_closure item closure_map cfg first_sets first_set_map =
  let open ContextFreeGrammar in
  let memoized_result = LR1ItemMap.find_opt item closure_map in
  match memoized_result with
  | None ->
    let rec closure_rec old_lr1_set lr1_set rec_first_set_map =
    if sized_lr1_item_set_equal old_lr1_set lr1_set then
      (**Last iteration had no effect -> we are done!*)
      (lr1_set, rec_first_set_map)
    else
      let new_lr1_set, new_first_set_map =
      (**Iterate over item set*)
      LR1ItemSet.fold ( fun item (accum_set, accum_first_set_map) ->
        let item_production = Iarray.get cfg.production_array item.production in
        let next_symbols = get_to_symbols_after_n item_production item.dot_pos in
        match next_symbols with
        | [] -> (accum_set, accum_first_set_map)
        | first_next_symbol :: tail ->
          let productions_opt = ProductionMap.find_opt first_next_symbol cfg.nt_to_production_map in
          match productions_opt with
          | None -> (accum_set, accum_first_set_map)
          | Some production_indices ->
            (**Iterate over all productions for this symbol*)
            List.fold_left (fun (inner_accum_set, inner_accum_first_set_map) production_index ->
              let first_set, new_first_set_map = FirstSets.memoized_compute_first_set_of_symbol_list (tail @ [item.lookahead]) first_sets inner_accum_first_set_map in
              let first_set_items =
                (**iterate over the first set*)
                SymbolSet.fold (fun first_set_symbol inner_inner_accum_set ->
                  let new_item = {
                    from_symbol = first_next_symbol;
                    production = production_index;
                    dot_pos = 0;  
                    lookahead = first_set_symbol;
                  }
                  in
                  LR1ItemSet.add new_item inner_inner_accum_set 
                ) first_set inner_accum_set
              in
              (first_set_items, new_first_set_map)
            ) (accum_set, accum_first_set_map) production_indices
      ) lr1_set.lr1_item_set (lr1_set.lr1_item_set, rec_first_set_map)
      in 
      let sized_new_lr1_set = {lr1_item_set = new_lr1_set; size = LR1ItemSet.cardinal new_lr1_set;} in
      closure_rec lr1_set sized_new_lr1_set new_first_set_map
    in
    let closure_lr1_set, final_first_set_map = closure_rec {lr1_item_set = LR1ItemSet.empty; size = 0} {lr1_item_set = (LR1ItemSet.add item LR1ItemSet.empty); size = 1} first_set_map in
    (closure_lr1_set, (LR1ItemMap.add item closure_lr1_set closure_map), final_first_set_map)
  | Some result -> 
    (result, closure_map, first_set_map)

let cc_closure sized_lr1_set cfg first_sets closure_map first_set_map = 
  LR1ItemSet.fold (fun item (accum_set, accum_closure_map, accum_first_set_map) ->
    let item_closure, item_added_closure_map, new_first_set_map = memoized_cc_item_closure item accum_closure_map cfg first_sets accum_first_set_map in
    let new_union = LR1ItemSet.union accum_set.lr1_item_set item_closure.lr1_item_set in
    let new_union_size = LR1ItemSet.cardinal new_union in
    ({lr1_item_set = new_union; size = new_union_size}, item_added_closure_map, new_first_set_map)  
  ) sized_lr1_set.lr1_item_set ({lr1_item_set = LR1ItemSet.empty; size = 0;} , closure_map, first_set_map)

let cc_goto sized_lr1_set symbol cfg first_sets closure_map first_set_map =
  let open ContextFreeGrammar in
  let moved = LR1ItemSet.fold (fun item accum_set ->
    let production = Iarray.get cfg.production_array item.production in
    let next_symbols = get_to_symbols_after_n production item.dot_pos in 
    match next_symbols with
    | [] -> accum_set
    | next_symbol :: tail ->
      if not (next_symbol = symbol) then
        accum_set
      else
        let new_item = {
          from_symbol = item.from_symbol;
          production = item.production;
          dot_pos = item.dot_pos + 1;
          lookahead = item.lookahead
        } in
        LR1ItemSet.add new_item accum_set
  ) sized_lr1_set.lr1_item_set LR1ItemSet.empty
  in
  let sized_moved = {lr1_item_set = moved; size = LR1ItemSet.cardinal moved} in
  cc_closure sized_moved cfg first_sets closure_map first_set_map

let cc_get_start_items cfg =
  let open ContextFreeGrammar in
  match ProductionMap.find_opt cfg.start_symbol cfg.nt_to_production_map with
  | None -> []
  | Some production_list -> 
    List.fold_left (fun acc_items production ->
      {
        from_symbol = cfg.start_symbol;
        production = production;
        dot_pos = 0;
        lookahead = eof;
      } :: acc_items  
    ) [] production_list

(**Helper - check if any set in set list is equal to set - returns (bool, index)*)
let item_set_list_contains_set item_set_list set =
  let index = List.find_index (fun elt -> sized_lr1_item_set_equal elt set) item_set_list in
  match index with
  | None -> (false, -1)
  | Some i -> (true, i)

let process_new_canonical_set canonical_set current_canonical_collection new_canonical_sets new_new_canonical_sets new_canonical_set_index cfg first_sets closure_map first_set_map =
    (**Get the index of the new_canonical_set we are currently processing*)
    (**We index from the END of the list because we are prepending to the lists, so need to keep indices consistent*)
    let transition_from_index = 
      (List.length current_canonical_collection.canonical_sets) + (List.length new_canonical_sets) - new_canonical_set_index - 1
    in
    let all_items_new_canonical_sets, all_items_new_transitions, all_items_new_closure_map, all_items_new_first_set_map =
    LR1ItemSet.fold (fun item (inner_accum_new_canonical_sets, inner_accum_new_transitions, inner_accum_closure_map, inner_accum_first_set_map) ->
      let production = Iarray.get cfg.ContextFreeGrammar.production_array item.production in
      let next_symbols = ContextFreeGrammar.get_to_symbols_after_n production item.dot_pos in
      match next_symbols with
      | [] -> (inner_accum_new_canonical_sets, inner_accum_new_transitions, inner_accum_closure_map, inner_accum_first_set_map) 
      | first_symbol :: remaining_symbols ->
        let goto_cc_set, new_inner_accum_closure_map, new_inner_accum_first_set_map = cc_goto canonical_set first_symbol cfg first_sets inner_accum_closure_map inner_accum_first_set_map in
        (**Check if the computed goto set was already processed*)
        let (is_old_cc_set, old_cc_set_index) = item_set_list_contains_set current_canonical_collection.canonical_sets goto_cc_set in 
        (**Check if the computed goto set is one of the ones we are processing in this iteration of build_canonical_collection*)
        let (is_new_cc_set, new_cc_set_index) = item_set_list_contains_set new_canonical_sets goto_cc_set in
        (**Check if the computed goto set is one of the ones already found while checking a previous new canonical set*)
        let (is_new_new_cc_set, new_new_cc_set_index) = item_set_list_contains_set new_new_canonical_sets goto_cc_set in
        (**Check if the computed goto set is one of the ones already found while checking a previous item in this new canonical set*)
        let (is_new_new_new_cc_set, new_new_new_cc_set_index) = item_set_list_contains_set inner_accum_new_canonical_sets goto_cc_set in

        let transition_to_index = 
        if is_old_cc_set then
          (List.length current_canonical_collection.canonical_sets) - 1 - old_cc_set_index
        else if is_new_cc_set then
          (List.length current_canonical_collection.canonical_sets) +
          (List.length new_canonical_sets) - 1 - new_cc_set_index
        else if is_new_new_cc_set then
          (List.length current_canonical_collection.canonical_sets) +
          (List.length new_canonical_sets) +
          (List.length new_new_canonical_sets) - 1 - new_new_cc_set_index
        else if is_new_new_new_cc_set then
          (List.length current_canonical_collection.canonical_sets) +
          (List.length new_canonical_sets) +
          (List.length new_new_canonical_sets) +
          (List.length inner_accum_new_canonical_sets) - 1 - new_new_new_cc_set_index
        else
          (**NEW canonical set*)
          (*let () = print_endline ("found new canonical set \n" ^ (string_of_lr1_item_set (goto_cc_set.lr1_item_set)) ^ "\n") in*)
          (List.length current_canonical_collection.canonical_sets) +
          (List.length new_canonical_sets) +
          (List.length new_new_canonical_sets) +
          (List.length inner_accum_new_canonical_sets)
        in
        let new_transition = {
          from_state = transition_from_index;
          to_state = transition_to_index;
          symbol = first_symbol;
        }
        in
        if not (is_old_cc_set || is_new_cc_set || is_new_new_cc_set || is_new_new_new_cc_set ) then
          (goto_cc_set :: inner_accum_new_canonical_sets, new_transition :: inner_accum_new_transitions, new_inner_accum_closure_map, new_inner_accum_first_set_map)
        else
          (inner_accum_new_canonical_sets, new_transition :: inner_accum_new_transitions, new_inner_accum_closure_map, new_inner_accum_first_set_map)
    ) canonical_set.lr1_item_set ([], [], closure_map, first_set_map)
    in
    (all_items_new_canonical_sets, all_items_new_transitions, all_items_new_closure_map, all_items_new_first_set_map)
    
(**guarantees that transitions are sorted by start state*)
let construct_canonical_collection cfg = 
  let first_sets = FirstSets.compute_first_sets cfg in
  let rec build_canonical_collection current_canonical_collection new_canonical_sets closure_map first_set_map =
    let () = print_endline 
      ("\r current_canonical_collection_size: " ^ (string_of_int (List.length current_canonical_collection.canonical_sets)) ^"    ") 
    in
    if List.is_empty new_canonical_sets then
      current_canonical_collection
    else
      let (new_new_canonical_sets, new_transitions, _, new_closure_map, new_first_set_map) =
        List.fold_left (fun (accum_new_canonical_sets, accum_new_transitions, new_canonical_set_index, accum_new_closure_map, accum_new_first_set_map) canonical_set ->
          let new_canonical_sets_from_this_set, new_transitions_from_this_set, new_closure_map_from_this_set, new_first_set_map_from_this_set = 
            process_new_canonical_set canonical_set current_canonical_collection new_canonical_sets accum_new_canonical_sets new_canonical_set_index cfg first_sets accum_new_closure_map accum_new_first_set_map in
          (new_canonical_sets_from_this_set @ accum_new_canonical_sets, new_transitions_from_this_set @ accum_new_transitions, new_canonical_set_index + 1, new_closure_map_from_this_set, new_first_set_map_from_this_set)
        ) ([], [], 0, closure_map, first_set_map) new_canonical_sets
      in
      build_canonical_collection 
        {canonical_sets = new_canonical_sets @ current_canonical_collection.canonical_sets; transitions = new_transitions @ current_canonical_collection.transitions}        
        new_new_canonical_sets new_closure_map new_first_set_map
  in
  let start_items = cc_get_start_items cfg in
  let start_item_set =  LR1ItemSet.of_list start_items in
  let start_closure, closure_map, first_set_map = cc_closure ({lr1_item_set = start_item_set; size = LR1ItemSet.cardinal start_item_set;}) cfg first_sets LR1ItemMap.empty FirstSets.FirstSetMap.empty in
  let cc_with_duplicates = build_canonical_collection {canonical_sets = []; transitions = [];} [start_closure;] closure_map first_set_map in
  (**our algorithm doesn't filter duplicate transitions, so filter them now*)
  (**Also reverse the collection list so the states are in order*)
  {canonical_sets = (List.rev cc_with_duplicates.canonical_sets); transitions = List.sort_uniq compare_cc_transitions cc_with_duplicates.transitions}
  