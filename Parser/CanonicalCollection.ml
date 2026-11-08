

(**represents an LR1 item [A -> B . C, a ]*)
(**A = from_symbol, B = prev_symbols, C = next_symbols, a = lookahead*)
type lr1_item = {
  from_symbol : string;
  prev_symbols : string list;
  next_symbols : string list;
  lookahead : string;
}

 let string_of_lr1_item lr1_item = 
  "[ " ^ lr1_item.from_symbol ^ " -> " ^
  (ContextFreeGrammar.string_of_symbol_list lr1_item.prev_symbols) ^ " . " ^
  (ContextFreeGrammar.string_of_symbol_list lr1_item.next_symbols) ^ ",  " ^
  lr1_item.lookahead ^ "]"

module LR1ItemOrder = struct
  type t = lr1_item
  let compare item_a item_b = 
    if not (item_a.from_symbol = item_b.from_symbol) then
      compare item_a.from_symbol item_b.from_symbol
    else if not (item_a.prev_symbols = item_b.prev_symbols) then
      compare item_a.prev_symbols item_b.prev_symbols
    else if not (item_a.next_symbols = item_b.next_symbols) then
      compare item_a.next_symbols item_b.next_symbols
    else
      compare item_a.lookahead item_b.lookahead
end

module LR1ItemSet = Set.Make(LR1ItemOrder)

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

type canonical_collection = {
  canonical_sets : LR1ItemSet.t list;
  transitions : cc_transition list;
}

let string_of_canonical_collection cc =
  "Canonical sets:\n" ^
  (List.fold_left (fun acc_str canonical_set ->
    (string_of_lr1_item_set canonical_set) ^ "\n ============ \n" ^ acc_str  
  ) "" cc.canonical_sets) ^
  "\nTransitions: \n" ^
  (List.fold_left (fun acc_str transition ->
    (string_of_cc_transition transition) ^ "\n" ^ acc_str
  ) "" cc.transitions)


let cc_closure lr1_set cfg first_sets = 
  let open ContextFreeGrammar in
  let rec closure_rec old_lr1_set lr1_set =
    if LR1ItemSet.equal old_lr1_set lr1_set then
      (**Last iteration had no effect -> we are done!*)
      lr1_set
    else
      let new_lr1_set =
      LR1ItemSet.fold ( fun item accum_set ->
        let next_symbols = item.next_symbols in
        match next_symbols with
        | [] -> accum_set
        | first_next_symbol :: tail ->
          let productions_opt = ProductionMap.find_opt first_next_symbol cfg.productions in
          match productions_opt with
          | None -> accum_set
          | Some productions ->
            List.fold_left (fun inner_accum_set production ->
              let first_set = FirstSets.compute_first_set_of_symbol_list (tail @ [item.lookahead]) first_sets in
              SymbolSet.fold (fun first_set_symbol inner_inner_accum_set ->
                let new_item = {
                  from_symbol = first_next_symbol;
                  prev_symbols = [];
                  next_symbols = production.to_symbols;
                  lookahead = first_set_symbol
                }
                in
                LR1ItemSet.add new_item inner_inner_accum_set 
              ) first_set inner_accum_set
            ) accum_set productions
      ) lr1_set lr1_set
      in
      closure_rec lr1_set new_lr1_set
  in
  closure_rec LR1ItemSet.empty lr1_set

let cc_goto lr1_set symbol cfg first_sets =
  let moved = LR1ItemSet.fold (fun item accum_set -> 
    match item.next_symbols with
    | [] -> accum_set
    | next_symbol :: tail ->
      if not (next_symbol = symbol) then
        accum_set
      else
        let new_item = {
          from_symbol = item.from_symbol;
          prev_symbols = item.prev_symbols @ [next_symbol];
          next_symbols = tail;
          lookahead = item.lookahead
        } in
        LR1ItemSet.add new_item accum_set
  ) lr1_set LR1ItemSet.empty
  in
  cc_closure moved cfg first_sets

let cc_get_start_items cfg =
  let open ContextFreeGrammar in
  match ProductionMap.find_opt cfg.start_symbol cfg.productions with
  | None -> []
  | Some production_list -> 
    List.fold_left (fun acc_items production ->
      {
        from_symbol = cfg.start_symbol;
        prev_symbols = [];
        next_symbols = production.to_symbols;
        lookahead = eof;
      } :: acc_items  
    ) [] production_list

(**Helper - check if any set in set list is equal to set - returns (bool, index)*)
let item_set_list_contains_set item_set_list set =
  let index = List.find_index (fun elt -> LR1ItemSet.equal elt set) item_set_list in
  match index with
  | None -> (false, -1)
  | Some i -> (true, i)

let process_new_canonical_set canonical_set current_canonical_collection new_canonical_sets new_new_canonical_sets new_canonical_set_index cfg first_sets =
    (**Get the index of the new_canonical_set we are currently processing*)
    (**We index from the END of the list because we are prepending to the lists, so need to keep indices consistent*)
    let transition_from_index = 
      (List.length current_canonical_collection.canonical_sets) + (List.length new_canonical_sets) - new_canonical_set_index - 1
    in
    let all_items_new_canonical_sets, all_items_new_transitions =
    LR1ItemSet.fold (fun item (inner_accum_new_canonical_sets, inner_accum_new_transitions) ->
      match item.next_symbols with
      | [] -> (inner_accum_new_canonical_sets, inner_accum_new_transitions) 
      | first_symbol :: remaining_symbols ->
        let goto_cc_set = cc_goto canonical_set first_symbol cfg first_sets in
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
          (goto_cc_set :: inner_accum_new_canonical_sets, new_transition :: inner_accum_new_transitions)
        else
          (inner_accum_new_canonical_sets, new_transition :: inner_accum_new_transitions)
    ) canonical_set ([], [])
    in
    (all_items_new_canonical_sets, all_items_new_transitions)
    

let construct_canonical_collection cfg = 
  let first_sets = FirstSets.compute_first_sets cfg in
  let rec build_canonical_collection current_canonical_collection new_canonical_sets =
    if List.is_empty new_canonical_sets then
      current_canonical_collection
    else
      let (new_new_canonical_sets, new_transitions, _) =
        List.fold_left (fun (accum_new_canonical_sets, accum_new_transitions, new_canonical_set_index) canonical_set ->
          let new_canonical_sets_from_this_set, new_transitions_from_this_set = 
            process_new_canonical_set canonical_set current_canonical_collection new_canonical_sets accum_new_canonical_sets new_canonical_set_index cfg first_sets in
          (new_canonical_sets_from_this_set @ accum_new_canonical_sets, new_transitions_from_this_set @ accum_new_transitions, new_canonical_set_index + 1)
        ) ([], [], 0) new_canonical_sets
      in
      build_canonical_collection 
        {canonical_sets = new_canonical_sets @ current_canonical_collection.canonical_sets; transitions = new_transitions @ current_canonical_collection.transitions}        
        new_new_canonical_sets
  in
  let start_items = cc_get_start_items cfg in
  let start_closure = cc_closure (LR1ItemSet.of_list start_items) cfg first_sets in
  let cc_with_duplicates = build_canonical_collection {canonical_sets = []; transitions = [];} [start_closure;] in
  (**our algorithm doesn't filter duplicate transitions, so filter them now*)
  {cc_with_duplicates with transitions = List.sort_uniq compare_cc_transitions cc_with_duplicates.transitions}
  