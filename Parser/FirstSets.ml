let first_set_process_production production first_set_map =
    let open ContextFreeGrammar in
    let from_symbol = production.from_symbol in
    let to_symbols = production.to_symbols in
    (*let () = print_endline (string_of_production production) in*)
    match to_symbols with
    | [] -> first_set_map (**Empty production? Shouldn't happen but OK*)
    | head :: tail -> 
      (**The find should always succeed since we add entries for all symbols at the beginning*)
      let start_set = SymbolMap.find head first_set_map in
      let epsilon_removed_set = SymbolSet.remove epsilon start_set in
      let gathered_set = 
      if not (SymbolSet.mem epsilon start_set) then
        start_set (**No epsilon, so only consider start set of the first symbol *)
      else
        let rec rec_process_production to_symbols_list_tail accumulated_first_set =
          match to_symbols_list_tail with
          | [] -> SymbolSet.add epsilon accumulated_first_set (**We processed the entire list, so add epsilon!*)
          | head :: tail ->
          let next_set = SymbolMap.find head first_set_map in
          let next_set_no_epsilon = SymbolSet.remove epsilon next_set in
          if SymbolSet.mem epsilon next_set then
            rec_process_production tail (SymbolSet.union accumulated_first_set next_set_no_epsilon)
          else
            SymbolSet.union accumulated_first_set next_set 
        in
        rec_process_production tail epsilon_removed_set
    in
    (*let () = print_endline ("gathered set: " ^ (string_of_symbol_set gathered_set)) in*)
    let final_set = SymbolSet.union (SymbolMap.find from_symbol first_set_map) gathered_set in
    (*let () = print_endline ("final set: " ^ (string_of_symbol_set) final_set) in *)
    let out_map = SymbolMap.add from_symbol final_set first_set_map in
    (*let () = print_endline ("returned map: " ^ (string_of_symbol_to_set_map out_map )) in*)
    out_map


let update_first_sets cfg first_sets_map =
  let open ContextFreeGrammar in
  ProductionMap.fold(fun symbol production_list acc_map ->
    List.fold_left (fun inner_acc_map production ->
      first_set_process_production production inner_acc_map
    ) acc_map production_list
  ) cfg.productions first_sets_map

let compute_first_sets cfg = 
  let open ContextFreeGrammar in
  let first_set_map = SymbolMap.empty in
  let terminal_map = SymbolSet.fold (fun terminal acc_map ->
    (**Each terminal's first set is just itself*)
    SymbolMap.add terminal (SymbolSet.add terminal SymbolSet.empty) acc_map
  ) cfg.terminals first_set_map
  in

  let start_map = SymbolSet.fold (fun nonterminal acc_map ->
    (**Each nonterminal maps to the empty set to start*)
    SymbolMap.add nonterminal SymbolSet.empty acc_map
  ) cfg.nonterminals terminal_map
  in

  (**Iteratively add to first sets until all of them stop growing*)
  let rec accumulate_first_sets prev_first_sets first_sets =
    (*let () = print_endline "iteration" in
    let () = print_endline (string_of_symbol_to_set_map first_sets) in*)
    if symbol_to_set_maps_equal prev_first_sets first_sets then
      (**Our last run did not do anything new - we are done*)
      first_sets
    else
     let new_first_sets = update_first_sets cfg first_sets in
     accumulate_first_sets first_sets new_first_sets 
  in
  accumulate_first_sets SymbolMap.empty start_map

(**Compute the first set of a string of symbols, given the first set map of the CFG*)
let compute_first_set_of_symbol_list symbol_list first_sets =
  let open ContextFreeGrammar in
  let rec compute_first_sets_of_symbol_list_rec accum_set rem_symbols = 
    match rem_symbols with
    (**We have reached the end of all the symbols, meaning they must have all contained epsilon, so the
      string must also have epsilon as a possible first symbol*)
    | [] -> SymbolSet.add epsilon accum_set
    | symbol :: tail -> 
      match SymbolMap.find_opt symbol first_sets with
      | None -> accum_set
      | Some first_set ->
        if not (SymbolSet.mem epsilon first_set) then
          SymbolSet.union accum_set first_set
        else
          let first_set_no_epsilon = SymbolSet.remove epsilon first_set in
          compute_first_sets_of_symbol_list_rec (SymbolSet.union first_set_no_epsilon accum_set) tail
  in
  compute_first_sets_of_symbol_list_rec SymbolSet.empty symbol_list

