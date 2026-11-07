

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
