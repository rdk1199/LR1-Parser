

let write_action_table_to_file file_name action_table =
  let open Parser.ParseTables in
  let file_name_ext = file_name ^ ".action" in
  Out_channel.with_open_text file_name_ext (fun oc ->
    let () = output_string oc ((string_of_int (Iarray.length action_table.entries)) ^ "\n") in
    Iarray.iteri (fun index action_map ->
      Parser.ContextFreeGrammar.SymbolMap.iter (fun symbol action ->
          let action_string =
            match action with
            | Shift (s, _) -> 
              (**The LR1 item associated with the shift is only useful for debugging during table construction,
              so don't write it to file*)
              "Shift " ^ (string_of_int s)
            | Reduce prod -> "Reduce " ^ Parser.ContextFreeGrammar.string_of_production prod
            | Accept prod -> "Accept " ^ Parser.ContextFreeGrammar.string_of_production prod
          in
          let line = (string_of_int index) ^ " " ^ symbol ^ " " ^ action_string ^ "\n" in
          output_string oc line
      ) action_map
    ) action_table.entries;
    flush oc;
    close_out oc
  )

let production_from_string_list string_list = 
  (**our format is lhs ::= rhs0, rhs1, ...*)
  (**So all we need to do is extract the ::= *)
  match string_list with
    | lhs :: "::=" :: rhs ->
      {
        Parser.ContextFreeGrammar.from_symbol = lhs;
        to_symbols = rhs;
      }
    | _ ->
      let list_string = List.fold_left (fun acc_str str ->
        acc_str ^ " " ^ str  
      ) "" string_list
      in
      failwith ("Invalid production string: " ^ list_string)
      

let process_action_table_line line =
  let tokens = Str.split (Str.regexp "[ \t\r\n]+") line in
  let () = assert (List.length tokens > 2) in
  match tokens with
  | state_str :: symbol :: action_str :: rem_tokens ->
    let state = int_of_string state_str in
    let action =
    match action_str with
    | "Accept" -> Parser.ParseTables.Accept (production_from_string_list rem_tokens)
    | "Shift" ->  
      (**Storing the LR1 item with the shift is only useful for debugging table construction, so don't read it*)
      Parser.ParseTables.Shift (int_of_string (List.hd rem_tokens), Parser.CanonicalCollection.empty_lr1_item)
    | "Reduce" -> Parser.ParseTables.Reduce (production_from_string_list rem_tokens)
    | s -> failwith ("Invalid action " ^ s ^ " in action table\n") (**invalid*)
    in
    (state, symbol, action)
  | _ -> failwith ("Action table line " ^ line ^ " is invalid!")

let read_action_table_from_file file_name =
  let open Parser.ContextFreeGrammar in
  let final_map_array = 
  In_channel.with_open_text file_name (
    fun ic ->
      let opt_first_line = In_channel.input_line ic in
      let num_states = 
        match opt_first_line with
        | None -> failwith "Action table is empty\n"
        | Some line -> int_of_string (String.trim line)
      in
      let map_array = Array.make num_states SymbolMap.empty in
      let rec parse_lines () =
        match In_channel.input_line ic with
        | Some line ->
          let (state, symbol, action) = process_action_table_line line in
          let () = Array.set map_array state (SymbolMap.add symbol action map_array.(state)) in
          parse_lines ()
        | None -> map_array
      in
      parse_lines ()
  )
  in
  Parser.ParseTables.{entries = Iarray.of_array final_map_array}

let process_goto_table_line line =
  let tokens = Str.split (Str.regexp "[ \t\r\n]+") line in
  let () = assert ((List.length tokens) > 2) in
  let state = int_of_string (List.hd tokens) in
  let symbol = List.hd (List.tl tokens) in
  let state_token = List.hd (List.tl (List.tl tokens)) in
  let to_state = int_of_string (state_token) in
  (state, symbol, to_state)

let read_goto_table_from_file file_name =
  let open Parser.ContextFreeGrammar in
  let final_map_array = 
  In_channel.with_open_text file_name (
    fun ic ->
      let opt_first_line = In_channel.input_line ic in
      let num_states = 
        match opt_first_line with
        | None -> failwith ("goto table " ^ file_name ^ " is empty\n")
        | Some line -> int_of_string (String.trim line)
      in
      let map_array = Array.make num_states SymbolMap.empty in
      let rec parse_lines () =
        match In_channel.input_line ic with
        | Some line ->
          let (state, symbol, to_state) = process_goto_table_line line in
          let () = Array.set map_array state (SymbolMap.add symbol to_state map_array.(state)) in
          parse_lines ()
        | None -> map_array
      in
      parse_lines ()
  )
  in
  Parser.ParseTables.{entries = Iarray.of_array final_map_array}


let write_goto_table_to_file file_name goto_table =
  let open Parser.ParseTables in
  let file_name_ext = file_name ^ ".goto" in
  Out_channel.with_open_text file_name_ext (fun oc ->
    let () = output_string oc ((string_of_int (Iarray.length goto_table.entries)) ^ "\n") in
    Iarray.iteri (fun index goto_map ->
      Parser.ContextFreeGrammar.SymbolMap.iter (fun symbol to_state ->
          let line = (string_of_int index) ^ " " ^ symbol ^ " " ^ (string_of_int to_state) ^ "\n" in
          output_string oc line
      ) goto_map
    ) goto_table.entries;
    flush oc;
    close_out oc
  )

let read_parse_table_from_files action_file goto_file =
  Parser.ParseTables.{action_table = (read_action_table_from_file action_file); goto_table = (read_goto_table_from_file goto_file)}