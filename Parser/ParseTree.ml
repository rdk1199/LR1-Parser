
type parse_tree_node =
  | Production of ContextFreeGrammar.production * parse_tree_node list
  | Leaf of Lexer.Tokenizer.token

let string_of_parse_tree_node node = 
  let rec offset_string_of_parse_tree_node node offset =
    match node with
    | Leaf token -> offset ^ token.token_type
    | Production (production, node_list) ->
      offset ^ production.from_symbol ^
      (
        List.fold_left (fun acc_str node ->
        acc_str ^ "\n" ^ (offset_string_of_parse_tree_node node ("  " ^ offset))
        ) "" node_list
      )
  in
  offset_string_of_parse_tree_node node ""


type parse_tree = {
  root : parse_tree_node
}

let string_of_parse_tree parse_tree =
  string_of_parse_tree_node parse_tree.root 


(**Helper : get n items off a list - not tail recursive *)
let rec first_n_of_list list n =
  if n <= 0 then
    []
  else
    match list with
    | [] -> []
    | head :: tail -> head :: (first_n_of_list tail (n-1))

(**Return parse tree or abort if invalid*)
let parse token_list parse_table =
  let open ContextFreeGrammar in
  let open ParseTables in
  (**The node_stack and state_stack are parse_tree_node and state lists, respectively, which we use as stacks*)
  let rec rec_parse curr_token_list node_stack state_stack =
    match curr_token_list with
    | [] -> assert(false) (*Shouldn't happen*)
    | token :: remaining_tokens ->
      let curr_word = token.Lexer.Tokenizer.token_type in
      match state_stack with
      | [] -> assert(false) (*Shouldn't happen*)
      | state :: remaining_stack ->
        match action_table_get parse_table.action_table state curr_word with
        | None -> assert(false) (*Parse error*)
        | Some action ->
          match action with
          | Accept production -> 
            let prod_length = List.length production.to_symbols in
            let child_symbols = List.rev (first_n_of_list node_stack prod_length) in
            {root = Production (production, child_symbols)} (**Successful parse*)
          | Shift s -> rec_parse remaining_tokens (Leaf token :: node_stack) (s :: state_stack)
          | Reduce production -> 
            let prod_length = List.length production.to_symbols in
            let child_symbols = List.rev (first_n_of_list node_stack prod_length) in
            let new_node_stack = List.drop prod_length node_stack in
            let new_state_stack = List.drop prod_length state_stack in
            let curr_state = List.hd new_state_stack in
            let new_reduced_node = Production (production, child_symbols) in
            let new_state_opt = goto_table_get parse_table.goto_table curr_state production.from_symbol in
            match new_state_opt with
            | None -> assert(false) (*Parse error*)
            | Some new_state -> rec_parse curr_token_list (new_reduced_node :: new_node_stack) (new_state :: new_state_stack)
  in
  rec_parse token_list [] [0]
