(** Read a CFG in from a file in Backus Naur form*)

(** First line should be the start nonterminal*)
(** Remaining lines one of the forms*)
(** lhs ::= rhs1 rhs2 rhs3 *) (*<- this creates a new current nonterminal*) 
(** | rhs1 rhs2 rhs*) (*<- this uses the old nonterminal*)
let build_production_from_line line curr_nonterminal =
  let trimmed_line = String.trim line in
  if String.length trimmed_line = 0 then
    (**invalid line - return empty production*)
    ([], curr_nonterminal)
  else if trimmed_line.[0] = '|' then
    (**reusing existing nonterminal*)
    let () = assert(String.length curr_nonterminal > 0) in
    let rem_string = String.sub trimmed_line 1 ((String.length trimmed_line) - 1) in
    let rhs_words = Str.split (Str.regexp "[ \t\r\n]+") rem_string in
    (curr_nonterminal :: rhs_words, curr_nonterminal)
  else 
    (**new nonterminal*)
    let split_string = Str.split (Str.regexp "[ \t\r\n]+") trimmed_line in
    (**Should have at least lhs ::= rhs1*)
    let () = assert(List.length split_string > 2) in
    let new_nt = List.hd split_string in
    let headless_list = List.tl split_string in
    let () = assert(List.hd headless_list = "::=") in
    let rhs_list = List.tl headless_list in
    (new_nt :: rhs_list, new_nt)


let read_cfg_from_file file_name = 
  let (final_start_symbol, final_prod_list) = 
  In_channel.with_open_text file_name (
    fun ic ->
      let opt_first_line = In_channel.input_line ic in
      let start_symbol =
        match opt_first_line with
        | None -> assert(false)
        | Some line ->  String.trim line
      in
      let rec parse_lines production_list curr_nonterminal =
        match In_channel.input_line ic with
        | Some line ->
          let new_production, new_curr_nt = build_production_from_line line curr_nonterminal in
          parse_lines (new_production :: production_list) new_curr_nt
        | None -> (production_list, curr_nonterminal)
      in
      let production_list, _ = parse_lines [] "" in
      (start_symbol, production_list)
  )
  in
  let () = print_endline ("Successfully read CFG from " ^ file_name) in
  Parser.ContextFreeGrammar.build_cfg final_start_symbol final_prod_list
