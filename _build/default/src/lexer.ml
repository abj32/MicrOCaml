open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input =
  let rec tokenize_aux i = if i >= String.length input then [] else
    match input.[i] with
    | ' ' | '\t' | '\n' -> tokenize_aux (i + 1)
    | '(' -> Tok_LParen :: tokenize_aux (i + 1)
    | ')' -> Tok_RParen :: tokenize_aux (i + 1)
    | '=' -> Tok_Equal :: tokenize_aux (i + 1)
    | '<' ->
      if i + 1 < String.length input then
        match input.[i + 1] with
        | '>' -> Tok_NotEqual :: tokenize_aux (i + 2)
        | '=' -> Tok_LessEqual :: tokenize_aux (i + 2)
        | '_' -> Tok_Less :: tokenize_aux (i + 1)
    | '>' ->
      if i + 1 < String.length input && input.[i + 1] = '=' then Tok_GreaterEqual :: tokenize_aux (i + 2)
      else Tok_Greater :: tokenize_aux (i + 1)
    | '|' ->
      if i + 1 < String.length input && input.[i + 1] = '|' then Tok_Or :: tokenize_aux (i + 2)
      else raise (InvalidInputException "Invalid")
    | '&' ->
      if i + 1 < String.length input && input.[i + 1] = '&' then Tok_Or :: tokenize_aux (i + 2)
      else raise (InvalidInputException "Invalid")
    | '+' -> Tok_Add :: tokenize_aux (i + 1)
    | '-' -> 
      if i + 1 < String.length input && input.[i + 1] = '>' then Tok_Arrow :: tokenize_aux (i + 2)
      else Tok_Sub :: tokenize_aux (i + 1)
    | '*' -> Tok_Mult :: tokenize_aux (i + 1)
    | '/' -> Tok_Div :: tokenize_aux (i + 1)
    | '^' -> Tok_Concat :: tokenize_aux (i + 1)
    | ';' ->
      if i + 1 < String.length input && input.[i + 1] = ';' then Tok_DoubleSemi :: tokenize_aux (i + 2)
      else raise (InvalidInputException "Invalid")
    | 'a'..'z' | 'A'..'Z' ->
      let tokenize_aux_2 j = if j >= String.length input then [Tok_ID (String.sub input i (j-i))] else
        match input.[j] with
        | 'n' ->
          if j + 2 < String.length input && input.[j + 1] = 'o' && input.[j + 2] = 't' then Tok_Not :: tokenize_aux (j + 3)
          else tokenize_aux_2 (j + 1)
        | 'i' ->
          if j + 1 < String.length input && input.[j + 1] = 'f' then Tok_If :: tokenize_aux (j + 2)
          else if j + 1 < String.length input && input.[j + 1] = 'n' then Tok_In :: tokenize_aux (j + 2)
          else tokenize_aux_2 (j + 1)
        | 't' ->
          if j + 3 < String.length input && input.[j + 1] = 'h' && input.[j + 2] = 'e' && input.[j + 3] = 'n' then Tok_Then :: tokenize_aux (j + 4)
          else if j + 3 < String.length input && input.[j + 1] = 'r' && input.[j + 2] = 'u' && input.[j + 3] = 'e' then Tok_Bool true :: tokenize_aux (j + 4)
          else tokenize_aux_2 (j + 1)
        | 'e' ->
          if j + 3 < String.length input && input.[j + 1] = 'l' && input.[j + 2] = 's' && input.[j + 3] = 'e' then Tok_Else :: tokenize_aux (j + 4)
          else tokenize_aux_2 (j + 1)
        | 'l' ->
          if j + 2 < String.length input && input.[j + 1] = 'e' && input.[j + 2] = 't' then Tok_Let :: tokenize_aux (j + 3)
          else tokenize_aux_2 (j + 1)
        | 'd' ->
          if j + 2 < String.length input && input.[j + 1] = 'e' && input.[j + 2] = 'f' then Tok_Def :: tokenize_aux (j + 3)
          else tokenize_aux_2 (j + 1)
        | 'r' ->
          if j + 2 < String.length input && input.[j + 1] = 'e' && input.[j + 2] = 'c' then Tok_Rec :: tokenize_aux (j + 3)
          else tokenize_aux_2 (j + 1)
        | 'f' ->
          if j + 2 < String.length input && input.[j + 1] = "u" && input.[j + 2] = 'n' then Tok_Fun :: tokenize_aux (j + 3)
          else if j + 4 < String.length input && input.[j + 1] = "a" && input.[j + 2] = 'l' && input.[j + 3] = 's' && input.[j + 4] = 'e' then Tok_Bool false :: tokenize_aux (j + 5)
          else tokenize_aux_2 (j + 1)
        | 'a'..'z' | 'A'..'Z' | '0'..'9' -> tokenize_aux_2 (j + 1)
        | _ -> Tok_ID (String.sub input i (j - i)) :: tokenize_aux j
      in tokenize_aux_2 if
    | '0'..'9' ->
      let rec tokenize_aux_2 j = if j >= String.length input then [Tok_Int (int_of_string (String.sub input i (j - i))] else
        match input.[j] with
        | '0'..'9' -> tokenize_aux_2 (j + 1)
        | _ -> Tok_Int (int_of_string (String.sub input i (j - i))) :: tokenize_aux j
      in tokenize_aux_2 i
    | _ -> raise (InvalidInputException "Invalid")
  in tokenize 0