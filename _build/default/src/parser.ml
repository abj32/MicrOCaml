open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks
  | Some Tok_Fun -> parse_fun toks
  | _ -> parse_or toks

and parse_let toks =
  match lookahead toks with
  | Some Tok_Let -> let t = match_token toks Tok_Let in
    (match lookahead t with
    | Some Tok_Rec -> let t' = match_token t Tok_Rec in
      (match lookahead t' with
      | Some Tok_ID (id) -> let t'' = match_many t' [Tok_ID id; Tok_Equal] in
        let (t''', e1) = parse_expr t'' in
        let (t'''', e2) = parse_expr (match_token t''' Tok_In) in
        (t'''', Let(id, true, e1, e2))
      | _ -> raise (InvalidInputException "Invalid"))
    | Some Tok_ID (id) -> let t' = match_many t [Tok_ID id; Tok_Equal] in
      let (t'', e1) = parse_expr t' in
      let (t''', e2) = parse_expr (match_token t'' Tok_In) in
      (t''', Let(id, false, e1, e2))
    | _ -> raise (InvalidInputException "Invalid"))
  | _ -> raise (InvalidInputException "Invalid")

and parse_if toks =
  match lookahead toks with
  | Some Tok_If -> let (t, e1) = parse_expr (match_token toks Tok_If) in
    let (t', e2) = parse_expr (match_token t Tok_Then) in
    let (t'', e3) = parse_expr (match_token t' Tok_Else) in
    (t'', If(e1, e2, e3))
  | _ -> raise (InvalidInputException "Invalid")

and parse_fun toks =
  match lookahead toks with
  | Some Tok_Fun ->
    (match lookahead (match_token toks Tok_Fun) with
    | Some Tok_ID (id) -> let t = match_many (match_token toks Tok_Fun) [Tok_ID id; Tok_Arrow] in
      let (t', e1) = parse_expr t in
      (t', Fun(id, e1))
    | _ -> raise (InvalidInputException "Invalid"))
  | _ -> raise (InvalidInputException "Invalid")

and parse_or toks = let (t, e1) = parse_and toks in
    match lookahead t with
    | Some Tok_Or -> let (t', e2) = parse_or (match_token t Tok_Or) in
    (t', Binop(Or, e1, e2))
    | _ -> (t, e1)

and parse_and toks = let (t, e1) = parse_equality toks in
    match lookahead t with
    | Some Tok_And -> let (t', e2) = parse_and (match_token t Tok_And) in
    (t', Binop(And, e1, e2))
    | _ -> (t, e1)

and parse_equality toks = let (t, e1) = parse_relational toks in
    match lookahead t with
    | Some Tok_Equal -> let (t', e2) = parse_equality (match_token t Tok_Equal) in
    (t', Binop(Equal, e1, e2))
    | Some Tok_NotEqual -> let (t', e2) = parse_equality (match_token t Tok_NotEqual) in
    (t', Binop(NotEqual, e1, e2))
    | _ -> (t, e1)

and parse_relational toks = let (t, e1) = parse_additive toks in
    match lookahead t with
    | Some Tok_Less -> let (t', e2) = parse_relational (match_token t Tok_Less) in
    (t', Binop(Less, e1, e2))
    | Some Tok_LessEqual -> let (t', e2) = parse_relational (match_token t Tok_LessEqual) in
    (t', Binop(LessEqual, e1, e2))
    | Some Tok_Greater -> let (t', e2) = parse_relational (match_token t Tok_Greater) in
    (t', Binop(Greater, e1, e2))
    | Some Tok_GreaterEqual -> let (t', e2) = parse_relational (match_token t Tok_GreaterEqual) in
    (t', Binop(GreaterEqual, e1, e2))
    | _ -> (t, e1)

and parse_additive toks = let (t, e1) = parse_multiplicative toks in
    match lookahead t with
    | Some Tok_Add -> let (t', e2) = parse_additive (match_token t Tok_Add) in
    (t', Binop(Add, e1, e2))
    | Some Tok_Sub -> let (t', e2) = parse_additive (match_token t Tok_Sub) in
    (t', Binop(Sub, e1, e2))
    | _ -> (t, e1)

and parse_multiplicative toks = let (t, e1) = parse_concat toks in
    match lookahead t with
    | Some Tok_Mult -> let (t', e2) = parse_multiplicative (match_token t Tok_Mult) in
    (t', Binop(Mult, e1, e2))
    | Some Tok_Div -> let (t', e2) = parse_multiplicative (match_token t Tok_Div) in
    (t', Binop(Div, e1, e2))
    | _ -> (t, e1)

and parse_concat toks = let (t, e1) = parse_unary toks in
    match lookahead t with
    | Some Tok_Concat -> let (t', e2) = parse_concat (match_token t Tok_Concat) in
    (t', Binop(Concat, e1, e2))
    | _ -> (t, e1)

and parse_unary toks =
  match lookahead toks with
  | Some Tok_Not -> let (t, e1) = parse_unary (match_token toks Tok_Not) in
  (t, Not(e1))
  | _ -> parse_function_call toks

and parse_function_call toks = let (t, e1) = parse_primary toks in
  match lookahead t with
  | Some Tok_Int (i) -> let (t', e2) = parse_primary t in
  (t', FunctionCall(e1, e2))
  | Some Tok_Bool (b) -> let (t', e2) = parse_primary t in
  (t', FunctionCall(e1, e2))
  | Some Tok_String (s) -> let (t', e2) = parse_primary t in
  (t', FunctionCall(e1, e2))
  | Some Tok_ID (id) -> let (t', e2) = parse_primary t in
  (t', FunctionCall(e1, e2))
  | Some Tok_LParen -> let (t', e2) = parse_primary t in
  (t', FunctionCall(e1, e2))
  | _ -> (t, e1)

and parse_primary toks =
  match lookahead toks with
  | Some Tok_Int (i) -> let t = match_token toks (Tok_Int i) in
  (t, Value(Int i))
  | Some Tok_Bool (b) -> let t = match_token toks (Tok_Bool b) in
  (t, Value(Bool b))
  | Some Tok_String (s) -> let t = match_token toks (Tok_String s) in
  (t, Value(String s))
  | Some Tok_ID (id) -> let t = match_token toks (Tok_ID id) in
  (t, ID id)
  | Some Tok_LParen -> let (t, e1) = parse_expr (match_token toks Tok_LParen) in
  ((match_token t Tok_RParen), e1)
  | _ -> raise (InvalidInputException "Invalid")

let rec parse_mutop toks =
  match lookahead toks with
  | Some Tok_Def -> let a = match_token toks Tok_Def in
    (match lookahead a with
    | Some Tok_ID (id) -> let t = match_many a [Tok_ID id; Tok_Equal] in
      let (t', e1) = parse_expr t in
      (match lookahead t' with
      | Some Tok_DoubleSemi -> ((match_token t' Tok_DoubleSemi), Def(id, e1))
      | _ -> raise (InvalidInputException "Invalid"))
    | _ -> raise (InvalidInputException "Invalid"))
  | Some Tok_DoubleSemi -> ((match_token toks Tok_DoubleSemi), NoOp)
  | _ -> let (t, e1) = parse_expr toks in
    (match lookahead t with
    | Some Tok_DoubleSemi -> ((match_token t Tok_DoubleSemi), Expr e1)
    | _ -> raise (InvalidInputException "Invalid"))