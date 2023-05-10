open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Value (value) -> value
  | ID (id) -> ref_lookup env id
  | Not (e) ->
    let expr = eval_expr env e in
    (match expr with
    | Bool (b) -> Bool(not b)
    | _ -> raise (TypeError "Not a boolean expression"))
  | Binop (op, e1, e2) ->
    (match op with
    | Add ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Int (x1 + x2)
      | _ -> raise (TypeError "Not integers"))
    | Sub ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Int (x1 - x2)
      | _ -> raise (TypeError "Not integers"))
    | Mult ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Int (x1 * x2)
      | _ -> raise (TypeError "Not integers"))
    | Div ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> if x2 <> 0 then Int (x1 / x2) else raise DivByZeroError
      | _ -> raise (TypeError "Not integers"))
    | Greater ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Bool (x1 > x2)
      | _ -> raise (TypeError "Not integers"))
    | Less ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Bool (x1 < x2)
      | _ -> raise (TypeError "Not integers"))
    | GreaterEqual ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Bool (x1 >= x2)
      | _ -> raise (TypeError "Not integers"))
    | LessEqual ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Bool (x1 <= x2)
      | _ -> raise (TypeError "Not integers"))
    | Concat ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (String (s1), String (s2)) -> String (s1 ^ s2)
      | _ -> raise (TypeError "Not strings"))
    | Equal ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Bool (x1 = x2)
      | _ -> raise (TypeError "Not integers"))
    | NotEqual ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Int (x1), Int (x2)) -> Bool (x1 <> x2)
      | _ -> raise (TypeError "Not integers"))
    | Or ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Bool (b1), Bool (b2)) -> Bool (b1 || b2)
      | _ -> raise (TypeError "Not boolean expressions"))
    | And ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (e1', e2') with
      | (Bool (b1), Bool (b2)) -> Bool (b1 && b2)
      | _ -> raise (TypeError "Not boolean expressions")))
  | If (guard, true_e, false_e) ->
    let guard' = eval_expr env guard in
    (match guard' with
    | Bool (b) -> if b then eval_expr env true_e else eval_expr env false_e
    | _ -> raise (TypeError "Not a boolean expression"))
  | Let (var, b, e1, e2) ->
    (match b with
    | true ->
      let env' = ref_extend_tmp env var in
      let v = eval_expr env' e1 in
      let _ = ref_update env' var v in
      eval_expr env' e2
    | false ->
      let e1' = eval_expr env e1 in
      let env' = ref_extend env var e1' in
      eval_expr env' e2)
  | Fun (var, e) -> Closure (env, var, e)
  | FunctionCall (e1, e2) ->
    let e1' = eval_expr env e1 in
    (match e1' with
    | Closure (a, x, e) ->
      let e2' = eval_expr env e2 in
      eval_expr (ref_extend a x e2') e
    | _ -> raise (TypeError "No Closure"))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  match m with
  | Def (var, expr) ->
    let env' = ref_extend_tmp env var in
    let v = eval_expr env' expr in
    let _ = ref_update env' var v in
    (env', Some(eval_expr env' expr))
  | Expr (expr) -> (env, Some (eval_expr env expr))
  | NoOp -> (env, None)
