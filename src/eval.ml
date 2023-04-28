open Ast
open Util

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

let ( % ) = modulo

let update_ctx ctx (name, v) =
  let is_in ctx name =
    let rec walk l n =
      match l with
      | [] -> false
      | (key, value) :: rest -> if key = n then true else walk rest n
    in
    walk ctx name
  in
  let _update_ctx l (key, value) =
    let rec walk l k =
      match l with
      | [] -> []
      | (_k, _v) :: rest ->
          if _k = k then (_k, value) :: rest else (_k, _v) :: walk rest k
    in
    walk l key
  in
  if is_in ctx name then _update_ctx ctx (name, v) else (name, v) :: ctx

let merge a b =
  let rec update x y =
    match x with
    | [] -> y
    | v :: rest ->
        let new_y = update rest y in
        update_ctx new_y v
  in
  update a b

exception RunTimeError of string

let raise_runtime msg = raise (RunTimeError msg)

let raise_runtime1 msg a =
  raise (RunTimeError (msg ^ Printf.sprintf ": %s" (expr_type_to_string a)))

let raise_runtime2 msg a b =
  raise
    (RunTimeError
       (msg ^ Printf.sprintf ": %s and %s" (expr_type_to_string a) (expr_type_to_string b)))

let instantiate_args ctx names values =
  if List.length names <> List.length values then
    raise_runtime
      "The number of the arguments provided is not equal to those that required"
  else
    let zipped = List.combine names values in
    merge zipped ctx

let bool_eval b = if b then Bool true else Bool false

let rec lookup_identifier ctx name =
  match ctx with
  | [] -> None
  | (k, v) :: _ when k = name -> Some v
  | _ :: rest -> lookup_identifier rest name

let rec eval ctx e =
  match e with
  | Unit | Integer _ | Char _ | Bool _ | Float _ | String _ -> (ctx, e)
  | Identifier name -> (ctx, name |> lookup_identifier ctx |> eval ctx |> snd)
  | HashMap hm -> (ctx, HashMap (eval_hashmap ctx hm))
  | HashMapAccess (hm, e) -> (ctx, access_hashmap ctx (hm, e))
  | List l -> (ctx, eval_list ctx l)
  | Block b -> (ctx, eval_block ctx b)
  | BinOp (op, a, b) -> (ctx, eval_binop ctx (op, a, b))
  | UnrOp (op, e) -> (ctx, eval_unrop ctx (op, e))
  | Conditional (c, t, f) -> (ctx, eval_conditional ctx (c, t, f))
  | FuncCall (name, args) -> (ctx, eval_funccall ctx (name, args))
  | Lambda (args, body, closures) -> (ctx, Lambda (args, body, merge ctx closures))
  | Let (name, e) ->
      let _e = snd (eval ctx e) in
      let new_ctx = update_ctx ctx (name, _e) in
      (new_ctx, e)
  | Return e ->
      let new_ctx, _e = eval ctx e in
      (new_ctx, _e)

and lookup_identifier ctx name =
  let rec try_walk ctx name =
    match ctx with
    | [] -> Unit
    | (k, v) :: _ when k = name -> v
    | _ :: rest -> try_walk rest name
  in
  try_walk ctx name

and eval_hashmap ctx hm =
  let rec try_eval ctx _hm =
    (* TODO: make tail-recursive *)
    match _hm with [] -> [] | (k, v) :: rest -> try_eval ctx rest
  in
  try_eval ctx hm

and access_hashmap ctx (hm, e) =
  let new_ctx, e_hm = eval ctx hm in
  let new_ctx, _e = eval new_ctx e in
  match (e_hm, _e) with
  | HashMap hm, String s -> lookup_identifier hm s
  | _ -> raise_runtime2 "Can't perform the hashmap access on the following types" e_hm _e

and eval_list ctx l =
  let res = List.map (fun x -> snd (eval ctx x)) l in
  List res

and eval_block ctx b = eval_exprs ctx b

and eval_binop ctx (op, a, b) =
  let ctx_1, e_a = eval ctx a in
  let _, e_b = eval ctx b in
  match op with
  | Add -> eval_add (e_a, e_b)
  | Sub -> eval_sub (e_a, e_b)
  | Mul -> eval_mul (e_a, e_b)
  | Div -> eval_mul (e_a, e_b)
  | Mod -> eval_mod (e_a, e_b)
  | Eq -> eval_eq (e_a, e_b)
  | Neq -> eval_neq (e_a, e_b)
  | Gt -> eval_gt (e_a, e_b)
  | Ge -> eval_ge (e_a, e_b)
  | Lt -> eval_lt (e_a, e_b)
  | Le -> eval_le (e_a, e_b)
  | And -> eval_and (e_a, e_b)
  | Or -> eval_or (e_a, e_b)
  | BAnd -> eval_band (e_a, e_b)
  | BOr -> eval_bor (e_a, e_b)
  | XOr -> eval_xor (e_a, e_b)
  | ShiftL -> eval_shiftl (e_a, e_b)
  | ShiftR -> eval_shiftr (e_a, e_b)

and eval_add (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia + ib)
  | Float ia, Float ib -> Float (ia +. ib)
  | _, _ -> raise_runtime2 "The following types are not supported by '+'" a b

and eval_sub (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia - ib)
  | Float ia, Float ib -> Float (ia -. ib)
  | _, _ -> raise_runtime2 "The following types are not supported by '-'" a b

and eval_mul (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia * ib)
  | Float ia, Float ib -> Float (ia *. ib)
  | _, _ -> raise_runtime2 "The following types are not supported by '*'" a b

and eval_div (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia / ib)
  | Float ia, Float ib -> Float (ia /. ib)
  | _, _ -> raise_runtime2 "The following types are not supported by '/'" a b

and eval_mod (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia % ib)
  | _, _ -> raise_runtime2 "The following types are not supported by '%'" a b

and eval_eq (a, b) =
  match (a, b) with
  | Unit, Unit -> Bool true
  | Bool ba, Bool bb -> bool_eval (ba = bb)
  | Integer ia, Integer ib -> bool_eval (ia = ib)
  | Float fa, Float fb -> bool_eval (fa -. fb < 0.00001)
  | Char ca, Char cb -> bool_eval (ca = cb)
  | String sa, String sb -> bool_eval (sa = sb)
  | HashMap hma, HashMap hmb -> eval_eq_hashmap (hma, hmb)
  | List la, List lb -> eval_eq_list (la, lb)
  | _, _ ->
      let a_type = expr_type_to_string a in
      let b_type = expr_type_to_string b in
      failwith (Printf.sprintf "Comparing %s and %s: impossible case" a_type b_type)

and eval_neq (a, b) =
  match eval_eq (a, b) with
  | Bool b -> Bool (Bool.not b)
  | _ -> failwith "Got something else besides bool from eval_eq: impossible case"

and eval_eq_hashmap (a, b) =
  let ln_a = List.length a in
  let ln_b = List.length b in
  if ln_a != ln_b then Bool false
  else
    let comp (k1, v1) (k2, v2) = if k1 = k2 then 0 else if k1 < k2 then 1 else -1 in
    let rec keys_are_eq la lb =
      let keys_a = List.map fst la in
      let keys_b = List.map fst lb in
      let rec inner _la _lb =
        match (_la, _lb) with
        | [], [] -> true
        | a :: a_rest, b :: b_rest -> if a != b then false else inner a_rest b_rest
        | _, _ -> failwith "keys_are_eq: impossible case"
      in
      inner keys_a keys_b
    in
    let rec values_are_eq la lb =
      let values_a = List.map snd la in
      let values_b = List.map snd lb in
      let rec inner _la _lb =
        match (_la, _lb) with
        | [], [] -> Bool true
        | _a :: _, _b :: _ when eval_eq (_a, _b) = Bool false -> Bool false
        | _ :: _a, _ :: _b -> inner _a _b
        | _, _ -> failwith "values_are_eq (eval_eq_hashmap): impossible case"
      in
      inner values_a values_b
    in
    let t_a = List.sort comp a in
    let t_b = List.sort comp b in
    if keys_are_eq t_a t_b = false then Bool false else values_are_eq t_a t_b

and eval_eq_list (a, b) =
  match (a, b) with
  | [], [] -> Bool true
  | _a :: a_rest, _b :: b_rest -> (
      match eval_eq (_a, _b) with
      | Bool false -> Bool false
      | Bool true -> eval_eq_list (a_rest, b_rest)
      | _ -> failwith "eval_list_eq: impossible case 1")
  | _, _ -> failwith "eval_list_eq: impossible_case 2"

and eval_gt (a, b) =
  match (a, b) with
  | Unit, Unit -> Bool false
  | Integer ia, Integer ib -> bool_eval (ia > ib)
  | Float fa, Float fb -> bool_eval (fa > fb)
  | Bool _, Bool _ -> raise_runtime2 "Can't compare booleans with '>'" a b
  | Char ca, Char cb -> bool_eval (ca > cb)
  | String sa, String sb -> bool_eval (sa > sb)
  | _a, _b -> raise_runtime2 "Can't compare those with '>'" _a _b

and eval_ge (a, b) =
  match eval_eq (a, b) with
  | Bool true -> Bool true
  | Bool false -> (
      match eval_gt (a, b) with
      | Bool true -> Bool true
      | Bool false -> Bool false
      | _ -> failwith "eval_ge: Should be impossible case 1")
  | _ -> failwith "eval_ge: Should be impossible case 2"

and eval_lt (a, b) =
  match eval_ge (a, b) with
  | Bool true -> Bool false
  | Bool false -> Bool true
  | _ -> failwith "eval_lt: Should be impossible case"

and eval_le (a, b) =
  match eval_gt (a, b) with
  | Bool true -> Bool false
  | Bool false -> Bool true
  | _ -> failwith "eval_le: Should be impossible case"

and eval_and (a, b) =
  match (a, b) with
  | Bool true, Bool true -> Bool true
  | Bool _, Bool _ -> Bool false
  | _ -> failwith "eval_and: Should be impossible case"

and eval_or (a, b) =
  match (a, b) with
  | Bool false, Bool false -> Bool false
  | Bool _, Bool _ -> Bool true
  | _ -> failwith "eval_or: Should be impossible case"

and eval_band (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia land ib)
  | _ -> raise_runtime2 "The following types are not supported for '&'" a b

and eval_bor (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia lor ib)
  | _ -> raise_runtime2 "The following types are not supported for '|'" a b

and eval_xor (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia lxor ib)
  | _ -> raise_runtime2 "The following types are not supported for '^'" a b

and eval_shiftl (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia lsr ib)
  | _ -> raise_runtime2 "The following types are not supported for '>>'" a b

and eval_shiftr (a, b) =
  match (a, b) with
  | Integer ia, Integer ib -> Integer (ia lsl ib)
  | _ -> raise_runtime2 "The following types are not supported for '<<'" a b

and eval_unrop ctx (op, e) =
  let new_ctx, _e = eval ctx e in
  match op with Neg -> eval_neg _e | Not -> eval_not _e

and eval_neg e =
  match e with
  | Integer i -> Integer ~-i
  | Float f -> Float ~-.f
  | _ -> raise_runtime1 "The following type is not supported for '~'" e

and eval_not e =
  match e with
  | Bool true -> Bool false
  | Bool false -> Bool true
  | _ -> raise_runtime1 "The following type is not supported for '!'" e

and eval_conditional ctx (c, t, f) =
  let ctx, c_e = eval ctx c in
  match c_e with
  | Bool true -> snd (eval ctx t)
  | Bool false -> snd (eval ctx f)
  | _ -> raise_runtime1 "When evaluating the conditional, expected Bool, but got" c_e

and eval_funccall ctx (name, args) =
  if is_keyword name then eval_builtin_func ctx (name, args)
  else
    let func = lookup_identifier ctx name in
    match func with
    | Lambda (placeholders, body, closure) ->
        let new_args = List.map (fun x -> snd (eval ctx x)) args in
        let new_ctx = merge ctx closure in
        let new_ctx = instantiate_args new_ctx placeholders new_args in
        snd (eval new_ctx body)
    | _ -> raise_runtime1 "Can only call functions, but instead got this" func

and is_keyword s = s = "print"

and eval_builtin_func ctx (name, args) =
  if name = "print" then eval_print ctx args
  else failwith ("eval_builtin_func: Impossible case: " ^ name)

and eval_print ctx args =
  let rec expr_value_to_string e =
    match e with
    | Unit -> "()"
    | Bool true -> "true"
    | Bool false -> "false"
    | Char c -> String.make 1 c
    | Integer i -> string_of_int i
    | Float f -> string_of_float f
    | String s -> s
    | List l ->
        let pref = "[" in
        let suff = "]" in
        let payload = l |> List.map expr_value_to_string |> String.concat ", " in
        pref ^ payload ^ suff
    | HashMap hm ->
        let pref = "{" in
        let suff = "}" in
        let payload =
          hm
          |> List.map (fun (a, b) -> "\"" ^ a ^ "\":" ^ expr_value_to_string b)
          |> String.concat ", "
        in
        pref ^ payload ^ suff
    | Block _ | HashMapAccess _ | FuncCall _ | Conditional _ | BinOp _ | UnrOp _
    | Identifier _ ->
        e |> eval ctx |> snd |> expr_value_to_string
    | Lambda _ -> "<lambda>"
    | Let _ -> "<let>"
    | Return _ -> "<return>"
  in
  let rec print_args a =
    match a with
    | [] -> Unit
    | v :: rest ->
        let e = snd (eval ctx v) in
        print_endline (expr_value_to_string e);
        print_args rest
  in
  print_args args

and eval_exprs ctx l =
  let rec walk _l (ctx, last_value) =
    match _l with
    | [] -> last_value
    | Return r :: _ -> r
    | e :: rest -> walk rest (eval ctx e)
  in
  walk l (ctx, Unit)

let evaluate l = eval_exprs [] l