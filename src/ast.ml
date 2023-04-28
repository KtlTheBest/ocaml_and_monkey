type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Gt (* greater than*)
  | Lt (* less than*)
  | Ge (* greater or equal*)
  | Le (* Less or equal *)
  | And
  | Or
  | BAnd
  | BOr
  | XOr
  | ShiftL
  | ShiftR

type unrop = Neg | Not

type expr =
  | Unit
  | HashMap of (string * expr) list
  | HashMapAccess of expr * expr
  | List of expr list
  | Block of statements
  | Integer of int
  | Char of char
  | Bool of bool
  | Float of float
  | String of string
  | Identifier of string
  | BinOp of binop * expr * expr
  | UnrOp of unrop * expr
  | Conditional of expr * expr * expr
  | FuncCall of string * expr list
  | Lambda of string list * expr * (string * expr) list
  | Let of string * expr
  | Return of expr

and statements = expr list

let binop_to_string op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Gt -> ">"
  | Lt -> "<"
  | Ge -> ">="
  | Le -> "<="
  | And -> "&&"
  | Or -> "||"
  | BAnd -> "&"
  | BOr -> "|"
  | XOr -> "^"
  | ShiftL -> "<<"
  | ShiftR -> ">>"

let unrop_to_string c = match c with Neg -> "~" | Not -> "!"

let opname op =
  match op with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Gt -> "Gt"
  | Lt -> "Lt"
  | Ge -> "Ge"
  | Le -> "Le"
  | And -> "And"
  | Or -> "Or"
  | BAnd -> "BAnd"
  | BOr -> "BOr"
  | XOr -> "XOr"
  | ShiftL -> "ShiftL"
  | ShiftR -> "ShiftR"

let expr_type_to_string e =
  match e with
  | Unit -> "Unit"
  | HashMap _ -> "HashMap"
  | HashMapAccess _ -> "HashMapAccess"
  | List _ -> "List"
  | Block _ -> "Block"
  | Integer _ -> "Integer"
  | Char _ -> "Char"
  | Bool _ -> "Bool"
  | Float _ -> "Float"
  | String _ -> "String"
  | Identifier _ -> "Identifier"
  | BinOp _ -> "BinOp"
  | UnrOp _ -> "UnrOp"
  | Conditional _ -> "Conditional"
  | FuncCall _ -> "FuncCall"
  | Lambda _ -> "Lambda"
  | Let _ -> "Let"
  | Return _ -> "Return"

let rec expr_to_string tab e =
  match e with
  | Unit -> tab ^ "()"
  | HashMap hm -> hashmap_to_string tab hm
  | HashMapAccess (hm, field) -> hashmap_access_to_string tab (hm, field)
  | List l -> list_to_string tab l
  | Block b -> block_to_string tab b
  | Integer i -> tab ^ string_of_int i ^ "\n"
  | Char c -> tab ^ String.make 1 c ^ "\n"
  | Bool true -> tab ^ "true\n"
  | Bool false -> tab ^ "false\n"
  | Float f -> tab ^ string_of_float f ^ "\n"
  | String s -> tab ^ s ^ "\n"
  | Identifier id -> tab ^ "[identifier]\n" ^ "  " ^ tab ^ id ^ "\n"
  | BinOp (op, a, b) -> binexpr_to_string tab (op, a, b)
  | UnrOp (op, e) -> unrexpr_to_string tab (op, e)
  | Conditional (c, t, f) -> if_to_string tab (c, t, f)
  | FuncCall (name, args) -> funccall_to_string tab (name, args)
  | Lambda (args, stats, _) -> lambda_to_string tab (args, stats)
  | Let (name, e) -> let_to_string tab (name, e)
  | Return e -> return_to_string tab e

and hashmap_to_string tab hm =
  let new_tab = tab ^ "  " in
  let pref = tab ^ "[hashmap]\n" in
  let field (a, b) = "\"" ^ a ^ "\" : " ^ expr_to_string new_tab b in
  pref ^ (hm |> List.map field |> String.concat ("\n" ^ new_tab)) ^ "\n"

and hashmap_access_to_string tab (hm, field) =
  let new_tab = tab ^ "  " in
  let pref = tab ^ "[hashmap_access]\n" in
  let hm_as_string = expr_to_string new_tab hm ^ "\n" in
  let field_as_string = expr_to_string new_tab field in
  pref ^ hm_as_string ^ field_as_string ^ "\n"

and list_to_string tab l =
  let new_tab = tab ^ "  " in
  let pref = tab ^ "[list]\n" in
  let l_as_string = l |> List.map (expr_to_string new_tab) |> String.concat "\n" in
  pref ^ l_as_string ^ "\n"

and block_to_string tab b =
  let new_tab = "  " ^ tab in
  let pref = tab ^ "[block]\n" in
  let b_as_string = b |> List.map (expr_to_string new_tab) |> String.concat "\n" in
  pref ^ b_as_string ^ "\n"

and binexpr_to_string tab (op, a, b) =
  let new_tab = "  " ^ tab in
  let op_as_string = binop_to_string op in
  let pref = tab ^ "[" ^ op_as_string ^ "]\n" in
  let a_as_string = expr_to_string new_tab a in
  let b_as_string = expr_to_string new_tab b in
  pref ^ a_as_string ^ b_as_string

and unrexpr_to_string tab (op, e) =
  let op_as_string = unrop_to_string op in
  let pref = tab ^ "[" ^ op_as_string ^ "]\n" in
  let e_as_string = expr_to_string ("  " ^ tab) e in
  pref ^ e_as_string

and if_to_string tab (c, t, f) =
  let pref = tab ^ "[if]\n" in
  let new_tab = "  " ^ tab in
  let c_as_string = expr_to_string new_tab c in
  let t_as_string = expr_to_string new_tab t in
  let f_as_string = expr_to_string new_tab f in
  pref ^ c_as_string ^ t_as_string ^ f_as_string

and funccall_to_string tab (name, args) =
  let new_tab = "  " ^ tab in
  let pref = tab ^ "[funccall]\n" in
  let args_as_string = args |> List.map (expr_to_string new_tab) |> String.concat "\n" in
  pref ^ new_tab ^ name ^ "\n" ^ args_as_string

and lambda_to_string tab (args, e) =
  let new_tab = "  " ^ tab in
  let pref = tab ^ "[lambda]\n" in
  let args_to_string _tab l =
    let aug_tab = "  " ^ _tab in
    let pref = _tab ^ "( " in
    let suff = " )\n" in
    pref ^ String.concat ("\n" ^ aug_tab) l ^ suff
  in
  let e_as_string = expr_to_string new_tab e in
  pref ^ args_to_string new_tab args ^ e_as_string

and let_to_string tab (name, e) =
  let new_tab = "  " ^ tab in
  let pref = tab ^ "[let]\n" in
  let e_as_string = expr_to_string new_tab e in
  pref ^ (new_tab ^ name ^ "\n") ^ e_as_string

and return_to_string tab e =
  let pref = tab ^ "[return]\n" in
  let e_as_string = expr_to_string ("  " ^ tab) e in
  pref ^ e_as_string

and expr_list_to_string tab x =
  x |> List.map (expr_to_string tab) |> String.concat ("\n" ^ tab)

let string_from_expr e = expr_to_string "" e

let print_ast (ast : expr) : unit =
  let s = string_from_expr ast in
  print_string s