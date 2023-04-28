%{

open Ast

%}

%token<bool> BOOL_LIT
%token<char> CHAR_LIT
%token<int> INTEGER_LIT
%token<float> FLOAT_LIT
%token<string> STRING_LIT
%token<string> IDENTIFIER

%token LET "let"
%token FN "fn"
%token IF "if"
%token ELSE "else"
%token ELIF "elif"
%token UNIT "unit"
%token RETURN "return"

%token LPAR "("
%token RPAR ")" 
%token LCUR "{"
%token RCUR "}"
%token LSQR "["
%token RSQR "]"
%token COLON ":"
%token SEMICOLON ";"
%token COMMA ","

%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token MOD "%"
%token ASSIGN "="
%token EQ "=="
%token NEQ "!="
%token LT "<"
%token LE "<="
%token GT ">"
%token GE ">="
%token AND "&&"
%token OR "||"
%token B_AND "&"
%token B_OR "|"
%token XOR "^"
%token SHIFTL "<<"
%token SHIFTR ">>"
%token NOT "!"
%token NEG "~"
%token QUOT "'"
%token DOUBLEQUOT "\""

%token SPACE
%token COMMENT

%token EOF

%token AFTER_ELSE

%nonassoc AFTER_ELSE /* https://github.com/jhjourdan/C11parser/blob/master/parser.mly */
%nonassoc ELSE

%type <expr> value
%type <expr> block
%type <expr list> expr_list
%type <expr> expr
%type <expr> val_expr
%type <expr> func_def
%type <string list> args_def_list
%type <string list> ne_args_def_list
%type <expr> base_expr
%type <expr> if_expr
%type <expr> disjunc_expr
%type <expr> conjunc_expr
%type <expr> comparison_expr
%type <expr> shift_expr
%type <expr> arithmetic_expr
%type <expr> multiply_expr
%type <expr> literal_expr
%type <expr> function_call
%type <expr list> args_list
%type <expr list> ne_args_list
%type <(string * expr) list> hashmap
%type <(string * expr) list> fields_list
%type <string * expr> hash_map_field
%type <expr list> array_list
%type <expr list> array_list_contents

%start <expr option> prog
%%

prog:
  | EOF { None }
  | v = value { Some v }
  ;

value:
  | l = block { l }
  ;

block:
  | l = expr_list { Block( l ) }

expr_list:
  | { [] }
  | EOF { [] }
  | e = expr rest = expr_list { e :: rest }

expr:
  | "let" id = IDENTIFIER "=" ve = val_expr { Let( id, ve ) }
  | "return" e = val_expr { Return( e ) }
  | ve = val_expr { ve }
  ;

val_expr:
  | fd = func_def { fd }
  | be = base_expr { be }
  ;

func_def:
  | FN "(" al = args_def_list ")" el = val_expr 
  { Lambda( al, el, [] ) }
  ;

args_def_list:
  | { [] }
  | l = ne_args_def_list { l }
  ;

ne_args_def_list:
  | id = IDENTIFIER { [id] }
  | id = IDENTIFIER "," rest = ne_args_def_list { id :: rest }
  ;

base_expr:
  | ife = if_expr { ife }
  | be = disjunc_expr { be }
  ;

if_expr:
  | "if" "(" ve = val_expr ")" "{" tr = block "}" "else" "{" fl = block "}" 
  { Conditional( ve, tr, fl ) }
  | "if" "(" ve = val_expr ")" "{" tr = block "}" "elif" oi = if_expr
  { Conditional( ve, tr, oi ) }
  | "if" "(" ve = val_expr ")" "{" tr = block "}" %prec AFTER_ELSE 
  { Conditional( ve, tr, Unit)}
  ;

disjunc_expr:
  | e1 = disjunc_expr "||" e2 = conjunc_expr { BinOp ( Or, e1, e2 ) }
  | e = conjunc_expr { e }
  ;

conjunc_expr:
  | e1 = conjunc_expr "&&" e2 = comparison_expr { BinOp ( And, e1, e2 ) }
  | e = comparison_expr { e }
  ;

comparison_expr:
  | e1 = shift_expr "==" e2 = shift_expr { BinOp ( Eq, e1, e2 ) }
  | e1 = shift_expr "!=" e2 = shift_expr { BinOp ( Neq, e1, e2 ) }
  | e1 = shift_expr "<" e2 = shift_expr { BinOp ( Lt, e1, e2 ) }
  | e1 = shift_expr "<=" e2 = shift_expr { BinOp ( Le, e1, e2 ) }
  | e1 = shift_expr ">" e2 = shift_expr { BinOp ( Gt, e1, e2 ) }
  | e1 = shift_expr ">=" e2 = shift_expr { BinOp ( Ge, e1, e2 ) }
  | e = shift_expr { e }
  ;

shift_expr:
  | e1 = arithmetic_expr "<<" e2 = arithmetic_expr { BinOp ( ShiftL, e1, e2 ) }
  | e1 = arithmetic_expr ">>" e2 = arithmetic_expr { BinOp ( ShiftR, e1, e2 ) }
  | e = arithmetic_expr { e }
  ;

arithmetic_expr:
  | e1 = arithmetic_expr "+" e2 = multiply_expr { BinOp ( Add, e1, e2 ) }
  | e1 = arithmetic_expr "-" e2 = multiply_expr { BinOp ( Sub, e1, e2 ) }
  | e = multiply_expr { e }
  ;

multiply_expr:
  | e1 = multiply_expr "*" e2 = literal_expr { BinOp ( Mul, e1, e2 ) }
  | e1 = multiply_expr "/" e2 = literal_expr { BinOp ( Div, e1, e2 ) }
  | e1 = multiply_expr "%" e2 = literal_expr { BinOp ( Mod, e1, e2 ) }
  | e = literal_expr { e }
  ;

literal_expr:
  | fc = function_call { fc }
  | i = INTEGER_LIT { Integer( i ) }
  | b = BOOL_LIT { Bool( b ) }
  | c = CHAR_LIT { Char( c ) }
  | f = FLOAT_LIT { Float( f ) }
  | s = STRING_LIT { String( s ) }
  | id = IDENTIFIER { Identifier( id ) }
  | "unit" { Unit }
  | s = hashmap { HashMap( s ) }
  | l = array_list { List( l ) }
  | name = val_expr "[" key = val_expr "]" { HashMapAccess( name, key ) }
  | "(" v = val_expr ")" { v }
  | "{" b = block "}" { b }
  ;

function_call:
  | id = IDENTIFIER "(" al = args_list ")" { FuncCall( id, al ) }
  ;

args_list:
  | { [] }
  | l = ne_args_list { l }
  ;

ne_args_list:
  | v = val_expr { [v] }
  | v = val_expr "," rest = ne_args_list { v :: rest }
  ;

hashmap:
  | "{" fl = fields_list "}" { fl }
  ;

fields_list:
  | { [] }
  | f = hash_map_field "," rest = fields_list { f :: rest }
  ;

hash_map_field:
  | s = STRING_LIT ":" v = val_expr { ( s, v ) }
  ;

array_list:
  | "[" l = array_list_contents "]" { l }
  ;

array_list_contents:
  | { [] }
  | v = val_expr "," rest = array_list_contents { v :: rest }
  ;