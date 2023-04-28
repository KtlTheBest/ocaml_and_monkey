{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    }

(*
References:
    https://github.com/mukul-rathi/bolt/blob/master/src/frontend/parsing/lexer.mll
*)
}

let digit = ['0' - '9']

let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let integer = "0" | ( ['1'-'9' ] digit* )
let floating_point = '.' digit+
let floating = integer floating_point
let alphabet = ['a'-'z' 'A'-'Z' '_']
let alnum = alphabet | digit
let identifier = alphabet alnum*
let special_symbols = "\\" ['x' 'n' 't' 'r' 'b' 'a' '0' '\\' '\'']
let readable_symbols = ['a'-'z' '0'-'9' 'A'-'Z' '_' '+' '-' '*' '/' '|']
let charlit = special_symbols | readable_symbols

rule read_token = parse
| whitespace { read_token lexbuf }
| newline { read_token lexbuf }
| '+' { ADD }
| '-' { SUB }
| '*' { MUL }
| '/' { DIV }
| '%' { MOD }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '>' { GT }
| ">=" { GE }
| '<' { LT }
| "<=" { LE }
| '(' { LPAR }
| ')' { RPAR }
| '{' { LCUR }
| '}' { RCUR }
| '[' { LSQR }
| ']' { RSQR }
| ',' { COMMA }
| ":" { COLON }
| ";" { SEMICOLON }
| "if" { IF }
| "else" { ELSE }
| "elif" { ELIF }
| "fn" { FN }
| "let" { LET }
| "return" { RETURN }
| "true" { BOOL_LIT(true) }
| "false" { BOOL_LIT(false) }
| "unit" { UNIT }
| "//" { read_single_line_comment lexbuf }
| '\'' { read_char lexbuf }
| "\"" { read_string (Buffer.create 16) lexbuf }
| integer { INTEGER_LIT( int_of_string( Lexing.lexeme lexbuf ) ) }
| floating { FLOAT_LIT( float_of_string( Lexing.lexeme lexbuf ) ) }
| identifier { IDENTIFIER( Lexing.lexeme lexbuf ) }
| eof { EOF }

and read_char = parse
| '\\' 'n' '\'' { CHAR_LIT ( '\n' ) }
| '\\' 't' '\'' { CHAR_LIT ( '\t' ) }
| '\\' 'b' '\'' { CHAR_LIT ( '\b' ) }
| '\\' 'r' '\'' { CHAR_LIT ( '\r' ) }
| '\\' '\\' '\'' { CHAR_LIT ( '\\' ) }
| '\\' '\'' '\'' { CHAR_LIT ( '\'' ) }
| _ '\'' { CHAR_LIT (Lexing.lexeme_char lexbuf 0) }

and read_single_line_comment = parse
| newline { read_token lexbuf }
| eof { EOF }
| _ { read_single_line_comment lexbuf }

and read_string buf = parse
| '"' { STRING_LIT (Buffer.contents buf) }
| '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
| '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' '\'' { Buffer.add_char buf '\''; read_string buf lexbuf }
| [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
| _ { raise ( SyntaxError ("Illegal character encountered during string...") ) }
| eof { raise ( SyntaxError ("String is not terminated") ) }