open Ast
open Lexing
open Parser
open Util
open Eval

let print_help () = 
  print_endline "Hello, this is an interpreter for Monkey Programming Language";
  print_endline "    Created by Danel Batyrbek";
  print_endline "";
  print_endline "Usage:";
  print_endline "    ./monkey.native <file>.mnk";
  print_endline "";
  print_endline "Notes:";
  print_endline "    This is an alpha version of the code. In theory several";
  print_endline "    filenames should work, but it hasn't been tested and the";
  print_endline "    author is not currently looking to fix miniscule bugs";
  print_endline "";
  print_endline "    Feel free to make PRs at GitHub!"

let lexer_and_parser lex_buf = Parser.prog Lexer.read_token lex_buf

let buffered_lexer_and_parser lex_buf =
  let fn buf =
    let res = Lexer.read_token buf in
    print_endline ("Output from lexer: " ^ token_to_string res);
    res
  in
  Parser.prog fn lex_buf

let file_length f =
  let fd = Unix.openfile f [ Unix.O_RDONLY ] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  Unix.close fd;
  len

let read_file f =
  let fd = open_in f in
  let s = really_input_string fd (file_length f) in
  close_in fd;
  s

let lex_and_parse_file f =
  let lexbuf = open_in f |> Lexing.from_channel in
  let file_contents = read_file f in
  print_endline file_contents;
  buffered_lexer_and_parser lexbuf

let lex_and_parse_files files = Array.map lex_and_parse_file files

let print_asts asts =
  let handle v =
    match v with
    | Some x -> Ast.print_ast x
    | None -> print_string "<Couldn't read some file>"
  in
  Array.iter handle asts

let rec clean l =
  match l with [] -> [] | Some v :: rest -> v :: clean rest | None :: rest -> clean rest

let () =
  let args = Sys.argv in
  let len = Array.length args in
  if len = 1 then print_help ()
  else
    let files = Array.sub args 1 (len - 1) in
    let asts = lex_and_parse_files files in
    let _ = print_asts asts in
    let code = asts |> Array.to_list |> clean in
    evaluate code |> ignore