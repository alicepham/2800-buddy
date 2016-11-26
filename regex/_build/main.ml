open Ast
open Parser
open Lexer

let () =
  let s = read_line () in
  let r = Parser.reg_exp Lexer.token (Lexing.from_string s) in
  print_endline (format_regex r)