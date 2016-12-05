
(* The type of tokens. *)

type token = 
  | STAR
  | RPAREN
  | OR
  | LPAREN
  | EOF
  | EMPTY
  | CHAR of (char)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.regex)
