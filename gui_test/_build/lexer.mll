(*Open the Parser module for use in lexer.ml*)
{
open Parser
}

(*Define regular expressions for the regex AST, using identifiers*)

let white = [' ' '\t']+


rule read =
  parse
  | white {read lexbuf}
  | ['a'-'z'] as c { CHAR c }
  | 'E' { EMPTY }
  | '*' { STAR }
  | '+' { OR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }