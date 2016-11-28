%{
open Ast
%}


%token <char> CHAR
%token EMPTY
%token STAR
%token OR
%token LPAREN
%token RPAREN
%token EOF

%start main
%type <Ast.regex> main


%%

main:
  |r = or_regex EOF { r }
  ;

or_regex:
  | r = or_regex OR s = con_regex { Or (r, s) }
  | r = con_regex { r }
  ;

con_regex:
  | r = con_regex s = star_regex { Concat (r,s) }
  | r = star_regex { r }
  ;

star_regex:
  | EMPTY { Empty }
  | c = CHAR { Char c }
  | LPAREN r = or_regex RPAREN { r }
  | r = star_regex STAR { Star r }
  ;