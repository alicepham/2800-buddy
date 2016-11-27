%{
open Ast
%}


%token <char> CHAR
%token EMPTY
%token STAR
%token OR
%token LPAREN
%token RPAREN
%token CONCAT
%token EOF

%nonassoc CHAR EMPTY LPAREN
%left OR
%left STAR
%left CONCAT


%start main
%type <Ast.regex> main


%%

reg_exp:
  |r = regex EOF { r }
  ;

regex:
  | EMPTY { Empty }
  | c = CHAR { Char c }
  | LPAREN r = regex RPAREN { r }
  | r = regex OR s = regex { Or(r,s) }
  | r = regex STAR { Star r }
  | r = regex s = regex { Concat(r,s) } %prec CONCAT
  ;

