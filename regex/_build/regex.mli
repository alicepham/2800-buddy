(*A regex type represents an inputed string from the GUI which is then
 *stored in a tree structure with the definition given in ast.ml*)
open Ast

(*A closure complexity represents the complexity of a given regex.
 *It is defined formally as a list of descending ints*)
type cc

(*------Printing and Parsing stuff------------------*)

(*[parse] parses a given regex to its associated AST*)
val parse: string -> regex

(*[print] prints a regex tree in a string*)
val print: regex -> string

(*---------------Rules of Kleene Algebra-------------*)

(*[star_rule reg] applies the star rule of Kleene algebra:
 * aa* = a*a *)
val star_rule: regex -> regex

(*[star_rule2 reg] applies the following rule of Kleene Algebra:
 * a*a* = a* *)
val star_rule2: regex -> regex

(*[star_rule3 reg] implements the third rule of stars in Kleene Algebra:
 * a** = a* *)
val star_rule3: regex -> regex

(*[star_rule4 reg] applies the following rule from Kleene Algebra
 * a* = (aa)* + a(aa)* *)
val star_rule4: regex -> regex

(*[denest reg] applies the denesting rule of Kleene Algebra:
 * (a*b* )a* = (a+b)* *)
val denest: regex -> regex

(*[shift reg] applies the shifting rule of Kleene Algebra:
 * a(ba)* = (ab)*a *)
val shift: regex -> regex


