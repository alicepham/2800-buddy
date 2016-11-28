(*A regex type represents an inputed string from the GUI which is then
 *stored in a tree structure with the following definition*)
open Ast
(*A closure complexity represents the complexity of a given regex*)
type cc

(*Check if a given regex is simpler than the second regex given*)
val is_simpler: regex -> regex -> bool

(*Simplify a given regex to return another regex which is in simplified
 *form*)
val simplify: regex -> regex

(*Returns an OCaml string for printing a regex to the GUI*)
val print: regex -> string