(*A regex type represents an inputed string from the GUI which is then
 *stored in a tree structure with the following definition*)
open Ast
(*A closure complexity represents the complexity of a given regex.
 *It is defined formally as a list of descending ints*)
type cc

(*Check if a given regex is simpler than the second regex given*)
val is_simpler: string -> string -> bool

(*Check if two regex's are equivalent*)
val is_equal: string -> string -> bool
