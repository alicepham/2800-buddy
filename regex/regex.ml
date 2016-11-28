open Ast
open Lexer
open Parser

type cc = int list

(*Parses a given regex string input from the GUI to create a tree
 *structure representing it*)
let parse str =
  let lexbuf = Lexing.from_string str in
  let ast = Parser.main Lexer.read lexbuf in
  ast

(*type of a regex
  type regex =
    | Empty
    | Char of char
    | Concat of regex * regex
    | Star of regex
    | Or of regex * regex)*)


(*-----Operations for closure complexities------*)
let successor y =
  List.map (fun x -> x + 1) y

let union l1 l2 =
  l1@l2 |> List.fast_sort compare |> List.rev
  (*sort the lists then return the union with all elements preserved*)

(*-----Determing equality of regex----*)

(*Determines the closure complexity of a given regex*)
let rec cl_comp reg_exp =
  match reg_exp with
    | Empty -> [0]
    | Char _ -> [0]
    | Concat (reg1,reg2) -> union (cl_comp reg1) (cl_comp reg2)
    | Star reg -> successor (cl_comp reg)
    | Or (reg1, reg2) -> union (cl_comp reg1) (cl_comp reg2)

(*Determines the number of concatenations in a given regex*)
let rec num_concats reg_exp =
  match reg_exp with
    | Empty -> 0
    | Char c -> 0
    | Concat (reg1,reg2) -> 1 + (num_concats reg1) + (num_concats reg2)
    | Star reg -> num_concats reg
    | Or (reg1, reg2) -> (num_concats reg1) + (num_concats reg2)

(*Determines the number of symbols in a given regex*)
let rec num_syms reg_exp =
  match reg_exp with
    | Empty -> 0
    | Char c -> 1
    | Concat (reg1,reg2) -> (num_syms reg1) + (num_syms reg2)
    | Star reg -> num_syms reg
    | Or (reg1,reg2) -> (num_syms reg1) + (num_syms reg2)

(*Check if the closure complexity of the first regex is simpler than the
 *closure complexity of the second*)
let is_simplercc reg1 reg2 = failwith "Unimplemented"

(*Check if a given regex is simpler than the second regex given*)
let is_simpler reg1 reg2 = failwith "Unimplemented"

(*Simplify a given regex to return another regex which is in simplified
 *form*)
let simplify reg_exp = failwith "Unimplemented"

(*Returns an OCaml string for printing a regex to the GUI*)
let print reg_exp = failwith "Unimplemented"