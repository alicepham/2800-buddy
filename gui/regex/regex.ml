open Ast
open Lexer
open Parser

type cc = int list

(*[parse str] parses a string to create an AST following the
 *regex grammar outlined by parser.mly
 *requires:
 *  -str: string*)
let parse str =
  let lexbuf = Lexing.from_string str in
  let ast = Parser.main Lexer.read lexbuf in
  ast

(*[print reg] produces a string from a regex tree*)
let rec print reg =
  match reg with
    | Empty -> "E"
    | Char a -> Char.escaped a
    | Concat(reg1, reg2) -> "("^(print reg1)^(print reg2)^")"
    | Or (reg1, reg2) -> "("^(print reg1) ^ "+" ^ (print reg2)^")"
    | Star reg -> "("^(print reg)^ ")*"


(*-----Operations for closure complexities------*)
(*[successor y] defines the successor of a closure complexity
 *i.e. the successor of each individual element of a list
 *requires:
 * -y: cc*)
let successor y =
  List.map (fun x -> x + 1) y

(*[union l1 l2] defines the union of two closure complexities,
 *which has the same definition as the union of two standard
 *mathematical lists*)
let union l1 l2 =
  l1@l2 |> List.fast_sort compare |> List.rev
  (*sort the lists then return the union with all elements preserved*)

(*-----Determing equality of regex and simplification--------*)

(*[cl_comp reg_exp] determines the closure complexity of a
 *given regex AST*)
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

(*-------Determining the relation <_cc---------*)
(*[chop lst n] *)
let rec chop lst n =
  match lst with
    | [] -> lst
    | h::t -> if n = 0 then [h] else h::(chop t (n-1))

(*helper for equal_cc_easy*)
let equal_cc_easy cc1 cc2 =
  let cc_chopped = chop cc2 (List.length cc1 -1) in
  List.for_all2 (fun x y -> x=y) cc1 cc_chopped

(*condition*)
let condition cc1 cc2 i =
  let acc = ref [] in
  for j=0 to (i-1) do
    acc := ((List.nth cc1 j) = (List.nth cc2 j))::(!acc)
  done;
  (not (List.mem false !acc)) && (List.nth cc1 i < List.nth cc2 i)


(*helper for equal_cc_hard*)
let rec equal_cc_hard cc1 cc2 i=
  if i < 1
  then false
  else (if condition cc1 cc2 i
        then true
        else equal_cc_hard cc1 cc2 (i-1))


(*Check if the closure complexity of the first regex is simpler than the
 *closure complexity of the second*)
let (@$$) cc1 cc2 =
  let len1 = List.length cc1 in
  let len2 = List.length cc2 in
  if len1 < len2
  then equal_cc_easy cc1 cc2
  else equal_cc_hard cc1 cc2 (len2 -1)

(*condition 3*)
let cond_3 str1 str2 =
  let size1 = String.length str1 in
  let size2 = String.length str2 in
  let reg1 = parse str1 in
  let reg2 = parse str2 in
  let cc1 = cl_comp reg1 in
  let cc2 = cl_comp reg2 in
  ((cc1 = cc2) && (size1=size2) &&
    (num_concats reg1 < num_concats reg2))

(*cond_4*)
let cond_4 str1 str2 =
  let size1 = String.length str1 in
  let size2 = String.length str2 in
  let reg1 = parse str1 in
  let reg2 = parse str2 in
  let cc1 = cl_comp reg1 in
  let cc2 = cl_comp reg2 in
  ((cc1 = cc2) && (size1=size2) &&
    (num_concats reg1 = num_concats reg2) &&
    (num_syms reg1 < num_syms reg2))

(*Check if a given regex is simpler than the second regex given*)
let is_simpler str1 str2 =
  let size1 = String.length str1 in
  let size2 = String.length str2 in
  let reg1 = parse str1 in
  let reg2 = parse str2 in
  let cc1 = cl_comp reg1 in
  let cc2 = cl_comp reg2 in
  ((cc1 @$$ cc2) ||
  ((cc1 = cc2) && (size1 < size2)) ||
  (cond_3 str1 str2) ||
  (cond_4 str1 str2))


(*-----------Implementation of Kleene Algebra------*)

(*[star_rule reg] applies the star rule of Kleene algebra:
 *aa* = a*a *)
let rec star_rule reg =
  match reg with
    | Empty -> Empty
    | Char a -> Char a
    | Concat(reg1, Star(reg2)) -> if reg1 = reg2
                                  then let reg' = star_rule reg1 in
                                  Concat(Star(reg'),reg')
                                  else
                                    Concat(star_rule reg1, Star(star_rule reg2))
    | Concat(reg1,reg2) -> Concat(star_rule reg1, star_rule reg2)
    | Or(reg1,reg2) -> Or(star_rule reg1, star_rule reg2)
    | Star reg -> Star(star_rule reg)

(*[star_rule2 reg] applies the idempotence of stars in Kleene Algebra:
 * a*a* = a* *)

let rec star_rule2 reg =
  match reg with
    | Empty -> Empty
    | Char a -> Char a
    | Concat(Star(reg1), Star(reg2)) -> if reg1 = reg2
                                        then Star(star_rule2 reg1)
                                        else Concat(Star(reg1),Star(reg2))
    | Concat(reg1,reg2) -> Concat(star_rule2 reg1, star_rule2 reg2)
    | Or(reg1,reg2) -> Or(star_rule2 reg1, star_rule2 reg2)
    | Star reg -> Star(star_rule2 reg)

(*[star_rule3 reg] implements the third rule of stars in Kleene Algebra:
 * a** = a* *)

let rec star_rule3 reg =
  match reg with
    | Empty -> Empty
    | Char a -> Char a
    | Star(Star(reg)) -> Star(star_rule3 reg)
    | Concat(reg1,reg2) -> Concat(star_rule3 reg1, star_rule3 reg2)
    | Or(reg1,reg2) -> Or(star_rule3 reg1, star_rule3 reg2)
    | Star reg -> Star(star_rule3 reg)

(*[denest reg] applies the denesting rule of Kleene Algebra:
 * (a*b* )a* = (a+b)**)

let rec denest reg =
  match reg with
    | Empty -> Empty
    | Char a -> Char a
    | Concat(Concat(Star(reg1),Star(reg2)),Star(reg3)) ->
          if reg1 = reg3
          then Star(Or(denest reg1, denest reg2))
          else Concat(Concat(Star(denest reg1),
                Star(denest reg2)),Star(denest reg3))
    | Concat(reg1,reg2) -> Concat(denest reg1, denest reg2)
    | Or(reg1, reg2) -> Or(denest reg1, denest reg2)
    | Star reg -> Star(denest reg)

(*[shift reg] applies the shifting rule of Kleene Algebra:
 * a(ba)* = (ab)*a *)

let rec shift reg =
  match reg with
    | Empty -> Empty
    | Char a -> Char a
    | Concat(reg1, Star(Concat(reg2,reg3))) ->
        if reg1 = reg3
        then Concat(Star(Concat(shift reg1, shift reg2)),(shift reg3))
        else Concat((shift reg1), Star(Concat(shift reg2, shift reg3)))
    | Concat(reg1,reg2) -> Concat(shift reg1, shift reg2)
    | Or(reg1,reg2) -> Or(shift reg1, shift reg2)
    | Star reg -> Star(shift reg)

(*[star_rule4 reg] applies the following rule from Kleene Algebra
 * a* = (aa)* + a(aa)* *)
let rec star_rule4 reg =
  match reg with
    | Empty -> Empty
    | Char a -> Char a
    | Concat(reg1, reg2) -> Concat(star_rule4 reg1, star_rule4 reg2)
    | Or(reg1, reg2) -> Or(star_rule4 reg1, star_rule4 reg2)
    | Star reg -> let reg' = star_rule4 reg in
                  Or(Star(Concat(reg',reg')),
                      Concat(reg',Star(Concat(reg',reg'))))
