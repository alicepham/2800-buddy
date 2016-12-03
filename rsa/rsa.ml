(*-----------Evalutation-----------*)

(*Key generation stuff*)

let p = 41
let q = 83
let n = 3403
let phi = 3280
let public_key = 17
let private_key = 193

(*Getting digits from a string*)
let string_to_list (s: string) =
  let rec helper c len =
    if c = len then []
    else (String.get s c)::(helper (c+1) len)
  in
  helper 0 (String.length s)

let (^^) x y =
  let x' = string_of_int x in
  let y' = string_of_int y in
  int_of_string (x'^y')

let is_upper x=
  (Char.code x > 64 && Char.code x < 91)

let rec list_to_digits lst =
  match lst with
    | [] -> 0
    | h::t -> if is_upper h
              then let code = Char.code - 65 in
                    code^^(list_to_digits t)
              else let code = Char.code - 70 in
                    code^^(list_to_digits t)

let encrypt str =
  let msg = int_of_string str in
  (exponentiate msg public_key) mod n

let decrypt str =
  let encrypted = encrypt str in
  (exponentiate encrypted private_key) mod n


(*------------------Printing and steps------------------*)
let print_stuff = ref ""

let to_print = !(print_stuff)


let step_1 =
  print_stuff := "First lets generate two large primes
p = 41 and q=83."

let step_2 =
  print_stuff := "Next we generate the modulus n by computing
  p*q = 3403"

let step_3 =
  print_stuff := "Now define the totient of the modulus, i.e
  the number of units in n. In our case, this is totient =
  (p-1)*(q-1) = 3280."

let step_4 =
  print_stuff := "We need a public key, or in other words, a large
  prime exponent that we can use to encrypt our message. Let's
  choose k = 17."

let step_5 =
  print_stuff := "The private key is generated by the recepient, defined
  as the inverse of k with respect to mod (totient). We can compute this
  by using the extended Euclidean algorithm, so that as-bt = 17s - 3403t = 1.
  This turns out to be 193."

let step_6 =
  print_stuff := "So now we have the following definitions:
    let p = 41
    let q = 83
    let n = 3403
    let phi = 3280
    let public_key = 17
    let private_key = 193"

let step_7 msg =
  let encrpyted_msg = string_of_int (encrypt msg) in
  print_stuff := "The encrypted message then is calculated by
  computing [msg]**17 mod (3403). The result is the following:
  " ^ " " ^ encrpyted_msg

let step_8 msg =
  let decrypted_msg = string_of_int (decrypt msg) in
  print_stuff := "The decrypted message is then " ^ " " ^ decrypted_msg ^ ".
  Which corresponds to our original message" ^ msg ^"."



