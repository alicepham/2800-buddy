open Random
(*-----------Evalutation-----------*)

(*Key generation*)
let p = "41"
let q = "83"
let n = "3403"
let phi = "3280"
let public_key = "17"
let private_key = "193"

(* [string_to_list s ] Transforms string [s] into a list of  characters in [s]
 * Requires:
 *  - s is a valid string type*)
let string_to_list (s: string) =
  let rec helper c len =
    if c = len then []
    else (String.get s c)::(helper (c+1) len)
  in
  helper 0 (String.length s)

(* [fast_exponentiate a b m] raises [a] to power [b] through an exponentiation
 * by squaring approach to handle large power cases
 * Example: 5^55 mod 221
 *
 *    5^55 = 5^(1,2,4,8,16,32) mod 221
 *    5^1 * 5^2 * 5^4 * 5^8 * 5^16 * 5^32 mod 221
 *    5 * 25 *  183 * 1 * 1 mod 221
 *    22875 mod 221
 *    112 mod 221
 *
 * Requires:
 *  - a is a valid integer representing the base
 *  - b is a valid integer representing the power >= 0
 *  - m is a valid integer representing the value to be moduloed by*)
let fast_exponentiate a b m =
  let a1 = a mod m in
  let p = 1 in
  let rec helper a_h b_h p_h =
    if b_h = 0 then p_h else
      if (b_h mod 2 = 1) then
         helper (a_h*a_h mod m) (b_h/2) (p_h*a_h mod m)
      else
         helper (a_h*a_h mod m) (b_h/2) (p_h)
    in helper a1 b p

(* [sieve n]*)
let sieve n =
  let is_prime = Array.make n true in
  let limit = truncate(sqrt (float_of_int(n -1))) in
  for i = 2 to limit do
    if is_prime.(i) then
      let j = ref (i*i) in
      while !j < n do
        is_prime.(!j) <- false;
        j := !j + i;
      done
  done;
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  is_prime

  (*[primes n]*)
let primes n =
 let primes, _ =
  let sieve = sieve n in
  Array.fold_right
    (fun is_prime (xs, i) -> if is_prime then (i::xs, i-1) else (xs, i-1))
    sieve
    ([], Array.length sieve - 1) in
  primes

(* [random_pick lst] *)
let random_pick n =
  let len = List.length (primes n) in
  List.nth (primes n) (Random.int len)

(* [extended_euclidean a b] Computes the extended euclidean division algorithm
 * for [a] and [b] of the form. This computes both the greatest common
 * divisor and the coefficients of Bezout's Identity. That identity is
 *    ax + by = gcd(a,b)
 * This computes the private key through the modular multiplicative inverse*)
let rec extended_euclidean a b =
  if b = 0 then (a, 1, 0)
  else match (extended_euclidean b (a mod b)) with
    | (x, y, z) -> x, z, y - a/b*z

(* [get_third x] Returns the third element of a triple*)
let get_third x : int  =
  match x with
  | (x,y,z) -> z

(* [is_upper x] Determines whether the character is uppercase
 * Requires:
 *  - x is a valid character of the upper or lowercase English alphabet*)
let is_upper x=
  (Char.code x > 64 && Char.code x < 91)

(* [list_to_digits lst] Tranforms a list of characters into their respective
 * ASCII values
 * Requires:
 *  - lst is a list of characters of the upper or lowercase English alphabet *)
let rec list_to_digits lst =
  match lst with
    | [] -> []
    | h::t -> if is_upper h
              then let code = Char.code h - 65 in
                    code ::(list_to_digits t)
              else if h = ' ' then let code = 53 in
                    code ::(list_to_digits t)
              else if h = '.' then let code = 54 in
                    code ::(list_to_digits t)
              else if h = ',' then let code = 55 in
                    code ::(list_to_digits t)
              else if h = '!' then let code = 56 in
                    code ::(list_to_digits t)
              else if h = '?' then let code = 57 in
                    code ::(list_to_digits t)
              else if h = ';' then let code = 58 in
                    code ::(list_to_digits t)
              else if h = ':' then let code = 59 in
                    code ::(list_to_digits t)
              else if h = '0' then let code = 60 in
                    code ::(list_to_digits t)
              else if h = '1' then let code = 61 in
                    code ::(list_to_digits t)
              else if h = '2' then let code = 62 in
                    code ::(list_to_digits t)
              else if h = '3' then let code = 63 in
                    code ::(list_to_digits t)
              else if h = '4' then let code = 64 in
                    code ::(list_to_digits t)
              else if h = '5' then let code = 65 in
                    code ::(list_to_digits t)
              else if h = '6' then let code = 66 in
                    code ::(list_to_digits t)
              else if h = '7' then let code = 67 in
                    code ::(list_to_digits t)
              else if h = '8' then let code = 68 in
                    code ::(list_to_digits t)
              else if h = '9' then let code = 69 in
                    code ::(list_to_digits t)
              else let code = Char.code h - 70 in
                    code ::(list_to_digits t)

(*[encrypt str] encrypts a string using RSA
 *requires:
 *  -str:string *)
let encrypt str =
  let chars = string_to_list str in
  let numbers = list_to_digits chars in
  List.map(fun x-> fast_exponentiate x (int_of_string public_key)
          (int_of_string n)) numbers

(*[decrypt msg] returns a list of ints that correspond to a msg
 *requires:
 *  -msg is a large int representing the encoding of an English msg*)
let decrypt str =
  List.map (fun x -> fast_exponentiate x (int_of_string private_key)
                     (int_of_string n)) str

(* [int_to_char x] Adjusts the standard char to int mapping by an offset for
 * the encoding cypher alphabet values
 * Requires:
 *  - x is a valid integer between 0 and 25 inclusive or 27 and 53 inclusive*)
let int_to_char x =
  if x > 26 && x< 53 then Char.chr (x + 70)
  else if x = 53 then ' '
  else if x = 54 then '.'
  else if x = 55 then ','
  else if x = 56 then '!'
  else if x = 57 then '?'
  else if x = 58 then ';'
  else if x = 59 then ':'
  else if x = 60 then '0'
  else if x = 61 then '1'
  else if x = 62 then '2'
  else if x = 63 then '3'
  else if x = 64 then '4'
  else if x = 65 then '5'
  else if x = 66 then '6'
  else if x = 67 then '7'
  else if x = 68 then '8'
  else if x = 69 then '9'
  else Char.chr(x + 65)

(* [translate lst] transforms a list of ints into the msg initially encrypted
 * Requires:
 *  - lst is a valid list of integers*)
let decode lst =
  let trans_chars = List.map (fun x -> int_to_char x) lst in
  List.fold_left (fun acc x -> acc ^(String.make 1 x) ) "" trans_chars

(*------------------Printing and steps------------------*)
let print_stuff = ref ""

let to_print = !(print_stuff)

(*[step_1] generates the large primes*)
let step_1 =
  print_stuff := "First lets generate two large primes
p =" ^ p ^  "and q =" ^ q

(*[step_2] defines and generates the modulus*)
let step_2 =
  print_stuff := "Next we generate the modulus n by computing
  p*q =" ^ string_of_int((int_of_string p) * (int_of_string q))

(*[step_3] defines and computes the totient*)
let step_3 =
  print_stuff := "Now define the totient of the modulus, i.e
  the number of units in n. In our case, this is totient = (p-1)*(q-1)" ^
  string_of_int((int_of_string(p)-1)*(int_of_string(q)-1))

(*[step_4] explains how to generate the public key*)
let step_4 =
  print_stuff := "We need a public key, or in other words, a large
  prime exponent that we can use to encrypt our message. Let's
  choose k = " ^ public_key

(*[step_5] explains how to generate the private key*)
let step_5 =
  print_stuff := "The private key is generated by the recepient, defined
  as the inverse of k with respect to mod (totient). We can compute this
  by using the extended Euclidean algorithm, so that as-bt = 17s - 3403t = 1.
  This turns out to be" ^
  string_of_int(get_third
  (extended_euclidean(int_of_string phi) (int_of_string public_key)))

(*[step_6] prints the user's determined fields for public and private
 *keys as well as the large primes*)
let step_6 =
  print_stuff := "So now we have the following definitions:
    let p = " ^  p ^
    "let q = " ^  q ^
    "let n = " ^ string_of_int (int_of_string(p)*int_of_string(q)) ^
    "let phi = " ^ string_of_int((int_of_string(p)-1)*(int_of_string(q)-1)) ^
    "public_key = " ^  public_key ^
    "let private_key =" ^ private_key

(*[step_7 msg] encrypts the user's message, and displays the resulting
 *encryption*)
let step_7 msg =
  let encrpyted_msg = encrypt msg in
  print_stuff := "The encrypted message then is calculated by
  computing [msg]**17 mod (3403). The result is the following: " ; encrpyted_msg

(*[step_8 lst] decrypts the message to a large int displayed on the
 *screen. *)
let step_8 lst =
  let decrypted_lst = decrypt lst in
  print_stuff := "The decrypted value is then " ; decrypted_lst

(* [step_9 msg] Displays the decoded message than had been decrypted. This
 * matches the message originally sent.*)
let step_9 msg =
  let decrypted_msg = decode msg in
  print_stuff := "The decrypted message is then " ^" "^ decrypted_msg ^ " "^
  "this matches the original message sent "
