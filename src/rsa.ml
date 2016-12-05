open Random
(*-----------Evalutation-----------*)

(* [string_to_list s ] Transforms string [s] into a list of  characters in [s]
 * Requires:
 *  - s is a valid string type*)
let string_to_list (s: string) =
  let rec helper c len =
    if c = len then []
    else (String.get s c)::(helper (c+1) len)
  in
  helper 0 (String.length s)

(* [list_to_string lst] Transform a list of integers into a string
 * representation
 * Requires:
 *  - lst is a valids int list *)
let rec list_to_string lst =
  match lst with
    | [] -> ""
    | h::t -> (string_of_int h)^(list_to_string t)

(* [phi p q] Returns the totient function phi value
 * Requires:
 *  - p is a prime integer
 *  - q is a prime integer *)
let phi p q =
  string_of_int((int_of_string(p)-1)*(int_of_string(q)-1))

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

(* [sieve n] Creates an array of size [n] where elements are initalized to bools
 * according to the Sieve of Eratosthenes algorithm. To find all prime numbers
 * less than a given integer [n]:
 *  "1) Create a list of consecutive integers from 2 through (n-1):(2,3,...,n-1)
 *   2) Initially, let p = 2, the smallest prime number
 *   3) Enumerate the multiples of p counting to n from 2p in increments of p
 *      and mark them in the list (these will be 2p, 3p, 4p,...,; the p itself
 *      should not be marked)
 *   4) Find the first number greater than p in the list that is not marked. If
 *      there was no such number, stop. Otherwise, let p now equal this new
 *      number (which is the next prime), and repeat from step 3"
 *
 * the boolean elements of the array created by [sieve n] correspond to false
 * for non-prime values and true for prime values.
 * Acknowledgement: Code is adapted from Rosetta code discussion of Sieve and
 * Primes. Found here https://rosettacode.org/wiki/Sieve_of_Eratosthenes#OCaml
 * Requires:
 *  - n is an integer less than 10^6+ *)
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

(* [primes n] Populates an array of size [n] as determined by the Sieve of
 * Eratosthenes. elements of the array set to true are updated with their
 * respective prime numbers.
 * Acknowledgement: Code is adapted from Rosetta code discussion of Sieve and
 * Primes. Found here https://rosettacode.org/wiki/Sieve_of_Eratosthenes#OCaml
 * Requires:
 *  - n is an integer less than 10^6+ *)
let primes n =
 let primes, _ =
  let sieve = sieve n in
  Array.fold_right
    (fun is_prime (xs, i) -> if is_prime then (i::xs, i-1) else (xs, i-1))
    sieve
    ([], Array.length sieve - 1) in
  primes

(* [random_pick n] Returns a random prime number less than the limit [n]
 * Requires:
 *  - n is an integer less than 10^6+  *)
let random_pick n =
  let len = List.length (primes n) in
  List.nth (primes n) (Random.int len)

(* [miller_rabin n] Employs the Miller-Rabin primality test to integer [n]
 * Approach relies on the "converse of Fermat's Theorem we know that:
 *         n-1      1
          a     ~  n   for any prime n and any a in [2,n-1]. If this fails on
 * an [n] then we know that [n] is not prime. so we write
                    s
 *        n-1 as d*2  , where d is odd. If n is prime, then the sequence hits 1
 * and stays there from then on. If [n] is prime then n-1 must hold the
 * position in the sequence right before the first 1." [miller-rabin n] returns
 * false if the sequence does not end in 1 indicating that [n] is composite
 * and true if [n] is probably prime. This approach does not use random witness
 * so as a result is only accurate up to 2 billion. Instead factors 2,3,5, and 7
 * are hard checked.
 * Acknowledgement: This code is adapted from Carnegie Mellon's 15-251 course
 * "Great Theoretical Ideas in Computer Science" Lecture 14, Fall 2010.
 * Requires:
 *   - n is an integer less than 2*10^5+ *)
let miller_rabin n =
  if n <= 10 then (n=2 || n=3 || n=5 || n=7) else
    if (n mod 2=0 || n mod 3=0 || n mod 5=0 || n mod 7=0) then false else
      let rec remove_twos m =
        let h = m/2 in
          if (h+h <m) then (0,m) else
            let (s,d) = remove_twos h in (s+1,d)
      in
        let (s,d) = remove_twos (n-1) in
          let is_witness_to_compositness a =
          let x = fast_exponentiate a d n in
            if x = 1 || x =(n-1) then false else
              let rec loop x r =
                if x = 1 || r = s then true else
                  if x = (n-1) then false else
                    loop((x*x) mod n) (r+1)
              in loop ((x*x) mod n ) 1
          in
          if (is_witness_to_compositness 2) then false
          else if (is_witness_to_compositness 3) then false
          else if (is_witness_to_compositness 5 ) then false
          else if (is_witness_to_compositness 7) then false
          else true

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

(* [private_key p q public_key] Generates private key from prime numbers [p]
 * and [q]
 * Requires:
 *  - p is a prime integer
 *  - q is a prime integer*)
let private_key p q public_key =
string_of_int(get_third
  (extended_euclidean(int_of_string (phi p q)) (int_of_string public_key)))

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
let encrypt str public_key n =
  let chars = string_to_list str in
  let numbers = list_to_digits chars in
  List.map(fun x-> fast_exponentiate x (int_of_string public_key)
          (int_of_string n)) numbers

(*[decrypt msg] returns a list of ints that correspond to a msg
 *requires:
 *  -msg is a large int representing the encoding of an English msg*)
let decrypt lst p q public_key n  =
  List.map (fun x -> fast_exponentiate x
                     (int_of_string(private_key p q public_key))
                     (int_of_string n)) lst

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

(*[step_1] generates the large primes*)
let step_1 p q =
  "First lets generate two large primes
p =" ^ p ^  "and q =" ^ q

(*[step_2] defines and generates the modulus*)
let step_2 p q =
  "Next we generate the modulus n by computing
  p*q =" ^ string_of_int((int_of_string p) * (int_of_string q))

(*[step_3] defines and computes the totient*)
let step_3 p q =
   "Totient = n = (p-1)*(q-1) = " ^
  string_of_int((int_of_string(p)-1)*(int_of_string(q)-1))

(*[step_4] explains how to generate the public key*)
let step_4 public_key =
   "Choose a public key, a large prime like" ^ public_key

(*[step_5] explains how to generate the private key*)
let step_5 p q public_key =
   "Private key (inverse of k), through extended Euclidean division is"
  ^ (private_key p q public_key)

(*[step_6] prints the user's determined fields for public and private
 *keys as well as the large primes*)
let step_6 p q public_key =
   "So: let p = " ^  p ^ ", " ^ "q = " ^ q ^
    ", n = " ^ string_of_int (int_of_string(p)*int_of_string(q)) ^
    ", phi = " ^ string_of_int((int_of_string(p)-1)*(int_of_string(q)-1)) ^
    ", public_key = " ^  public_key ^
    ", private_key =" ^ (private_key p q public_key)

(*[step_7 msg] encrypts the user's message, and displays the resulting
 *encryption*)
let step_7 msg public_key n =
  let encrpyted_msg = encrypt msg public_key n in
 "encrpyted msg = [msg]**(public_key) mod n. =" ^
  (list_to_string encrpyted_msg)

(*[step_8 lst] decrypts the message to a large int displayed on the
 *screen. *)
let step_8 lst p q public_key n =
  let decrypted_lst = decrypt lst p q public_key n in
  "The decrypted value is then " ^ (list_to_string decrypted_lst)

(* [step_9 msg] Displays the decoded message than had been decrypted. This
 * matches the message originally sent.*)
let step_9 lst =
  let decrypted_msg = decode lst in
   "The decrypted message is then " ^" "^ decrypted_msg
