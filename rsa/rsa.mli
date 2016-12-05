(*Defines the RSA interface, for encryption of strings*)

(*[encrypt str] encrypts a string using RSA
 *requires:
 *  -str:string *)
val encrypt: string -> int list

(*[decrypt msg] returns a list of ints that correspond to a msg
 *requires:
 *  -msg is a large int representing the encoding of an English msg*)
val decrypt: int list -> int list

(* [translate lst] transforms a list of ints into the msg initially encrypted
 * Requires:
 *  - lst is a valid list of integers*)
val decode: int list -> string

(* [random_pick n] Returns a random prime number less than the limit [n]
 * Requires:
 *  - n is an integer less than 10^6+  *)
val random_pick: int -> int

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
val miller_rabin: int -> bool
