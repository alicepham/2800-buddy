(*Defines the RSA interface, for encryption of strings*)

(* [step_1] generates the large primes [p] and [q]
 * Requires:
 *  - p is prime integer
 *  - q is a prime integer*)
val step_1 : string -> string -> string

(* [step_2] defines and generates the modulus
 * Requires:
 *  - p is prime integer
 *  - q is a prime integer *)
val step_2: string -> string -> string

(* [step_3] defines and computes the totient
 * Requires:
 *  - p is prime integer
 *  - q is a prime integer *)
val step_3: string -> string -> string

(* [step_4] explains how to generate the public key
 * Requires:
 *  - public_key is a prime integer used for exponentiation*)
val step_4: string -> string

(* [step_5] explains how to generate the private key
 * Requires:
 *  - p is a prime integer
 *  - q is a prime integer
 *  - public_key is a prime integer used for exponentiation*)
val step_5: string -> string -> string -> string

(* [step_6] prints the user's determined fields for public and private
 * keys as well as the large primes
 * Requires:
 *  - p is a prime integer
 *  - q is a prime integer
 *  - public_key is a prime integer used for exponentiation *)
val step_6: string -> string -> string -> string

(* [step_7 msg] encrypts the user's message, and displays the resulting
 * encryption
 * Requires:
 *  - msg is a valid string of the user's input to encrypt
 *  - public_key is a prime integer used for exponentiation
 *  - n is the totient value to be moded by*)
val step_7: string -> string -> string -> string

(* [step_8 lst] decrypts the message to a large int displayed on the
 * screen.
 * Requires:
 *  - lst is an int list, the result of the encrypted message
 *  - p is a prime integer
 *  - q is a prime integer
 *  - public_key is a prime integer used for exponentiation
 *  - n is the totient value to be moded by *)
val step_8: int list -> string -> string -> string -> string -> string

(* [step_9 msg] Displays the decoded message than had been decrypted. This
 * matches the message originally sent.
 * Requires:
 *  - lst is the int list returned by the decryption *)
val step_9: int list -> string

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
