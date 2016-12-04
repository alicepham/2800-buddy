(*Defines the RSA interface, for encrytion of strings*)

val encrypt: string -> int list

val decrypt: int list -> int list

val decode: int list -> string
