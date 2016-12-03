(*Key generation stuff*)

let p = 41
let q = 83
let n = 3403
let phi = 3280
let public_key = (17, 3403)
let private_key = (193, 3403)

(*Getting digits from a string*)

let string_to_list s =
  let rec helper c len =
    if c = len then []
    else (String.get s c)::(helper (c+1) len)
  in
  helper 0 (String.length s)

let list_to_digits lst =
  List.map (fun x -> Char.code x) lst