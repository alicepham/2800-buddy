let string_to_list (s: string) =
  let rec helper c len = 
    if c = len then []
    else (String.get s c)::(helper (c+1) len)
  in 
  helper 0 (String.length s)