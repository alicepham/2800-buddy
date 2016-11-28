type regex =
  | Empty
  | Char of char
  | Concat of regex * regex
  | Star of regex
  | Or of regex * regex