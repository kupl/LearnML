let rec drop l n =
  match n with
  |0 -> l
  |_ -> match l with
    |[] -> []
    |_::tl -> drop tl (n-1)
