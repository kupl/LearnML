let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match a, b with
    [], [] -> []
  | a, [] -> a
  | [], b -> b
  | ahead::atail, bhead::btail -> ahead::bhead::zipper(atail, btail)


