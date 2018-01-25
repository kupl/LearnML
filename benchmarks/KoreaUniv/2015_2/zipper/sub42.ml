let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match b with
  | [] -> a
  | hd :: tl ->
      (match a with
      |[] -> b
      | hda :: tla -> if hd<hda then (zipper(hd::a, tl)) else ( hda::zipper(tla , b) ));;

