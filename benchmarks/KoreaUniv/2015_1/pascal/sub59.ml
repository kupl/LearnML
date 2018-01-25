(* Problem 1 *)
let rec factorial n =
  match n with
      0 -> 1
     |1 -> 1
     |_ -> n * factorial (n - 1);;

let pascal : int * int -> int
=fun (x,y) -> if x=0 || y=0 then 1
              else factorial x/(factorial (x - y) * factorial y);;
