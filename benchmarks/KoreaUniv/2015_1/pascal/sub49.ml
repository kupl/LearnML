(* Programmed by Dong Hyun Koo - 2009210036 *)
(* E-mail : tellmewhy07@gmail.com *)
(* Github : https://github.com/AlwaysAwake *)

(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y = 0 || x = y then 1 else pascal(x-1, y-1) + pascal(x-1, y) (* TODO *)
