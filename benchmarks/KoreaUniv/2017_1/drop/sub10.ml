(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> let rec loop x y =
match x with
| [] -> []
| hd::tl -> if y = 0 then x
else (loop tl (y-1))
in (loop l n)