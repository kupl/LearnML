(* problem 8*)
let rec insert a l = match l with
  | [] -> [a]
  | hd::tl -> if hd>a then a::l else hd::(insert a tl)
let rec sort l =
  match l with
  | [] -> []
  | hd::tl -> insert hd (sort tl)

let rec ele_div c a = match c with
  | [] -> 0
  | hd::tl -> if a=0 then 1 else if a>hd then (ele_div tl a) + ele_div c (a-hd) else if a=hd then 1 else if a>0 then ele_div tl (a-hd) else 0

let change : int list -> int -> int
= fun coins amount -> match coins with
                      | [] -> 0
                      | hd::tl-> ele_div (sort coins) amount

