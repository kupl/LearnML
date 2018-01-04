(*2007-10575 조용훤*)
type nat = ZERO | SUCC of nat

(*
let zero = ZERO
and one = SUCC ZERO
and two = SUCC (SUCC ZERO)
let three = SUCC two
let four = SUCC three 
*)

let rec natadd (a, b) =
   let n = fst (a, b) in
   let m = snd (a, b) in
   match n with
    | ZERO -> m
    | SUCC o -> natadd (o, (SUCC m)) 

let rec natmul (a, b) = 
   let n = fst (a, b) in
   let m = snd (a, b) in
   match n with
    | ZERO -> ZERO
    | SUCC o -> natadd (m ,(natmul (o, m)))
