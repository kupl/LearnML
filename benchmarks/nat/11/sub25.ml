type nat = ZERO | SUCC of nat

let rec natadd(a, b) = 
  match a with
  | ZERO -> b
  | SUCC(n) -> natadd(n, SUCC(b))

let rec natmul(a, b) = 
  let rec proc(x, y) = 
    match (x,y) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (SUCC(ZERO), SUCC(ZERO)) -> SUCC(ZERO)
    | (SUCC(ZERO), SUCC(n)) -> SUCC(proc(a, n))
    | (SUCC(n), _) -> SUCC(proc(n, y))
  in
  proc(a, b)


(*
let a = SUCC(SUCC(ZERO))
let b = SUCC(SUCC(SUCC(ZERO)))
*)

