let add2: int -> int
= fun y -> y+2

let add4: int -> int
= fun x -> add2 (add2 x)

