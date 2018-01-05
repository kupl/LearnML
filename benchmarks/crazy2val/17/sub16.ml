(*컴퓨터공학부/2011-11729/안진우/2-2*)

type crazy2 = NIL
        | ZERO of crazy2
        | ONE of crazy2
        | MONE of crazy2

let rec eval_h ((x: crazy2), (y: float)) : float =
        match (x, y) with
        | (NIL, c) -> 0. 
        | (ZERO z, c) -> eval_h(z, c+.1.)
        | (ONE z, c) -> (2.**c) +. eval_h(z, c+.1.)
        | (MONE z, c ) -> -.(2.**c) +. eval_h(z, c+.1.) 
       
let crazy2val (x: crazy2) : int =
        eval_h (x, 0.) |> int_of_float
