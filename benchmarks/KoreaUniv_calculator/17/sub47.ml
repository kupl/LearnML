(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_eval e n =
        match e with
        | X -> n
        | INT a -> a
        | ADD (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 + v2
        | SUB (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 - v2
        | MUL (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 * v2
        | DIV (e1, e2) ->
                let v1 = calc_eval e1 n in
                let v2 = calc_eval e2 n in
                v1 / v2
        | SIGMA (e1, e2, e3) ->
                let f = calc_eval e1 n in
                let t = calc_eval e2 n in
                if f > t then 0 else (calc_eval e3 f) + (calc_eval (SIGMA (INT (f+1), INT (t), e3)) n)
let rec calculator : exp -> int
= fun e ->
        match e with
        | X -> raise (Failure "Incomputable Formula")
        | INT n -> n
        | ADD (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e2 in
                v1 + v2
        | SUB (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e2 in
                v1 - v2
        | MUL (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e2 in
                v1 * v2
        | DIV (e1, e2) ->
                let v1 = calculator e1 in
                let v2 = calculator e1 in
                v1 / v2
        | SIGMA (e1, e2, e3) ->
                let f = calculator e1 in
                let t = calculator e2 in
                if f > t then 0 else (calc_eval e3 f) + calculator (SIGMA (INT (f+1), INT (t), e3))
