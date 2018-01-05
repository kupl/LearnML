(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 2 Exercise 4  *)

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec galculator (e : exp) : float =
    let rec calc e v =
        match e with
        | X -> (match v with
                  Some(c) -> c
                | None -> raise FreeVariable)
        | INT x -> float_of_int x
        | REAL x -> x
        | ADD(lhs, rhs) -> (calc lhs v) +. (calc rhs v)
        | SUB(lhs, rhs) -> (calc lhs v) -. (calc rhs v)
        | MUL(lhs, rhs) -> (calc lhs v) *. (calc rhs v)
        | DIV(lhs, rhs) -> (calc lhs v) /. (calc rhs v)
        | SIGMA(low, high, body) ->
            let intLow = int_of_float (calc low v)
            and intHigh = int_of_float (calc high v) in
            sigma intLow intHigh body 0.
        | INTEGRAL(a, b, body) ->
            let flLow = calc a v
            and flHigh = calc b v in
            integral flLow flHigh body 0.
    and sigma low high e acc =
        if low > high then acc
        else let evaluation = calc e (Some(float_of_int low)) in
            sigma (low+1) high e (acc +. evaluation)
    and integral low high e acc =
        if (abs_float (low-.high)) < 0.1 then acc
        else if low > high then -.(integral high low e acc)
        else let evaluation = calc e (Some low) in
            integral (low +. 0.1) high e (acc +. evaluation *. 0.1)
    in
    calc e None
