(* 1 *) 
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

let rec to_int_function e =
        match e with
        | X -> (fun x -> x)
        | INT n -> (fun x -> n)
        | REAL n -> (fun x -> (int_of_float n))
        | ADD(a, b) ->
          (fun x -> ((to_int_function a) x) + ((to_int_function b) x))
        | SUB(a, b) ->
          (fun x -> ((to_int_function a) x) - ((to_int_function b) x))
        | MUL(a, b) ->
          (fun x -> ((to_int_function a) x) * ((to_int_function b) x))
        | DIV(a, b) ->
          (fun x -> ((to_int_function a) x) / ((to_int_function b) x))
        | _ -> raise FreeVariable

let rec to_float_function e =
        match e with
        | X -> (fun x -> x)
        | INT n -> (fun x -> (float_of_int n))
        | REAL n -> (fun x -> n)
        | ADD(a, b) ->
          (fun x -> ((to_float_function a) x) +. ((to_float_function b) x))
        | SUB(a, b) ->
          (fun x -> ((to_float_function a) x) -. ((to_float_function b) x))
        | MUL(a, b) ->
          (fun x -> ((to_float_function a) x) *. ((to_float_function b) x))
        | DIV(a, b) ->
          (fun x -> ((to_float_function a) x) /. ((to_float_function b) x))
        | _ -> raise FreeVariable


let rec sigma (a, b, f) = 
        try
            if a > b then 0.0
            else if a = b then (f (float_of_int a))
            else (f (float_of_int a)) +. sigma((a+1), b , f)
        with _ -> raise FreeVariable

let rec integral (a, b ,f) = 
        try
            if b -. a < 0.1 then 0.0
            else ((f a) *. 0.1) +. integral((a+.0.1), b, f)
        with _ -> raise FreeVariable

let rec galculator exp = 
        match exp with
        | X -> raise FreeVariable
        | INT x -> (float_of_int x)
        | REAL x -> x
        | ADD(a, b) -> (galculator a) +. (galculator b)
        | SUB(a, b) -> (galculator a) -. (galculator b)
        | MUL(a, b) -> (galculator a) *. (galculator b)
        | DIV(a, b) -> (galculator a) /. (galculator b)
        | SIGMA(a, b, c) ->
           let f = (int_of_float (galculator a)) in
           let t = (int_of_float (galculator b)) in
           let e = (to_float_function c) in
           sigma(f, t, e)
        | INTEGRAL(a, b, c) ->
           let f = (galculator a) in
           let t = (galculator b) in
           let e = (to_float_function c) in
           if f > t then -1.0 *. integral(t, f, e)
           else integral(f, t, e)
