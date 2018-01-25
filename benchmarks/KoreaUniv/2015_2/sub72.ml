(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
  match lst with
      [] -> []
    | h::t -> if (pred h) then h::(filter pred t) else (filter pred t);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
  = fun (a,b) -> 
    match a with
        [] -> b
      | h::t -> h::
                  (match b with
                      [] -> t
                    | h2::t2 -> h2::(zipper (t,t2)));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
  = fun (n,f) -> 
    if n>0
    then (fun x -> (let f2 = iter (n-1,f) in f2(f x)))
    else fun x -> x;;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
    | Const of int 
    | Var of string 
    | Power of string * int 
    | Times of aexp list
    | Sum of aexp list

let rec smrz : aexp -> aexp
  = fun aexp ->
    match aexp with
        Times(l) -> (match l with
                        [e] -> e
                      | e::Times(l)::t -> smrz(Times(e::(l@t)))
                      | Const a::Const b::t -> smrz(Times(Const(a*b)::t))
                      | Var(v)::Var(v2)::t -> 
                          if v=v2 
                          then smrz(Times(Power(v, 2)::t))
                          else Times(Var(v)::Var(v2)::t)
                      | Var(v)::Power(v2, i)::t -> 
                          if v=v2 
                          then smrz(Times(Power(v, i+1)::t))
                          else Times(Var(v)::Power(v2, i)::t)
                      | Power(v2, i)::Var(v)::t -> 
                          if v=v2 
                          then smrz(Times(Power(v, i+1)::t))
                          else Times(Var(v)::Power(v2, i)::t)
                      | Power(v, i)::t -> 
                          if i=0
                          then smrz(Times(t))
                          else if i=1
                          then smrz(Times(Var(v)::t))
                          else Times(Power(v, i)::t)
                      | Const a::t -> 
                          if a=0 
                          then Const 0 
                          else Times(Const a::t)
                      | h::t -> Times(smrz(h)::smrz(Times(t))::[])
                      | _ -> Times(l)
                    )
      | Sum(l) -> (match l with
                      [e] -> e
                    | Const 0::t -> smrz(Sum(t))
                    | e::Sum(l)::t -> smrz(Sum(e::(l@t)))
                    | Const a::Const b::t -> smrz(Sum(Const(a+b)::t))
                    | Var(v)::Var(v2)::t -> 
                        if v=v2 
                        then smrz(Sum(Const 2::Var(v)::t))
                        else Sum(l)
                    | h::t -> Sum(smrz(h)::smrz(Sum(t))::[])
                    | _ -> Sum(l)
                  )
      | _ -> aexp
;;

let rec diff : aexp * string -> aexp
  = fun (aexp,x) -> 
    match aexp with
        Const(i) -> Const 0
      | Var(v) -> if v=x then Const 1 else Var(v)
      | Power(v, i) -> if v=x then
            (if i=2 then Times[Const 2; Var(v)] else 
             if i=1 then Const 1 else Times[Const i; Power(v, i-1)])
          else Power(v, i)
      | Times(l) -> let ll = smrz(Times(l)) in
            (match ll with
                Times(Const a::Var(v)::t) -> 
                  if v=x
                  then smrz(Times(Const a::t))
                  else Times(Const a::Var(v)::t)
              | Times(Const a::Power(v, i)::t) -> 
                  if v=x 
                  then smrz(Times(Const(a)::smrz(Times(diff(smrz(Power(v, i)), x)::t))::[]))
                  else Times(Const a::Power(v, i)::t)
              | Times([e]) -> diff(e, x)
              | _ -> ll
            )

      | Sum(l) -> let ll = smrz(Sum(l)) in
            (match ll with
              | Sum([e]) -> smrz(diff(e, x))
              | Sum(h::t) -> smrz(Sum(smrz(diff(h, x))::diff(Sum(t), x)::[]))
              | e -> smrz(diff(e, x))
            )
;;


(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec eval : exp -> int -> int
  = fun e i ->
    match e with
        X -> i
      | INT(a) -> a
      | ADD(a, b) -> eval a i + eval b i
      | SUB(a, b) -> eval a i - eval b i
      | MUL(a, b) -> eval a i * eval b i
      | DIV(a, b) -> eval a i / eval b i
      | SIGMA(a, b, c) -> 
          let aa = eval a 0 in
            if aa <= eval b 0
            then eval (SIGMA(INT(aa+1), b, c)) 0 + eval c aa
            else 0
      | _ -> raise(Failure "IDK")
;;


let rec calculator : exp -> int
  = fun e -> 
    match e with
        X -> raise(Failure "Variable Only")
      | _ -> eval e 0
;;



























