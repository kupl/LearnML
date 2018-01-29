exception Problem;;
(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec sigma (f, a, b) = (if a=b then (f a)
                          else if a<b then (f a) + sigma (f, a+1, b)
                        else raise Problem)

let rec calculator1 : exp -> int
= fun e -> match e with
  |X->0
  |INT n ->n
  |ADD(e1, e2)->calculator1(e1)+calculator1(e2)
  |SUB(e1, e2) -> calculator1(e1)-calculator1(e2)
  |MUL(e1, e2)-> calculator1(e1)*calculator1(e2)
  |DIV(e1, e2)->calculator1(e1)/calculator1(e2)
  |_-> raise Problem

let rec makefun ex =match ex with
|X-> (fun x->x)
|INT n -> (fun x->n)
|ADD(e1, e2)->(fun x->(((makefun e1)x)+((makefun e2)x)))
|SUB(e1, e2)->(fun x->(((makefun e1)x)-((makefun e2)x)))
|MUL(e1, e2)->(fun x->(((makefun e1)x)*((makefun e2)x)))
|DIV(e1, e2)->(fun x->(((makefun e1)x)/((makefun e2)x)))
|SIGMA(e1,e2,e3)->((fun x->(makefun(INT(sigma(makefun(e3), calculator1(e1), calculator1(e2)))))x))
|_->(fun x->0)

let rec calculator : exp -> int
= fun e -> match e with
  |X->0
  |INT n ->n
  |ADD(e1, e2)->calculator(e1)+calculator(e2)
  |SUB(e1, e2) -> calculator(e1)-calculator(e2)
  |MUL(e1, e2)-> calculator(e1)*calculator(e2)
  |DIV(e1, e2)->calculator(e1)/calculator(e2)
  |SIGMA(e1, e2, e3) ->(match e3 with
                        |_->sigma(makefun(e3), calculator(e1), calculator(e2)))
  |_-> raise Problem;;