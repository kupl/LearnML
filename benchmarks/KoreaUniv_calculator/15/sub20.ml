type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calx : exp * exp -> int
=fun (i,e) ->(
  match e with
  | X -> calx(i,i)
  | INT(n) -> n
  | ADD(q,w) -> calx(i,q)+calx(i,w)
  | SUB(q,w) -> calx(i,q)-calx(i,w)
  | MUL(q,w) -> calx(i,q)*calx(i,w)
  | DIV(q,w) -> calx(i,q)/calx(i,w))

let rec sigma : exp*exp*exp -> int
=fun (q,w,r) -> (
  if calx(q,q)<calx(w,w) then calx(q,r)+sigma(INT(calx(q,q)+1),w,r)
  else calx(q,r))

let rec cal e =
  match e with
  | INT(n) -> n
  | ADD(q,w) -> cal(q)+cal(w)
  | SUB(q,w) -> cal(q)-cal(w)
  | MUL(q,w) -> cal(q)*cal(w)
  | DIV(q,w) -> cal(q)/cal(w)
  | SIGMA(q,w,r) -> match r with |SIGMA(a,s,d) -> (cal(w)-cal(q)+1)*sigma(a,s,d) | _ -> sigma(q,w,r)

let calculator e = cal(e)