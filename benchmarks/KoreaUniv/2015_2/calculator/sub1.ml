type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calf f x =
  match f with 
  |X -> (match x with 
    |INT (n) -> n  
    |_ -> raise (Failure "argument must be INT"))
  |INT n -> n 
  |ADD (a,b)-> (calf a x) + (calf b x)
  |SUB (a,b)-> (calf a x) - (calf b x)
  |MUL (a,b)-> (calf a x) * (calf b x)
  |DIV (a,b)-> (calf a x) / (calf b x)
(*sigma함수를 돕기위한 보조함수, 먼저 선언해주었음. 시그마안에 있는 함수를 f(x)라고 할 때 그 함숫값을 계산해주는 함수이다.*)
  let rec sigma (a,b,c)=
    if (calf a a)(*이렇게 표현하면 calf함수 내에서 INT가 없는 int형으로 반환 가능*)<(calf b b) then (calf c a)+sigma(INT((calf a a)+1),b,c)
    else (calf a c)
(* calculator의 SIGMA를 계산해주는 내용 함수. *)
let rec calculator op= 
  match op with
  | INT (num) -> num
  | ADD (a,b) -> calculator(a) + calculator (b)
  | SUB (a,b) -> calculator(a) - calculator (b)
  | MUL (a,b) -> calculator(a) * calculator (b)
  | DIV (a,b) -> calculator(a) / calculator (b)
  | SIGMA (a,b,c) -> 
    (match c with 
    |SIGMA(q,w,e)-> (calculator(b)-calculator(a)+1)*sigma(q,w,e)
    |_->sigma(a,b,c)
  )







