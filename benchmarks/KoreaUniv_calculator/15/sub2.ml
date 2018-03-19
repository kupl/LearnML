type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal_x : exp * exp -> int  (*뒤의 exp에 n을 대입해주는 역할*)
=fun (n, exp) ->
  match exp with
  |X -> cal_x(n, n)
  |INT a -> a
  |ADD(a, b) -> cal_x(n, a) + cal_x(n, b)
  |SUB(a, b) -> cal_x(n, a) - cal_x(n, b)
  |MUL(a, b) -> cal_x(n, a) * cal_x(n, b)
  |DIV(a, b) -> cal_x(n, a) / cal_x(n, b)


let rec sigma : exp * exp * exp -> int
=fun (a, b, f) ->
  if cal_x(a, a) < cal_x(b, b) then cal_x(a, f) + sigma(INT(cal_x(a,a) + 1), b, f)
  else cal_x(a, f)



let rec cal : exp -> int 
=fun exp ->
  match exp with
  |INT n -> n
  |ADD(a, b) -> (cal a) + (cal b)
  |SUB(a, b) -> (cal a) - (cal b)
  |MUL(a, b) -> (cal a) * (cal b)
  |DIV(a, b) -> (cal a) / (cal b)
  |SIGMA(a, b, e) -> 
    (match e with
      |SIGMA(x, y, z) -> (cal x - cal y + 1) * sigma(x, y, z)
      |_ -> sigma(a, b, e)
    )


let calculator : exp -> int
=fun e -> cal e ;;