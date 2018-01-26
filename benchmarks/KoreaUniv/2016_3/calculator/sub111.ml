
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> match exp with
    | X -> raise(Failure "NotImplemented, Wrong input")
    | INT a -> a
    | ADD (f1, f2) -> calculator f1 + calculator f2
    | SUB (f1, f2) -> calculator f1 - calculator f2
    | MUL (f1, f2) -> calculator f1 * calculator f2
    | DIV (f1, f2) -> calculator f1 / calculator f2
    | SIGMA (f1, f2, f3) -> 
      let in1 = calculator f1 in
        let in2 = calculator f2 in
          if in1=in2 then
            begin
                  let rec calsig = fun x y ->
                    match x with
                    | X -> calsig y y
                    | INT n -> n
                    | ADD (a, b)->(calsig a y)+(calsig b y)
                    | SUB (a, b)->(calsig a y)-(calsig b y)
                    | MUL (a, b)->(calsig a y)*(calsig b y)
                    | DIV (a, b)->(calsig a y)/(calsig b y)
                    | SIGMA (a, b, c) -> calculator x in calsig f3 f2
            end
          else
            calculator (SIGMA (f2, f2, f3))+calculator (SIGMA (f1, SUB(f2, INT 1), f3));;
        