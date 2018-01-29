
  type exp =
      | X
      | INT of int
      | ADD of exp * exp
      | SUB of exp * exp
      | MUL of exp * exp
      | DIV of exp * exp
      | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
    = fun exp ->
      let rec xfilter x e =(*calculation with X-exp-, return exp*)
        match (x,e) with
          |(x,X) -> x

          |(x,INT(n)) -> INT(n)
          |(x,ADD(e1, e2)) -> ADD((xfilter x e1) , (xfilter x e2))
          |(x,SUB(e1, e2)) -> SUB((xfilter x e1) , (xfilter x e2))
          |(x,MUL(e1, e2)) -> MUL((xfilter x e1) , (xfilter x e2))
          |(x,DIV(e1, e2)) -> DIV((xfilter x e1) , (xfilter x e2))

          |(x,SIGMA(e1,e2,e3)) -> (*change SIGMA to ADD *)
              if e1 == e2 then (xfilter e1 e3) 
              else ADD((xfilter e1 e3),(SIGMA((ADD(e1, INT(1))),e2,e3)))

      in let rec sfilter e1 e2 e3 = (*change SIGMA to ADD, return exp *)
        if (calculator e1) = (calculator e2) then (xfilter e1 e3) 
        else ADD((xfilter e1 e3),(SIGMA( (ADD( INT(calculator e1),  INT(1)) ),e2,e3)))


      in match exp with (*formal calculation, return int*)
        |INT(n)-> n

        |ADD(e1, e2) -> ((calculator e1) + (calculator e2))
        |SUB(e1, e2) -> ((calculator e1) - (calculator e2))
        |MUL(e1, e2) -> ((calculator e1) * (calculator e2))
        |DIV(e1, e2) -> ((calculator e1) / (calculator e2))

        |SIGMA(e1,e2,e3) ->  calculator(sfilter e1 e2 e3)
        |X -> 0

