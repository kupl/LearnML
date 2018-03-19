(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec another e l =
match e with
|X-> (match l with
      |[n]->n
      |_-> raise (NOANSWER))
|INT n -> n
|ADD(x,y) -> another x l + another y l
|SUB(x,y) -> another x l - another y l
|MUL(x,y) -> another x l * another y l
|DIV(x,y) -> another x l / another y l

|SIGMA(x,y,exp) -> let v1 = another x l in
                    let v2 = another y l in
                      let rec keep v1 v2 exp l =
                        if v1 == v2 then another exp [v2]
                        else
                        another exp [v1] + keep((v1)+1) v2 exp l
                        in keep v1 v2 exp l

let calculator :exp ->int
=fun e->
another e []
