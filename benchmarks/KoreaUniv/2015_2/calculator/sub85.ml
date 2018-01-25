exception FreeVariable

type exp = X
    | INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp

let rec calc l x = match l with
    | X ->
      (match x with
      | None -> raise FreeVariable
      | Some s -> s)
    | INT i -> i
    | ADD (a, b) -> (calc a x) + (calc b x)
    | SUB (a, b) -> (calc a x) - (calc b x)
    | MUL (a, b) -> (calc a x) * (calc b x)
    | DIV (a, b) -> (calc a x) / (calc b x)
    | SIGMA (a, b, c) -> 
      let y1 = calc a x in
      let y2 = calc b x in
      let rec sigma i res =
        if i > y2 then res else 
	  let v = calc c (Some i) in
          sigma (i + 1) (v + res) in
      	  sigma y1 0

let calculator l = calc l None;;






