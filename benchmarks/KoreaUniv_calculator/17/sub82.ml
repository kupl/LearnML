(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
    match e with
        X -> raise(Failure "wrong")
        | INT x -> x
        | ADD (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1+v2)
        | SUB (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1-v2)
        | MUL (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1*v2)
        | DIV (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1/v2)
        | SIGMA (e1, e2, exp) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
            if v1 = v2 then (
                let rec asdf = fun a b -> (
                match a with
                    X -> asdf b b
                    | INT x -> x
                    | ADD (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1+y2)
                    | SUB (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1-y2)
                    | MUL (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1*y2)
                    | DIV (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1/y2)
                    | SIGMA (x1, x2, expr) -> calculator a
                )
                in asdf exp e2
            )
            else calculator (SIGMA (ADD(e1, INT 1), e2, exp)) + calculator (SIGMA(e1, e1, exp))
;;