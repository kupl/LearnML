type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception CANTSOL of string

let rec mathemadiga ex =
        let rec solX (a ,f, e) =
                match e with
                        X -> if a=1 then f
                                else raise(CANTSOL "I can't solve this problem.")
                        |INT x -> float_of_int x
                        |REAL x -> x
                        |ADD(x1,x2) -> (solX (a,f,x1)) +. (solX (a,f,x2))
                        |SUB(x1,x2) -> (solX (a,f,x1)) -. (solX (a,f,x2))
                        |MUL(x1,x2) -> (solX (a,f,x1)) *. (solX (a,f,x2))
                        |DIV(x1,x2) -> if (solX (a,f,x2)) = 0.0 then raise(CANTSOL "I can't solve this problem.")
                                                else (solX (a,f,x1)) /. (solX (a,f,x2))
                        |SIGMA(x1,x2,x3) -> let si = solX(a,f,x1) in
                                            let sn = int_of_float (solX(a,f,x1)) in
                                            let fi = solX(a,f,x2) in
                                            let fn = int_of_float (solX(a,f,x2)) in
                                            if si > fi then 0.0
                                                else if sn = fn then solX(1,(float_of_int sn),x3)
                                                        else solX(1,(float_of_int sn),x3) +. solX(1,f,SIGMA(INT (sn+1),x2,x3))
                        |INTEGRAL(x1,x2,x3) -> let diff = solX(a,f,(SUB(x2,x1))) in
                                               if diff < 0.0 then solX(1,f,INTEGRAL(x2,x1,x3)) *. -1.0
                                                        else if diff < 0.1 then 0.0
                                                                else if diff = 0.0 then 0.0
                                                                        else (0.1 *. solX(1,solX(1,f,x1),x3)) +. solX(1,f,INTEGRAL(REAL(solX(1,f,x1)+.0.1),x2,x3))


        in
        solX(0,1.0,ex)