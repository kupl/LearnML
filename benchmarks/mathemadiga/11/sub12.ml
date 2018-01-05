type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp 
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception Error

let rec mathemadiga exp = 
        match exp with
        
        |X -> raise Error

        |INT h -> float h
        |REAL h -> h
        
        |ADD (INT a, INT b) -> float (a + b)
        |ADD (REAL a, REAL b) -> (a +. b)
        |ADD (INT a, REAL b) -> (float a) +. b
        |ADD (REAL a, INT b) -> a +. (float b)
        |ADD (_, X) -> raise Error
        |ADD (X, _) -> raise Error

        |ADD (REAL a, expp) | ADD(expp, REAL a) -> a +. (mathemadiga expp)
        |ADD (INT a, expp) | ADD(expp, INT a) -> (float a) +. (mathemadiga expp)
        |ADD (expp, exppp) -> (mathemadiga expp) +. (mathemadiga exppp)

        |SUB (INT a, INT b) -> float (a - b)
        |SUB (REAL a, REAL b) -> (a -. b)
        |SUB (INT a, REAL b) -> (float a) -. b
        |SUB (REAL a, INT b) -> a -. (float b)
        |SUB (_, X) -> raise Error
        |SUB (X, _) -> raise Error

        |SUB (REAL a, expp) -> a -. (mathemadiga expp)
        |SUB (expp, REAL a) -> (mathemadiga expp) -. a
        |SUB (INT a, expp) -> (float a) -. (mathemadiga expp)
        |SUB (expp, INT a) -> (mathemadiga expp) -. (float a)
        |SUB (expp, exppp) -> (mathemadiga expp) -. (mathemadiga exppp)

        |MUL (INT a, INT b) -> float (a * b)
        |MUL (REAL a, REAL b) -> (a *. b)
        |MUL (INT a, REAL b) -> (float a) *. b
        |MUL (REAL a, INT b) -> a *. (float b)
        |MUL (_, X) -> raise Error
        |MUL (X, _) -> raise Error

        |MUL (REAL a, expp) | MUL(expp, REAL a) -> a *. (mathemadiga expp)
        |MUL (INT a, expp) | MUL(expp, INT a) -> (float a) *. (mathemadiga expp)
        |MUL (expp, exppp) -> (mathemadiga expp) *. (mathemadiga exppp)

        |DIV (INT a, INT b) -> (float a) /. (float b)
        |DIV (REAL a, REAL b) -> (a /. b)
        |DIV (INT a, REAL b) -> (float a) /. b
        |DIV (REAL a, INT b) -> a /. (float b)
        |DIV (_, X) -> raise Error
        |DIV (X, _) -> raise Error

        |DIV (REAL a, expp) -> a /. (mathemadiga expp)
        |DIV (expp, REAL a) -> (mathemadiga expp) /. a
        |DIV (INT a, expp) -> (float a) /. (mathemadiga expp)
        |DIV (expp, INT a) -> (mathemadiga expp) /. (float a)
        |DIV (expp, exppp) -> (mathemadiga expp) /. (mathemadiga exppp)

        |SIGMA(INT a, INT b, func) -> 
                        if a> b then raise Error
                        else 
                          (match (a, b, func) with
      
                          |(a, b, ADD(X, expp))
                          |(a, b, ADD(expp, X)) -> if a= b then mathemadiga (ADD (INT a, expp))
                                                   else 
                                                    mathemadiga (ADD (INT a, expp)) +. 
                                                    mathemadiga (SIGMA (INT (a + 1), INT b , expp))

                          |(a, b, SUB(X, expp)) -> if a= b then mathemadiga (SUB (INT a, expp))
                                                   else 
                                                    mathemadiga (SUB (INT a, expp)) +. 
                                                    mathemadiga (SIGMA (INT (a + 1), INT b , expp))

                          |(a, b, SUB(expp, X)) -> if a= b then mathemadiga (SUB (expp, INT a))
                                                   else 
                                                    mathemadiga (SUB (expp, INT a)) +. 
                                                    mathemadiga (SIGMA (INT (a + 1), INT b , expp))
                          
                          |(a, b, MUL(expp, X))                          
                          |(a, b, MUL(X, expp)) -> if a= b then mathemadiga (MUL (INT a, expp))
                                                   else 
                                                    mathemadiga (MUL (INT a, expp)) +. 
                                                    mathemadiga (SIGMA (INT (a + 1), INT b , expp))
                          

                          |(a, b, DIV(X, expp)) -> if a= b then mathemadiga (DIV (INT a, expp))
                                                   else 
                                                    mathemadiga (DIV (INT a, expp)) +. 
                                                    mathemadiga (SIGMA (INT (a + 1), INT b , expp))
(**)
                          |(a, b, DIV(expp, X)) -> if a= b then mathemadiga (DIV (expp, INT a))
                                                   else 
                                                    mathemadiga (DIV (expp, INT a)) +. 
                                                    mathemadiga (SIGMA (INT (a + 1), INT b , expp))
                                                    
                          |(a, b, X) -> if a= b then float a
                                        else ((((float b)*.((float b)+.1.0))-.((float a)*.((float a)-.1.0)))*.0.5)

                          |(a, b, REAL c) -> c *. ((float b) -. (float a) +. 1.0)

                          |(a, b, expp) -> mathemadiga (SIGMA(INT a,INT b, REAL (mathemadiga expp)))
                         

                                        )

         |SIGMA(REAL a, REAL b, func) -> 
                        if a> b then raise Error
                        else 
                          (match (a, b, func) with
                          |(a, b, ADD(X, expp))
                          |(a, b, ADD(expp, X)) -> if a= b then mathemadiga (ADD (REAL a, expp))
                                                   else 
                                                    mathemadiga (ADD (REAL a, expp)) +.
                                                    mathemadiga (SIGMA (REAL (a +. 1.0), REAL b , expp))

                          |(a, b, SUB(X, expp)) -> if a= b then mathemadiga (SUB (REAL a, expp))
                                                   else 
                                                    mathemadiga (SUB (REAL a, expp)) +. 
                                                    mathemadiga (SIGMA (REAL (a +. 1.0), REAL b , expp))

                          |(a, b, SUB(expp, X)) -> if a= b then mathemadiga (SUB (expp, REAL a))
                                                   else 
                                                    mathemadiga (SUB (expp, REAL a)) +. 
                                                    mathemadiga (SIGMA (REAL (a +. 1.0), REAL b , expp))
                          
                          |(a, b, MUL(expp, X))                          
                          |(a, b, MUL(X, expp)) -> if a= b then mathemadiga (MUL (REAL a, expp))
                                                   else 
                                                    mathemadiga (MUL (REAL a, expp)) +. 
                                                    mathemadiga (SIGMA (REAL (a +. 1.0), REAL b , expp))
                          

                          |(a, b, DIV(X, expp)) -> if a= b then mathemadiga (DIV (REAL a, expp))
                                                   else 
                                                    mathemadiga (DIV (REAL a, expp)) +. 
                                                    mathemadiga (SIGMA (REAL (a +. 1.0), REAL b , expp))

                          |(a, b, DIV(expp, X)) -> if a= b then mathemadiga (DIV (expp, REAL a))
                                                   else 
                                                    mathemadiga (DIV (expp, REAL a)) +. 
                                                    mathemadiga (SIGMA (REAL (a +. 1.0), REAL b , expp))
                                                    
                          |(a, b, X) -> if a= b then a
                                        else (((b *. (b +. 1.0)) -. (a *. (a -.1.0))) *. 0.5)

                          |(a, b, REAL c) -> c *. (b -. a +. 1.0)

                          |(a, b, expp) -> mathemadiga (SIGMA(REAL a, REAL b, REAL (mathemadiga expp)))

                          

                          )

         |SIGMA(expp, REAL b, func) -> mathemadiga(SIGMA (REAL (mathemadiga expp), REAL b, func))
         |SIGMA(REAL a, expp, func) -> mathemadiga(SIGMA (REAL a, REAL (mathemadiga expp), func))
         |SIGMA(expp, INT b, func) -> mathemadiga(SIGMA (REAL (mathemadiga expp), MUL (INT b, INT 1), func))
         |SIGMA(INT a, expp, func) -> mathemadiga(SIGMA (MUL (INT a, INT 1), REAL (mathemadiga expp), func))
         |SIGMA(expp, exppp, func) -> mathemadiga(SIGMA (REAL (mathemadiga expp), REAL (mathemadiga exppp), func))


         |SIGMA(X, _, _) |SIGMA(_, X, _) -> raise Error




         |INTEGRAL(REAL a, REAL b, func) ->
                         if a = b then 0.0
                         else if (a > b) then mathemadiga (SUB (REAL 0.0 , (INTEGRAL(REAL b, REAL a, func))))
                         else
                              (match (a, b, func) with
                              |(a, b, ADD(expp, X))
                              |(a, b, ADD(X, expp)) -> mathemadiga (ADD ((MUL ((REAL 0.1), (ADD (REAL a, expp))),
                                                                   (INTEGRAL (REAL (a +. 0.1), REAL b, expp)))))
                              
                              |(a, b, MUL(expp, X))
                              |(a, b, MUL(X, expp)) -> mathemadiga (ADD ((MUL ((REAL 0.1), (MUL (REAL a, expp))),
                                                                   (INTEGRAL (REAL (a +. 0.1), REAL b, expp)))))
                              
                              
                              |(a, b, SUB(X, expp)) ->  mathemadiga (ADD ((MUL ((REAL 0.1), (SUB (REAL a, expp))),
                                                                   (INTEGRAL (REAL (a +. 0.1), REAL b, expp)))))

                              |(a, b, SUB(expp, X)) ->  mathemadiga (ADD ((MUL ((REAL 0.1), (SUB (expp, REAL a))),
                                                                   (INTEGRAL (REAL (a +. 0.1), REAL b, expp)))))
                              
                              |(a, b, DIV(X, expp)) ->  mathemadiga (ADD ((MUL ((REAL 0.1), (DIV (REAL a, expp))),
                                                                   (INTEGRAL (REAL (a +. 0.1), REAL b, expp)))))

                              |(a, b, DIV(expp, X)) ->  mathemadiga (ADD ((MUL ((REAL 0.1), (DIV (expp, REAL a))),
                                                                   (INTEGRAL (REAL (a +. 0.1), REAL b, expp)))))
                              
                              |(a, b, X) -> 0.5 *. (a +. b) *. (b -. a)

                              |(a, b, REAL c) -> c *. (b -. a)

                              |(a, b, expp) -> mathemadiga (INTEGRAL(REAL a, REAL b, REAL (mathemadiga expp)))

                              )
                          
         |INTEGRAL(expp, REAL b, func) -> mathemadiga(INTEGRAL (REAL (mathemadiga expp), REAL b, func))
         |INTEGRAL(REAL a, expp, func) -> mathemadiga(INTEGRAL (REAL a, REAL (mathemadiga expp), func))
         |INTEGRAL(expp, exppp, func) -> mathemadiga(INTEGRAL (REAL (mathemadiga expp),
                                                               REAL (mathemadiga exppp), func))
         |INTEGRAL(X, _, _) |INTEGRAL(_, X, _) -> raise Error

