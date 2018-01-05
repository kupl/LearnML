(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 5: galculator
 * Written by Dongho Kang 
 *)

type exp = X
 | INT of int
 | REAL of float
 | ADD of exp * exp
 | SUB of exp * exp
 | MUL of exp * exp
 | DIV of exp * exp
 | SIGMA of exp * exp * exp
 | INTEGRAL of exp * exp * exp
 ;;

 exception FreeVariable (* exception for unpaired X *)
 
 let rec galculator: exp -> float = fun e -> 

     let rec sigma: int * int * exp -> float = fun (i_st, i_en, e) -> 
         if i_st > i_en then 0.0
         else
             galculator_with_x (e, float_of_int i_st, true) +. sigma (i_st + 1, i_en, e)
     
     and integral: float * float * exp -> float = fun (i_st, i_en, e) ->
         if i_en -. i_st < 0.1 then 0.0
         else
             ((galculator_with_x (e, i_st, true)) *. 0.1) +. (integral (i_st +. 0.1, i_en, e))
    
     and galculator_with_x: exp * float * bool -> float = fun (e, x, b) ->
         match e with
         | X -> 
                 if b then x
                 else raise FreeVariable
         | INT  num -> float_of_int num 
         | REAL num -> num
         | ADD  (exp1, exp2) -> (galculator_with_x (exp1, x, b)) +. (galculator_with_x (exp2, x, b))
         | SUB  (exp1, exp2) -> (galculator_with_x (exp1, x, b)) -. (galculator_with_x (exp2, x, b))
         | MUL  (exp1, exp2) -> (galculator_with_x (exp1, x, b)) *. (galculator_with_x (exp2, x, b))
         | DIV  (exp1, exp2) -> (galculator_with_x (exp1, x, b)) /. (galculator_with_x (exp2, x, b))
         | SIGMA (exp1, exp2, exp3) -> 
             let i_st = int_of_float (galculator_with_x (exp1, x, b)) in
             let i_en = int_of_float (galculator_with_x (exp2, x, b)) in 
             sigma (i_st, i_en, exp3)
         | INTEGRAL (exp1, exp2, exp3) ->
             let i_st = galculator_with_x (exp1, x, b) in 
             let i_en = galculator_with_x (exp2, x, b) in 

             if i_st > i_en then -. (integral (i_en, i_st, exp3))
             else integral (i_st, i_en, exp3)
     in

     galculator_with_x (e, 0.0, false)
 ;;
