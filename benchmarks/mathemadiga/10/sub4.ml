(* CSE/ 2004-11920 / Yeseong Kim/ Prob 2*)

exception Error of string
exception FreevarError 
exception DividedByZero 

type exp =
  X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let mathemadiga expr =
        let checkZero z =
                if (z = 0.) then raise DividedByZero
                else z
        in
        let rec general expr b x = (* b = boolean of Using X, x = VALUE_OF_X *)
                match expr with
                        ADD(e1,e2) -> (general e1 b x) +. (general e2 b x)
                |       SUB(e1,e2) -> (general e1 b x) -. (general e2 b x)
                |       MUL(e1,e2) -> (general e1 b x) *. (general e2 b x)
                |       DIV(e1,e2) -> (general e1 b x) /. (checkZero (general e2 b x))
                |       SIGMA(e1,e2,e3) -> (sigma_eval_pipe1 e1 e2 e3 b x)
                |       INTEGRAL(e1,e2,e3) -> (integral_eval_pipe1 e1 e2 e3 b x)
                |       INT(n) -> (float_of_int n) (*Casting*)
                |       REAL(n) -> n
                |       X       -> if (b = false)  then raise FreevarError
                                                   else x
        and
        (* SIGMA PART ////////////////////////////////////////////////////////////////*)
        (*let*) sigma_eval_pipe1 e1 e2 e3 b x = 
                match e1 with
                        INT(n1) -> (sigma_eval_pipe2 n1 e2 e3 b x)
                |       _ -> raise (Error "Invalid SIGMA arg1")
        and
        (*let*) sigma_eval_pipe2 n1 e2 e3 b x =
                match e2 with
                        INT(n2) -> (sigma_eval_pipe3 n1 n2 e3)
                |       _ -> raise (Error "Invalid SIGMA arg2")
        and
        (*let*) sigma_eval_pipe3 n1 n2 e3 =
                if (n1 > n2) then raise (Error "Invalid SIGMA argument A > B")
                else (sigma_do 1 n2 n1 e3)
        and
        (*let rec*) sigma_do n1 n2 nowX expr=
                if (nowX > n2)  then 0.
                else (general expr true (float_of_int nowX)) +. (sigma_do n1 n2 (nowX+1) expr)
        and
        (* END OF SIGMA //////////////////////////////////////////////////////////////*)
        (* INTEGRAL PART /////////////////////////////////////////////////////////////*)
        (*let*) integral_eval_pipe1 e1 e2 e3 b x =
                (integral_eval_pipe2 (general e1 b x) e2 e3 b x)
        and
        (*let*) integral_eval_pipe2 n1 e2 e3 b x =
                (integral_eval_pipe3 n1 (general e2 b x) e3)
        and
        (*let*) integral_eval_pipe3 n1 n2 e3 =
                if (n1 <= n2) then (integral_do n1 n2 n1 e3)
                else ((integral_do n2 n1 n2 e3) *. -1.0)
        and
        (*let rec*) integral_do n1 n2 nowX expr =
                if (nowX >= n2)  then 0.0
                else ((general expr true nowX) *. (min 0.1 (n2 -.nowX))) +. (integral_do n1 n2 (nowX +. 0.1) expr)
        in
        (* END OF INTEGRAL ///////////////////////////////////////////////////////////*)
        (general expr false 0.)
