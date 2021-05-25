(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-13
  Homework-# : 2-4
  Excercise-Name : Check lambda map
*)

type lambda = V of var
          | P of var * lambda
          | C of lambda * lambda
and var = string

let check lambda = (
  let rec checkInContext (lambda, ctx) = (
    match lambda with
    | C (m1,m2) -> (checkInContext (m1, ctx)) && (checkInContext (m2,ctx))
    | P (var, m) -> (
      checkInContext(m, var::ctx)
    )
    | V var -> (
      List.mem var ctx
    )
  ) in checkInContext (lambda, [])
)

