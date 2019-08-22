(*컴퓨터공학부 2014-16775 김민지
programming language hw 2-4*)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec checkHelp (sl:string list) (m:lambda) : bool =
  match m with
  |V m1 -> if (List.mem m1 sl) then true else false
  |P (n, m1) -> checkHelp (n::sl) m1
  |C (m1, m2) -> (checkHelp sl m1) && (checkHelp sl m2)

let check (x:lambda) : bool = 
  checkHelp [] x

