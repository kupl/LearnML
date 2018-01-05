(*컴퓨터공학부 2014-16775 김민지
programming language hw 2-4*)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkHelp (sl:string list) (m:metro) : bool =
  match m with
  |STATION m1 -> if (List.mem m1 sl) then true else false
  |AREA (n, m1) -> checkHelp (n::sl) m1
  |CONNECT (m1, m2) -> (checkHelp sl m1) && (checkHelp sl m2)

let checkMetro (x:metro) : bool = 
  checkHelp [] x

