type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and 
  var = string
(*
let rec check (m: lambda) : bool =
  match m with
  | V n -> true
  | C (m1, m2) -> (check m1) && (check m2)
  | P (n, m) -> 
      ( match m with
      | V n2 -> (if (n=n2) then true else false)
      | _ -> (check m)
      )
*)
let rec checkMemory (m, mem): bool =
  match mem with 
  | [] -> false
  | hd::tl -> 
      if (hd=m)
      then true
      else checkMemory(m, tl)

let rec check_wMem (m, mem): bool=
  match m with
  | V n -> checkMemory(n, mem)
  | C (m1, m2)-> (check_wMem (m1, mem)) && (check_wMem (m2, mem))
  | P (n, m1) -> check_wMem (m1, (n::mem))

let check (m: lambda) : bool=
  check_wMem(m, [])
