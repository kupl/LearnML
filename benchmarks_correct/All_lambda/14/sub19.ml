type lambda = V of var
	     | P of var * lambda
	     | C of lambda * lambda
and var = string

let compareId (idStation: string) (idArea: string): bool = 
  if (idStation = idArea) then true
  else false

let rec checkArea (idList: string list) (m: lambda): bool = 
  match m with
  | V id -> List.exists (compareId id) idList
  | P (id, m1) -> (checkArea (id::idList) m1)
  | C (m1, m2) -> (checkArea idList m1) && (checkArea idList m2)

let rec check (m: lambda): bool = 
  match m with
  | V id -> false
  | P (id, m1) -> (checkArea [id] m1) 
  | C (m1, m2) -> (checkArea [] m1) && (checkArea [] m2)
