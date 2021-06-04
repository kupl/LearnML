type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (input : lambda) : bool =
  let rec helpCheck (a : lambda) (lst : string list) : bool =
    match a with
    | V a -> List.exists (fun (__s7 : string) -> __s7 = a) lst
    | P (a, b) -> helpCheck b (a :: lst)
    | C (a, b) -> helpCheck a lst && helpCheck b lst
  in
  helpCheck input []
