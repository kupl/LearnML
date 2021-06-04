type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check input =
  let rec helpCheck a lst =
    match a with
    | V a -> List.exists (fun a -> a = a) lst
    | P (a, b) -> helpCheck b (a :: lst)
    | C (a, b) -> helpCheck a lst && helpCheck b lst
  in
  helpCheck input []
