type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check (m : lambda) : bool =
  let rec loop area lambda =
    match lambda with
    | V var -> List.exists (fun x -> x = var) area
    | P (var, lambda') -> loop (var :: area) lambda'
    | C (lambda1, lambda2) -> (loop area lambda1) && (loop area lambda2) in
  loop [] m

