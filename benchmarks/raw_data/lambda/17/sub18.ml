type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
            and var = string

let check (m: lambda) : bool =
  let rec op ((l: string list), (a: lambda)) =
    match a with
    | V n -> List.exists (fun x -> x = n) l
    | P (n, a') -> op((List.append [n] l), a')
    | C (a', a'') -> (op (l, a')) && op (l, a'')
  in 
  op ([], m)