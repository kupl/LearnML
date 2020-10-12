type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec checklambdalist ((m: lambda), (l: string list)) : bool =
     match m with
     | V x -> List.mem x l
     | P (a, m') -> checklambdalist(m', l@[a])
     | C (x, y) -> (checklambdalist(x, l) && checklambdalist(y, l))

let check (m: lambda) : bool =
     checklambdalist(m, [])
