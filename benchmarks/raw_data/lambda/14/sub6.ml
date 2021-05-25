type lambda =
  V of var
| P of var * lambda
| C of lambda * lambda
and var =
  string

let check(lambda: lambda): bool =
  let rec checkInternal((lambda:lambda), (validNames: var list)) =
    match lambda with
      V(id) ->
        List.mem id validNames
    | P(id, lambda) ->
        checkInternal(lambda, id :: validNames)
    | C(lambda1, lambda2) ->
        checkInternal(lambda1, validNames) &&
        checkInternal(lambda2, validNames)
  in checkInternal(lambda, [])
