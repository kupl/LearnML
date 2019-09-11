type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec checkHelper = (fun (env,inputMetro) ->
  match inputMetro with
  | V a ->
    if List.mem a env then true
    else false
  | C(a,b) -> checkHelper (env,a) && checkHelper (env,b)
  | P(a,b) ->
    let newEnv = a::env in
    checkHelper(newEnv, b)
  )

let rec check = (fun x ->
  checkHelper ([],x)
  )
