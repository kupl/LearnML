type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let rec checkMetroHelper = (fun (env,inputMetro) ->
  match inputMetro with
  | STATION a ->
    if List.mem a env then true
    else false
  | CONNECT(a,b) -> checkMetroHelper (env,a) && checkMetroHelper (env,b)
  | AREA(a,b) ->
    let newEnv = a::env in
    checkMetroHelper(newEnv, b)
  )

let rec checkMetro = (fun x ->
  checkMetroHelper ([],x)
  )
