type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

module Env = Set.Make(String)

let rec isInclude: metro * Env.t -> bool = fun (m, env) ->
    match m with
    | STATION a -> Env.mem a env
    | AREA (a, b) -> begin
        let new_env = Env.add a env in
        isInclude (b, new_env)
    end
    | CONNECT (a, b) -> (isInclude (a, env)) && (isInclude (b, env))

let checkMetro: metro -> bool = fun x -> isInclude (x, Env.empty)
