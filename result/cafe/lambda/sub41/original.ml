type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec findStation (x : lambda) : string list =
  match x with
  | V station -> [ station ]
  | C (a, b) -> findStation a @ findStation b
  | P (area, next) -> findStation next


let rec exclude (m : lambda) (strl : string list) : bool =
  match strl with
  | [] -> true
  | _ -> (
      match m with
      | V sta -> false
      | P (area, next) ->
          if List.mem area strl then
            exclude next (List.filter (fun (x : string) -> x != area) strl)
          else exclude next strl
      | C (V sta, con2) -> exclude con2 strl
      | C (con1, V sta) -> exclude con1 strl
      | C (con1, con2) ->
          exclude con1 (findStation con1) && exclude con2 (findStation con2) )


let rec check (x : lambda) : bool = exclude x (findStation x)
