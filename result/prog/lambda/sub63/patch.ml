type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec ptree ((e1 : string), (e2 : lambda)) : bool =
  match e2 with
  | P (x, y) -> (
      match y with
      | P (a, b) -> ptree (a, b) || ptree (x, b) || ptree (e1, b)
      | C (a, b) ->
          (ptree (x, a) || ptree (x, b)) && (ptree (e1, a) || ptree (e1, b))
      | V a -> ptree (x, y) || ptree (e1, y) )
  | C (x, y) -> ptree (e1, x) && ptree (e1, y)
  | V x -> e1 = x


let rec ctree ((e1 : lambda), (e2 : lambda)) : bool =
  match (e1, e2) with
  | P (x1, y1), P (x2, y2) -> ptree (x1, y1) && ptree (x2, y2)
  | C (x1, y1), P (x2, y2) -> ctree (x1, y1) && ptree (x2, y2)
  | P (x1, y1), C (x2, y2) -> ptree (x1, y1) && ctree (x2, y2)
  | C (x1, y1), C (x2, y2) -> ctree (x1, y1) && ctree (x2, y2)
  | V x, _ -> false
  | _, V y -> false


let rec __s3 ((__s4 : lambda), (__s5 : string list)) : bool =
  match __s4 with
  | V __s11 -> List.mem __s11 __s5
  | C (__s12, __s13) -> __s3 (__s12, __s5) && __s3 (__s13, __s5)
  | P (__s14, __s15) -> __s3 (__s15, List.append __s5 [ __s14 ])


let rec check (e : lambda) : bool =
  match e with
  | P (x, y) -> __s3 (e, [])
  | C (x, y) -> ctree (x, y)
  | V x -> false
