(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec subm
  = fun m ->
    let bw
    = fun b ->
      match b with
      | SimpleBranch(l,w) -> l*w
      | CompoundBranch(l,m) -> l*(subm m)
    in
    let rec w
    = fun b ->
      match b with
      | SimpleBranch(l,w) -> w
      | CompoundBranch(l,m) ->
        match m with
        | (l, r) -> (w l) + (w r)
    in
    match m with
    | (a, b) ->
      if (bw a) != (bw b) then 0
      else if (w a) = 0 then 0
      else if (w b) = 0 then 0
      else (w a)+(w b)
  in
  ((subm m) != 0)
;;
