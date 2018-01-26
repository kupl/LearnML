(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec bindec = fun b acc ->
    match b with
    | [] -> acc
    | ZERO :: rest -> bindec rest (acc * 2)
    | ONE :: rest -> bindec rest (acc * 2 + 1)
  in
  let decbin = fun d ->
    let reverse = fun l ->
      let rec reverse_loop = fun acc ll ->
        match ll with
        | [] -> acc
        | hd :: tl -> reverse_loop (hd :: acc) tl
      in
      reverse_loop [] l
    in
    let rec decbin_loop = fun d ->
      if d = 0 then
        []
      else
        match d mod 2 with
        | 0 -> ZERO :: decbin_loop (d / 2)
        | _ -> ONE :: decbin_loop (d / 2)
    in
    let result = reverse (decbin_loop d)
    in match result with
    | [] -> [ZERO]
    | result -> result
  in
  decbin ((bindec b1 0) * (bindec b2 0))
