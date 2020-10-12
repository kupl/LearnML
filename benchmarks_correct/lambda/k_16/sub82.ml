
      type lambda =
            | V of var
              | P of var * lambda
                | C of lambda * lambda
                  and var = string

                    let check : lambda -> bool
                      = fun lambda ->
                            let rec find = fun e env ->
                                  match e with
                                    | V n ->
                                                      begin
                                                                    match env with
                                                                              | [] -> false
                                                                                        | hd::tl -> if (n = hd) then true else (find e tl)
                                                                                                  end
                                                        | P (v, f) -> find f ([v] @ env)
                                                          | C (f1, f2) -> if (find f1 env) && (find f2 env) then true else false
                                                            in find lambda []
