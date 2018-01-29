(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin =
  fun b1 b2 ->
    let rec reverse lst =
      match lst with
      [] -> []
      | hd :: tl -> (reverse tl) @ [hd]
    in
      let rec btd binary pro = 
        match binary with
        [] -> 0
        | [ZERO] -> 0
        | [ONE] -> 1 * pro
        | hd :: tl -> ((btd [hd] 1) * pro) + (btd tl (pro * 2))
      in
        let rec dtb decimal sum =
          if decimal > 0 then
            if decimal mod 2 = 0 then (dtb (decimal / 2) (sum @ [ZERO]))
            else (dtb (decimal / 2) (sum @ [ONE]))
          else sum
        in
          let v1 = (reverse b1) in let v2 = (reverse b2) in
            let decimal_mul = (btd v1 1) * (btd v2 1) in
              (reverse (dtb decimal_mul []));;