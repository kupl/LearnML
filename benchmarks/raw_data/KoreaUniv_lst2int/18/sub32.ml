let rec lst2int : int list -> int
= fun lst -> 
  match lst with
    |[] -> 0
    |[n] -> n
    |head :: tail ->
      let rec find_len l =
        match l with
          |[] -> 1
          |head :: tail -> 10 * find_len tail
      in head * find_len tail + lst2int tail;;
