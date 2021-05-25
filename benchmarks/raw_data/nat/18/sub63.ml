type nat = ZERO | SUCC of nat

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;


two;;