type peano = Z | S of peano

(*val print_peano: peano -> string*)
val peano_of_int: int -> peano
val int_of_peano: peano -> int
(*val div: peano -> peano ->peano*)
val inc: peano -> peano
(*val dec: peano -> peano*)
val add: peano -> peano -> peano
val sub: peano -> peano -> peano
val mul: peano -> peano -> peano
val power: peano -> peano -> peano
