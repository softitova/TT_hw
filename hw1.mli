type peano = Z | S of peano


val int_of_peano: peano -> int

val inc: peano -> peano
val add: peano -> peano -> peano
val sub: peano -> peano -> peano
val mul: peano -> peano -> peano
val power: peano -> peano -> peano
