type peano = Z | S of peano

val print_peano: peano -> string
val peano_of_int: int -> peano
val int_of_peano: peano -> int
val div: peano -> peano ->peano
val inc: peano -> peano
val dec: peano -> peano
val add: peano -> peano -> peano
val sub: peano -> peano -> peano
val mul: peano -> peano -> peano
val power: peano -> peano -> peano



type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

val string_of_lambda: lambda -> string
val lambda_of_string: string -> lambda


type 'a my_list =
                | Cons of ('a * 'a my_list)
                | Nil


val rev: 'a my_list -> 'a my_list
val merge_sort: 'a my_list -> 'a my_list
