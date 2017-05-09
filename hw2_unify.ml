type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(*val system_to_equation: (algebraic_term * algebraic_term) list -> (algebraic_term * algebraic_term)
*)

let rec list_to_string x str = match x with
         |[]-> str
         |h::t -> type_to_string h (" " ^  (list_to_string t str))



and type_to_string x str = match x with 
        |Var a -> a ^ " " ^ str
        |Fun(a, b) -> "(" ^ a ^ " " ^ (list_to_string b str) ^ ")" ;;


let rec system_to_string x str = match x with
        |[] -> str
        |(l,r)::t -> type_to_string l "" ^ " = " ^  type_to_string r "" ^ "\n" ^
        system_to_string t str;;




let rec go x str = match x with
        |[]-> str
        |h::t -> simple_name h (go t str) 

and  simple_name x str = match x with
        |Var a -> str ^ a
        |Fun (a, h) -> a ^ (go h str) ;;

let rec  get_name x str = match x with
        |[]-> str       
        |(l,r)::t -> simple_name l (simple_name r (get_name t str));;  
(*let system_to_equation x =




let apply_substitution x y = failwith "Not implemented";;
let check_solution x y = failwith "Not implemented";;
let solve_system x = failwith "Not implemented";;*)
