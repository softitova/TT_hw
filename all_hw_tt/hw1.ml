
(*---------------- PEANO ---------------*)

type peano = Z|S of peano;;

let rec add x y = match y with
	Z-> x
	|S yy -> S(add x yy);;

let rec sub x y = match (x, y) with 
	(xx, Z)-> xx
	|(Z, yy) -> Z
	|(S xx, S yy)-> sub xx yy;;

let rec mul x y = match (x, y)  with
	(xx, Z) -> Z
	|(Z, yy) -> Z
	|(xx, S yy) -> add (mul xx yy) (xx);;

let rec power x y = match (x, y) with
	(xx, Z) -> S Z
	|(Z, xx) -> Z
	|(S Z, xx) -> S Z
	|(xx, S yy) -> mul (power xx yy) (xx);;

let peano_of_int n =
	 let rec make_from_int n p_n =
	 	 match n with
		 	0 ->p_n
			| x -> S(make_from_int (x - 1) p_n) in
	  make_from_int n Z;;

let print_peano p_n =
	let rec make_from_peano p_n str =
		match p_n with
			Z -> "0"^str
			|S x -> make_from_peano x "'"^str in
	make_from_peano p_n "";;

let inc p_n = add(p_n)(S Z);;

let rec div x y = match sub (inc (x)) (y)  with
        Z -> Z
        |_ -> add (S Z) (div  (sub x y) (y));;


let rec int_of_peano p = match p with
       Z -> 0
       | S x -> 1 + int_of_peano x;;

let dec p_n = 
 	match p_n with
		Z -> Z
		|_ -> sub(p_n)(S Z);;


(*---------------- PARSERS ---------------*)

type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;


let lambda_of_string s =

        let s = s ^ ";" in
        let pos  = ref 0 in
        let get() = s.[!pos] in
        let next() = if !pos < String.length s - 1 then pos := !pos + 1 else failwith "oops" in
        let eat x  = if get() <> x then failwith "stop eating" else next() in

        let parse_ident_str()  =
                let rec  rec_parse s =
                        if (get() <> ' ' && get() <> '.' && get() <> ')' && (get()) <> ';') 
                        then let cur_dig = get() in next();
                        rec_parse  (s^ (String.make 1 (cur_dig))) else s in
                rec_parse "" in

        let parse_ident() =
                Var(parse_ident_str()) in

        let rec parse_abs() =
                eat '\\';
                let v = parse_ident_str () in
                eat '.';
                let l = parse_lambda() in
                Abs(v, l)

        and  parse res  =  if ((!pos =  String.length s - 1) || ')' = get())
        then res else (eat ' '; App(res, parse_lambda()))
        
        and parse_lambda() =
                match (get()) with
                '\\' -> (let res = parse_abs() in
                                parse res)
                |'(' -> (eat '(';
                        let res = parse_lambda() in
                        eat ')';
                        parse res)
                |_   ->  (let res = parse_ident() in
                                parse res) in
        parse_lambda();;

let string_of_lambda lambda =
        let rec to_string lambda s =
                match lambda with
                        | Var (x) -> s ^"("^x^")"
                        | Abs (x, y) -> s^"(\\"^x^"."^(to_string y "")^")"
                        | App (x, y) -> s^(to_string x "")^ " " ^(to_string y "") in
        to_string lambda "";;

(*------------------ REVERSE -----------------*)
type 'a my_list =
    | Cons of ('a * 'a my_list)
    | Nil

let rec rev_append l1 l2 = match l1 with
    | []   -> l2
    | h::t-> rev_append t (h::l2);;
     
let rev l = rev_append l [];;


(*---------------- MERGE SORT ---------------*)

let rec merge x y = match (x, y) with 
    |([], _) -> y
    |(_, []) -> x
    |(h1:: t1), (h2::t2) ->
    if ((<) h1 h2)
        then (h1::(merge t1 y))
    else (h2::(merge x t2));;

let rec split x y z = match x with
    | []-> (y,z)
    | x::other -> split (other) (z) (x::y);;
        
let rec merge_sort x = match x with
    | ([] | (_::[])) -> x
    | _ -> let (firstPart, secondPart) = split x [] [] in
    (merge (merge_sort firstPart) (merge_sort secondPart));;


let () = List.iter (printf "%d ") (merge_sort  [18;2;6;4;5]);;


(* type 'a my_list =
    | Cons of ('a * 'a my_list)
    | Nil

let rec rev_append l1 l2 = match l1 with
    | Nil   -> l2
    | Cons(l_head, l_tail)  -> rev_append l_tail (Cons(l_head, l2));;

let rec rev l = rev_append l Nil;;

let c = Cons(6, Cons(7,Nil));;

let rec print_list l str =
        match l with
        |Nil -> print_int str
        |Cons(x, y) -> print_list y  (str*10 + x) ;;

(*print_list c 0;;*)
let d = rev c;;
(*print_list d 0;;
*)

let rec merge x y = match (x, y) with
        |(Nil, _) -> y
        |(_, Nil) -> x
        |((Cons(h1, t1)), (Cons(h2, t2))) ->
        if ((<) h1 h2)
                then Cons(h1, merge t1 y)
        else Cons(h2, merge x t2);;

let rec split x y z = match x with
        | Nil -> (y,z)
        | Cons(x, other) -> split (other) (z) (Cons(x, y));;

let rec merge_sort x = match x with
        | (Nil | Cons(_, Nil)) -> x
        | _ -> let (firstPart, secondPart) = split x Nil Nil in
        (merge (merge_sort firstPart) (merge_sort secondPart));;


let m = merge_sort c;; *)
(* print_list m 0;;*)
