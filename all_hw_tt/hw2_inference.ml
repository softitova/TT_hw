(*val infer_simp_type : lambda -> ((string * simp_type list) * simp_type) option
*)

open Hw1
type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type
let counter = ref 0 ;;

let name_generator () =
        let ret = "t"^ string_of_int !counter in                                              
        counter := !counter + 1; ret;;


let st_to_string st = 
	let rec impl st str = 
		match st with 
			S_Elem v -> str ^ v
			| S_Arrow (x, y) -> str ^ (impl x "") ^ " -> " ^ (impl y "") in
	impl st "";;


module Mmap = Map.Make (String);;

let rec stat x = match x with
        |S_Elem a -> Hw2_unify.Var a
        |S_Arrow (a, b) -> (Hw2_unify.Fun ("impl", [stat a; stat b]));;

let stas sys = 
        List.map (fun(a, b) -> (stat a, stat b)) sys;;

let [@warning "-8"] rec atst x = match x with 
        |Hw2_unify.Var a -> S_Elem a
        |(Hw2_unify.Fun (a, [b; c])) -> S_Arrow (atst b, atst c);;


let atss sys = 
        List.map (fun(a, b) -> (atst a, atst b)) sys;;

let sas_to_sss sys = 
        List.map (fun(a, b) -> (a, atst b)) sys;;


let list_to_map l =
        let rec helper l map = match l with
                |[] -> map
                |h::t -> helper t (Mmap.add h (S_Elem(name_generator())) map) in
        helper l Mmap.empty;;
        
        
let rec in_simp_helper xx map = match xx with
        |Var(x)    -> 
                        ([], Mmap.find x map)
        |App(x, y) ->
                        let sys1, type_t1 = in_simp_helper x map in
                        let sys2, type_t2 = in_simp_helper y map in
                        let tt = S_Elem(name_generator()) in
                        ((List.append sys1
                                (List.append sys2
                                        [(type_t1, S_Arrow (type_t2, tt))])), tt) 
        |Abs(x, y) -> 
                        let t = (Mmap.add x (S_Elem(name_generator())) map) in
                        let sys1, type_t1 = in_simp_helper y t in
                        (sys1, S_Arrow(Mmap.find x t, type_t1));;



let infer_simp_type x = 
        let sys, type_t =  in_simp_helper x (list_to_map
        (Hw1_reduction.free_vars x)) in
        match (Hw2_unify.solve_system (stas sys)) with  
                |None -> None
                |Some a -> Some (sas_to_sss a, atst
                (Hw2_unify.apply_substitution a (stat type_t)));;

(*
infer_simp_type(Hw1.lambda_of_string("\\x.x"));
*)


(*
let None  = infer_simp_type (Hw1.lambda_of_string "\\x.x x");;
*)

(*---------------- Algorithm W -----------------*)


type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of
hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type


let ps s =
        print_string (s ^ "\n");;

let string_of_hmt hmt =
        let rec impl hmt = 
                match hmt with
                        |HM_Elem v -> v
                        |HM_Arrow(hmt1, hmt2) -> (impl hmt1) ^ " -> " ^ (impl hmt2) 
                        |HM_ForAll(v, hmt) -> "∀" ^ v ^ "." ^ (impl hmt) in
        impl hmt;;

module StringSet = Set.Make (String) 
module StringMap = Map.Make (String) 

let free_vars_hmt hmt = 
        let rec hepler hmt blocked = match hmt with
                |HM_Elem v ->
                        if StringSet.mem v blocked 
                                then StringSet.empty
                                else StringSet.singleton v
                |HM_Arrow (hmt1, hmt2) -> StringSet.union (hepler hmt1 blocked) (hepler hmt2 blocked)        
                |HM_ForAll(v, x) -> hepler x (StringSet.add v blocked) in
        hepler hmt StringSet.empty;;

let free_vars_context cxt = 
        StringMap.fold (fun k v set -> StringSet.union (free_vars_hmt v) set) cxt StringSet.empty;;

let pm m = 
        StringMap.iter (fun k v -> (print_string ("{" ^ k ^ " " ^
        (string_of_hmt v) ^ "}\n"))) m;;

let print_set s =
        StringSet.iter (fun s -> (print_string (s ^ "\n"))) s;;

(* y will not  pass *)
print_string "\n";;



let closure hmt ctx = 
        let fctx = free_vars_context ctx in
        let diff = StringSet.fold (fun k set -> if StringSet.mem k fctx then set
                        else StringSet.add k set) (free_vars_hmt hmt)
                        StringSet.empty in
        StringSet.fold (fun k t -> HM_ForAll(k, t)) diff hmt;;

let rec hta hmt =
        match hmt with
                |HM_Elem v -> Hw2_unify.Var v
                |HM_Arrow(hmt1, hmt2) -> Hw2_unify.Fun ("impl", [hta hmt1; hta hmt2])
                |_ -> failwith ("never happens, 'cause according to Artem quantifiers can't be met here");;

(* no documentation herre *)
let sath sat =
        let rec ath a =
                match a with 
                        |Hw2_unify.Var v -> HM_Elem v
                        |Hw2_unify.Fun ("impl", [a; b]) -> HM_Arrow (ath a, ath b) 
                        |_ -> failwith "no warining pls" in
        List.fold_left (fun map (v, t) -> StringMap.add v (ath t) map) StringMap.empty sat ;;

(*
let Some res = Hw2_unify.solve_system [hta t2, hta t3];;
*)

(* make substitution ie s is subst, t is type to make subst to *)
(*      subst is a map *)
let ms s t =
        let rec h tfs blocked =
                match tfs with 
                        |HM_Elem v -> if StringSet.mem v blocked then tfs
                                else 
                                        if StringMap.mem v s 
                                                then StringMap.find v s 
                                                else tfs 
                        |HM_Arrow (h1, h2) -> HM_Arrow(h h1 blocked, h h2 blocked)
                        |HM_ForAll (v, h1) -> HM_ForAll(v, h h1 (StringSet.add
                        v blocked)) in 
        h t StringSet.empty;;



let merge_subst s2 s1 =
        StringMap.fold (fun k v map -> if StringMap.mem k map then map else StringMap.add k v map) s2 
        (StringMap.fold (fun k v map -> StringMap.add k (ms s2 v) map) s1 StringMap.empty);; 

(* returns type with no quantifiers *)
let dwrp t =
        let rec hepler t =
                match t with
                        |HM_ForAll(v, lhs) -> ( ms (StringMap.singleton v (HM_Elem(name_generator ()))) (hepler lhs))
                        |_ -> t in
        hepler t;;

(*subst to context*)
let stc subst ctxt = StringMap.fold (fun k v map -> (StringMap.add k (ms subst v) map)) ctxt StringMap.empty;;

let rec wepler ctx l = match l with
        |HM_Var v -> (StringMap.empty, dwrp (StringMap.find v ctx))
        |HM_App (x, y) -> 
                        (let s1, t1 = wepler ctx x in
                        let s2, t2 = wepler (stc s1 ctx) y in
                        let fresh = name_generator () in
                        let res = Hw2_unify.solve_system [hta (ms s2 t1), hta
                        (HM_Arrow(t2, HM_Elem (fresh)))] in
                        match res with 
                                |None -> failwith "Robinson fault is not an error" 
                                |Some r -> (
                                        let rob_subst = sath r in
                                        let merged = merge_subst rob_subst (merge_subst s2 s1) in
                                        (merged, ms merged (HM_Elem fresh))))
        |HM_Abs (x, y) -> 
                        (let fresh = name_generator () in
                        let stmp = StringMap.remove x ctx in
                        let stmp = StringMap.add x (HM_Elem(fresh)) stmp in
                        let s1, t1 = wepler stmp y in
                        (s1, HM_Arrow((ms s1 (HM_Elem(fresh))), t1)))
        |HM_Let (x, h1, h2) ->
                        (let s1, t1 = wepler ctx h1 in
                        let sctx = stc s1 ctx in
                        let nctx = StringMap.remove x sctx in
                        let nctx = StringMap.add x (closure t1 sctx) nctx in
                        let s2, t2 = wepler nctx h2 in
                        (merge_subst s2 s1, t2));;


let algorithm_w l = 
        let s, t = wepler StringMap.empty l in
        Some ((StringMap.bindings s), t);;

let hml = HM_Abs("x", HM_Var("x"));;


let t1 = HM_ForAll ("alpha", HM_Arrow(HM_Elem("alpha"), HM_Elem("beta")));;
let t2 = HM_Arrow(HM_Elem("theta"), HM_Elem("gamma"));;
let t3 = HM_Arrow(HM_Elem("alpha"), HM_Elem("betha"));;

let cxt1 = StringMap.empty;;
let cxt1 = StringMap.add "a" t1 cxt1;;
let cxt1 = StringMap.add "b" t2 cxt1;;

let subst1 = StringMap.empty;;
let subst1 = StringMap.add "alpha" (HM_Elem "v") subst1;;
let subst1 = StringMap.add "gamma" (HM_Elem "u") subst1;;

let subst2 = StringMap.empty;;
let subst2 = StringMap.add "u" (HM_Elem "z") subst2;;
let subst2 = StringMap.add "b" (HM_Elem "x") subst2;;

let testik = HM_Let("id", HM_Abs("x", HM_Var("x")), HM_Abs("x", HM_App(HM_Var("id"), HM_Var("x"))));;

let pl l =
        List.iter (fun (v, t) -> print_string ("[ " ^ v ^ " " ^ (string_of_hmt t) ^ " ]")) l;;


let Some (trash, t) = algorithm_w testik;;
print_string "\n";;
ps (string_of_hmt t);;
pl trash;;



