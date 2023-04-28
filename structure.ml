
open Stdlib ;;
#load   "str.cma";;


(* Type terme de premier ordre :
Arité -1 Variable 
Arité 0 Constante 
Arité n>0 Fonction qui prend n variable *)
type terme  = {name : string ; ar : int ; arg : terme list} ;;

let append l t =l@t ;;

(*reverse  et enleve les String vide*)
let rec rev l = match l with 
|[] -> l
|a::t -> if (String.equal a "" ) then (rev t) else  append (rev t) [a] ;;
;;


(* Renvoie vrai si c'est une Majuscule*)
let maj c = 
  if Char.code c > 64 && Char.code c <91 then true
  else false ;;

  (* Renvoie vrai si c'est une variable ou une constante*)
let estpasfonction s = Bool.not (String.contains s '(') ;;

(* Permet de determine l'arité d'un terme -1 si c'est une variable 0 si c'est une constante*)
let xouc s = if maj (String.get s 0) then -1 else 0 ;;

(* Decoupe s En terme de premier ordre et les mets dans l puis renvoie l 
Todo Rectifier Les cas multi parenthese ')' *)
let rec bayd (s:string ) (l:string list) = 
  if  ( String.equal s "" )then l 
  else if (Bool.not (String.contains s ',')) then l@[s]
  else if (String.contains (Str.string_before s (Str.search_forward (Str.regexp{|,|}) s 0 )) '(' ) then bayd (Str.string_after s ((Str.search_forward (Str.regexp {|)|}) s 0)+1) ) l@[(Str.string_before s ((Str.search_forward (Str.regexp {|)|}) s 0)+1))]
  else bayd (Str.string_after s ((Str.search_forward (Str.regexp {|,|}) s 0)+1) ) l@[Str.string_before s (Str.search_forward (Str.regexp {|,|}) s 0 )]
;;

(* Enleve l'entete de la fonction ainsi que les parenthese ouvrante est fermante *)
let frag (s :string ) =
  let x = (Str.string_after s ((Str.search_forward (Str.regexp {|(|} ) s 0)+1) ) in 
  let t =    (Str.string_before x (Str.search_backward (Str.regexp {|)|} ) x ((String.length x)))) in
    rev (bayd t []) ;;

  (* Transforme une String en terme  *)
  let rec  part (s:string ) =
    if (estpasfonction s) then let x = {name=  s  ; ar = xouc s ; arg = []} in x
    else let x ={name = Str.string_before s (Str.search_forward (Str.regexp {|(|}) s 0); ar =List.length (frag s) ; arg = List.map part (frag s) } in x
;; 



(* type terme  = {name : string ; ar : int ; arg : terme list} ;; *)

(* 
l'entrée : deux termes.
la sortie : des variables differentes ancrés.****

probleme : c'est la resolution  

*)

type eq = {right : terme ; left : terme} ;;

(* @return liste d'equations  *)

(* @return liste d'equations *)
let rec unif_term (term1:terme) (term2:terme)  = 
if (term1.ar < 1 || term2.ar < 1 ) then [{right =term1 ; left=term2}]
  
else if (term1.ar == term2.ar && (String.equal term1.name term2.name)) then 
  let rec unif_arg arg_term1 arg_term2 =
    match arg_term1, arg_term2 with
    | [],_ -> []
    | t::q , x::y -> (unif_term t x)@(unif_arg q y) in
  (unif_arg term1.arg term2.arg)
else failwith "kmok (differentes arités ou differentes noms)" ;;

(* unif_term (part "F(toz,g(a),X)") (part "F(Zebi,g(u),t)") ;; *)

let variable_est_repeter (variable : string) (fonction : string) = 
  let re = Str.regexp_string variable
    in
        try ignore (Str.search_forward re fonction 0); true
        with Not_found -> false ;;

let rec simple_sys (l:eq list) acc= 
  match l  with
  | [] -> []
  | t::q ->
      

    if (String.equal t.right.name t.left.name)  then 
        if (t.right.ar > 0 && t.left.ar > 0 && t.right.ar!= t.left.ar ) then (*f(X)=f(Y,Z)*)
          failwith "kmok (f(X)=f(Y,Z))"
        else if (t.right.ar < 1 && t.left.ar < 1) then  (* X=X; a=a *)
          (simple_sys q acc) 
        else
            (simple_sys (append (unif_term t.left t.right) q) acc)        
      else 
        if (t.right.ar == 0 && t.left.ar == 0) then
          failwith "kmok (a=b)"

        else if (t.left.ar >= 0 && t.right.ar == -1 ) then (* a=X ; f(X)=X *)
          (simple_sys q [{right = t.left ; left = t.right}]@acc)
            else if (t.right.ar == 0) then (* X=a *)
                  simple_sys q [t]@acc
                else if (t.right.ar > 0) then (* x=f(..) *)
                  if ( variable_est_repeter t.left.name t.right.name) then (* x=f(X) *)
                    failwith "kmok (x=f(X))"
                  else (* x=f(Y) *)
                    simple_sys q  [t]@acc 
                else   (* X=Y *)
                  simple_sys q  [t]@acc ;;  


(*getters*)
let getn x = x.name ;;
let geta x = x.ar ;;
let getl x = x.arg ;;

