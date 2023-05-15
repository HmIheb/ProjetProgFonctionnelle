
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

 (* Trouve la bonne parenthese fermante relié a la premiere parenthese ouvrante trouvé *)
  let findpar s start =
     let rec helper s start stack = match s.[start] with
      | '(' -> helper s (start + 1) (start :: stack) 
      | ')' -> (match stack with 
      | [] -> failwith "Unmatched closing parenthesis" 
      | top :: rest -> if rest = [] then start else helper s (start + 1) rest) 
      | _ -> helper s (start + 1) stack in helper s start [] ;;

(* Decoupe s En terme de premier ordre et les mets dans l puis renvoie l 
Todo Rectifier Les cas multi parenthese ')' *)
let rec bayd (s:string ) (l:string list) = 
  if  ( String.equal s "" )then l 
  else if (Bool.not (String.contains s ',')) then l@[s]
  else if (String.contains (Str.string_before s (Str.search_forward (Str.regexp{|,|}) s 0 )) '(' ) then bayd (Str.string_after s ((findpar s 0)+2) ) l@[(Str.string_before s ((findpar s 0)+1))]
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

type eq = {left : terme ; right : terme} ;;

(* @return liste d'equations  *)

(* @return liste d'equations *)
let rec unif_term (term1:terme) (term2:terme)  = 
if (term1.ar < 1 || term2.ar < 1 ) then [{left =term1 ; right=term2}]
  
else if (term1.ar == term2.ar && (String.equal term1.name term2.name)) then 
  let rec unif_arg arg_term1 arg_term2 =
    match arg_term1, arg_term2 with
    | [],_ -> []
    | t::q , x::y -> (unif_term t x)@(unif_arg q y) in
  (unif_arg term1.arg term2.arg)
else failwith "(differentes arités ou differentes noms)" ;;

(* verfie la presence d'une variable dans un terme *)
let rec variable_est_repeter (variable : string) (args:terme list) = 
  match args with
  | [] -> false
  | t::q -> if ( String.equal variable t.name ) then
                true
            else if (t.ar > 0) then
              (variable_est_repeter variable t.arg) || (variable_est_repeter variable q)
            else (variable_est_repeter variable t.arg) || (variable_est_repeter variable q) ;;    

let rec simple_sys (l:eq list) acc = 
  match l  with
  | [] -> acc
  | t::q ->
    if (String.equal t.right.name t.left.name)  then 
        if (t.right.ar > 0 && t.left.ar > 0 && t.right.ar!= t.left.ar ) then (*f(X)=f(Y,Z)*)
          failwith "(f(X)=f(Y,Z))"
        else if (t.right.ar < 1 && t.left.ar < 1) then  (* X=X; a=a *)
          (simple_sys q acc) 
        else
            (simple_sys (append (unif_term t.left t.right) q) acc)        
      else 
        if (t.right.ar == 0 && t.left.ar == 0) then
          failwith "(a=b)"

        else if (t.left.ar >= 0 && t.right.ar == -1 ) then (* a=X ; f(X)=X *)
          let e = {right = t.left ; left = t.right} in
          simple_sys q [e]@acc
            else if (t.right.ar == 0) then (* X=a *)
                  simple_sys q [t]@acc
                else if (t.right.ar > 0) then (* x=f(..) *)
                  let tt = t.right in
                  if ( variable_est_repeter t.left.name t.right.arg ) then (* x=f(X) *)
                    failwith "(x=f(X))"
                  else (* x=f(Y) *)
                    simple_sys q  [t]@acc 
                else   (* X=Y *)
                  simple_sys q  [t]@acc ;;  

(* substitution dans une liste de termes avec une equation*)
let rec sub_term eq l  =
  match l with
  | [] -> []
  | t::q -> if(String.equal eq.left.name t.name) then (* X=a  X *)
                [eq.right]@sub_term eq q (* X=a  a *)
            else if (t.ar > 0) then  (* X=a   f(x) *)
              let subb = (sub_term eq t.arg) in
              let termm = {name = t.name; ar = t.ar; arg = subb} in
              [termm]@(sub_term eq q)  
            else [t]@(sub_term eq q) ;; (* X=a  Z *)

(* substitution dans une liste d'equations avec une equation*)            
let rec sub_eq eq l =
  match l with
  | [] -> []
  | t::q -> if(String.equal eq.left.name t.left.name) then (* X=.. , X=..*)
              if (String.equal eq.right.name t.right.name) then (* X=f , X=f*)
                sub_eq eq q
              else if (eq.right.ar == -1) then  (*X=f X=g *)
                    if (variable_est_repeter eq.right.name t.right.arg ) then
                      failwith "(X=U, X=f(u)) 2"
                    else
                      [t]@sub_eq eq q
                      (* failwith "kmok (X=a, X=f(t)) 1" *)
                  else  if (t.right.ar == -1) then
                          if (variable_est_repeter t.right.name eq.right.arg) then
                            failwith "kmok (X=U, X=f(u)) 2"
                          else 
                            [t]@sub_eq eq q

                          else  failwith "kmok (X=a, X=f(t)) 2"
            else 
               if (String.equal eq.left.name t.right.name ) then 
              if (variable_est_repeter t.left.name eq.right.arg) then
                failwith "kmok (X=f(..), U=X)"
              else 
                 let teerm = {left=t.left; right=eq.right} in
                  [t]@[teerm]@sub_eq eq q 
                
              else if (String.equal eq.right.name t.left.name ) then 
                if (variable_est_repeter eq.left.name t.right.arg) then
                  failwith "kmok (X=f(..), U=X)"
                else 
                   let teerm = {left=eq.left; right=t.right} in
                    [t]@[teerm]@sub_eq eq q 
                  
                else  if (eq.right.ar <=0 && t.right.ar > 0) then (* X=a , Y=f(...) *)
                  let args = (sub_term eq  t.right.arg) in
                  let termm = {name = t.right.name ; ar = t.right.ar ; arg = args} in
                  let eqq = {left = t.left ; right = termm } in
                  [eqq]@sub_eq eq q
                else  [t]@sub_eq eq q ;;
              (* else  let args = (sub_term eq  t.right.arg) in
                    let termm = {name = t.right.name ; ar = t.right.ar ; arg = args} in
                    let eqq = {left = t.left ; right = termm } in
                    [eqq]@sub_eq eq q ;; *)


(* fait les substitutions sur entre les equations d'in system *)
let rec remp_sys (l:eq list) acc =
  match l with
  | [] -> acc
  | t::q -> let sys = (sub_eq t q) in
            (remp_sys sys [t]@acc)  ;;

(* unifier une listes de termes par une liste d'equations *)                      
let rec unifier_args args sys  =
match args with
| [] -> []
| arg::q -> match sys with
        | [] -> []      
        |  eq::qq -> if (String.equal arg.name eq.left.name) then 
                        [eq.right]@unifier_args q sys
                    else let term = unifier_term arg sys in
                        [term]@unifier_args  q sys 

                and  
(* unifier un terme par une liste d'equations *)          
unifier_term term (sys:eq list) = 
            match sys with
            | [] -> term  
            | eq::q -> if (String.equal term.name eq.left.name) then 
                           eq.right
                      else if (term.ar <= 0) then
                        term
                          else  
                            let args = unifier_args term.arg sys in
                            let termm = {name = term.name ; ar = term.ar ; arg = args} in
                            (unifier_term termm q)  ;;



(*getters*)
let getn x = x.name ;;
let geta x = x.ar ;;
let getl x = x.arg ;;

 (* Transforme un terme en String*)
let rec t2s (t:terme):string =
    if(t.ar<1) then t.name 
  else let rec concatlt l = match l with
    | [] -> "" 
    | h::q -> if q = [] then (t2s h) 
  else ((t2s h)^",")^(concatlt q) in t.name^"("^(concatlt t.arg)^")" 
;; 

(* Type Permutation avec Gauche droite et la variable qui les remplace *)
type perm = {var: terme ; left : terme ; right : terme} ;;
      
(* Contain sur les list de permutation *)
  let rec permcontain (t1:terme) (t2:terme) (buf:perm list) = match buf with 
  |[]-> false 
  |hd::tl -> (hd.left=t1 && hd.right=t2)||(permcontain t1 t2 tl) 
;;

(* Avoir une Permutation si on sait qu'elle existe *) 
let getperm t1 t2 buf = 
  let rec help buff = match buff with 
    |[] -> t1 
    |hd::q -> if (hd.left=t1 && hd.right=t2) then hd.var 
      else help q 
  in help buf ;; 
  (* Union de deux Listes *) 
let rec union l1 l2 = 
    let rec f x l = match l with 
    | [] -> true 
    | hd::tl -> if x = hd then false 
    else f x tl in match l2 with 
    | [] -> l1 
    | hd::tl -> if f hd l1 then union (hd::l1) tl 
    else union l1 tl ;;

(* Toute les permutation à faire entre 2 termes *)
let rec fly t1 t2 buf = 
  if (t1 = t2) then buf 
  else if (permcontain t1 t2 buf) then buf 
  else if t1.ar>=1 && t1.ar == t2.ar && (String.equal t1.name t2.name) then 
    let rec flyh l1 l2 buff = match l1,l2 with 
    |[],[] -> buff 
    |t::q,hd::tl -> if (fly t hd buff = buff || t=hd) then buff 
    else union (buf@(fly t hd buff)) (flyh q tl (buf@(fly t hd buff))) 
  in flyh t1.arg t2.arg buf 
  else buf@[{left = t1 ; right = t2; var = { name= ("Z"^(Int.to_string(List.length buf))) ; ar= (-1) ; arg=[] } }] ;;
  (* Anti-unification *)
let rec antiuni (t1:terme) (t2:terme) (buf: perm list) = 
    if t1.ar>=1 && t1.ar == t2.ar && (String.equal t1.name t2.name) then
        {name=t1.name ; ar =t1.ar ; arg =
        let rec lantiuni l1 l2 b = match l1,l2 with 
        | [],[] -> []; 
        | t::q , hd::tl -> [antiuni t hd b]@(lantiuni q tl (fly t hd b)) 
      in lantiuni t1.arg t2.arg (fly t1 t2 buf) } 
    else if (t1.ar == t2.ar && (String.equal t1.name t2.name))then t1 
    else if (permcontain t1 t2 buf) then (getperm t1 t2 buf)
      else { name= ("Z"^(Int.to_string(List.length buf))) ; ar= (-1) ; arg=[] } ;; 

let antiunif t1 t2 =
  t2s (antiuni (part t1) (part t2) []);;

let unif t1 t2 = 
  t2s (unifier_term (part t1) (remp_sys (simple_sys (unif_term (part t1) (part t2))  []) [])) ;;
;;

