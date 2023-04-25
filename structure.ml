
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



(*getters*)
let getn x = x.name ;;
let geta x = x.ar ;;
let getl x = x.arg ;;