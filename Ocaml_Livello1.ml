(* Esercizio 1 -> Valore assoluto *)
let f x =
  if x >= 0 then x else -x;;

(* Esercizio 2 -> maggiore tra due numeri *)
let max x y =
  if (x<0 || y <0)then failwith "ERRORE" else if x >= y then x else y;;

(* Esercizio 3 -> LogicalNand *)
let logicalNand a b =
  if (a && b) then false else true;;

logicalNand true true;;
logicalNand true false;;
logicalNand false false;;
logicalNand false true;;

(* Esercizio 4 -> Weekly Lectures *)

type weekday = Mo | Tu | We | Th | Fr

type course = ALF | LIP

let isLecture (d:weekday) (c:course) : (bool) = match c with
    ALF when d=Tu || d=Th || d=Fr -> true
  |LIP when d=We || d=Th -> true
  | _ -> false;;

isLecture Mo ALF;;
isLecture Th ALF;;
isLecture We LIP;;

(* Esercizio 5 -> Parrot Trouble *)

let parrot_trouble (talking:bool) (hour:int) : (bool option) = match (talking, hour) with
    (_, hour) when (hour < 0 || hour > 23) -> None
  |(true, hour) when (hour <= 7 || hour >= 20) -> Some true
  | (true, hour) when (hour > 7 && hour < 20) -> Some false
  | _ -> None;;

parrot_trouble true 6;;
parrot_trouble true 9;;
parrot_trouble false 8;;
parrot_trouble true 34;;

(* Esercizio 6 -> Even and odd *)

let is_even (x:int) : (bool) = match x with
    x when (x mod 2 = 0) -> true
  |_ -> false;;


let win (a:int) (b:int) : (int) = 
  if(a < 1 || a > 5)&&(b >= 1 && b <= 5) then -1 else 
  if (b < 1 || b > 5)&&(a >= 1 && a <= 5) then 1 else
  if((a < 1 || a > 5)&&(b < 1 || b > 5)) then 0 else 
  if is_even (a+b) then 1 else -1;;

win 3 4;;
win 4 4;;
win 0 3;;
win 3 0;;
win 0 0;;

(* Esercizio 7 -> Funzione in range *)

let in_range x a b : (bool) =
  if(x >= a && x <= b) then true else false;;

in_range 3 2 4;;
in_range 1 2 3;;

(* Esercizio 8 : Funzione quadrato *)

let exp9 (x:int) : (int) =
  let square x = x*x
  in square (square (square (x))) * x;;
exp9 2;;

(* Esercizio 9 *)
type card = Joker | Val of int;;

let win (p:card) (d:card) : (bool) = match (p,d) with
    (Joker, Joker) -> false
  | (Joker, _) -> true
  | (_, Joker) -> false
  | (_) -> if p>=d then true else false;;

win Joker Joker;;
win (Val 3) (Val 4);;
win (Val 4) (Val 3);;
win Joker (Val 4);;
win (Val 4) Joker;;


(* Esercizio 10 -> Head or Tail *)
let hot=
  if (Random.int(500) > (500/2)) then "Head" else "Tail";;

(* Esercizio 11 -> Prende in input 3 valori, restituisce una coppia dove a sinistra c'è il minimo e a destra il massimo*)

let max a b c = if (a>b && a>c) then a else if (b>a && b>c ) then b else c;;
let min a b c = if (a<b && a<c) then a else if (b<a && b<c ) then b else c;;

let minmax3 a b c = 
  (min a b c, max a b c);;
  
minmax3 2 3 4;;


(* Esercizio 12 -> *)

let guess5 n = if (n<1 || n>5) then failwith "Errore" else
    (1 + Random.int(5)) |> fun p -> ((if(n=p) then true else false), p);;

guess5 4;;

(* Esercizio 13 -> *)

let seven_eleven_doubles =
  let d1 = (1 + Random.int(6)) and d2 = (1 + Random.int(6)) in 
  if((d1+d2)=7 || (d1+d2)=11 || (d1=d2)) then (true,d1,d2) else (false, d1,d2);; 
  
  
  
  
  
  
