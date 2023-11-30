(* Esercizio 1 -> Comparing Fractions *)

let is_posfra (a:int) (b:int) : bool =
  if((a<0)&&(b<0))||((a>0)&&(b>0)) then true
  else if (b=0) then failwith "Not a fraction"
  else false;;

let compare_posfrac (a,b) (c,d) =
  if(a*d) = (b*c) then 0 else
    if (a*d) > (b*c) then 1 else -1;;

assert (compare_posfrac (1,2) (2,4) == 0);;
assert (compare_posfrac (1,2) (1,3) == 1);;
assert (compare_posfrac (1,2) (2,3) == -1);;

(* Esercizio 2 -> Consensus3 *)



(* Esercizio 3 -> Tris *)

let tris (a,b,c,d) : (bool) = match (a,b,c,d) with
    (a,b,c,_) when a=b && a=c -> true
  | (a,b,_,d) when a=b && a=d -> true
  | (a,_,c,d) when a=c && a=d -> true
  | (_,b,c,d) when c=b && b=d -> true
  | _ -> false
;;

let hand () = (Random.int(10)+1, Random.int(10)+1, Random.int(10)+1, Random.int(10)+1);;

tris(hand());;
tris(hand());;
tris(0,0,0,1);;

(* Esercizio 4 -> Poker *)

type suit = S | H | D | C;;
type card = Card of int * suit;;

let suitEx () = match (Random.int(4)) with
    0 -> (Card(1+Random.int(10),S))
  | 1 -> (Card(1+Random.int(10),H))
  | 2 -> (Card(1+Random.int(10),D))
  | 3 -> (Card(1+Random.int(10),C))
  | _ -> failwith "Errore";;

let rndHand () =
  (suitEx(),suitEx(),suitEx(),suitEx(),suitEx());;

let poker4 (Card(v1,s1),Card(v2,s2),Card(v3,s3),Card(v4,s4)) =
if((v1 = v2 && v2 = v3 && v3 = v4)&&(s1<>s2 && s1<>s3 && s1<>s4 && s2<>s3 && s2<>s4 && s3<>s4)) then true else false;;

let poker(c1,c2,c3,c4,c5) = 
poker4(c1,c2,c3,c4) ||
poker4(c5,c2,c3,c4) ||
poker4(c1,c5,c3,c4) ||
poker4(c1,c2,c5,c4) ||
poker4(c1,c2,c3,c5);;

poker (rndHand());;
poker (Card(1,S),Card(1,H),Card(1,D),Card(1,C),Card(2,S));;
poker (Card(2,S),Card(1,H),Card(1,D),Card(1,C),Card(1,S));;
poker (Card(1,S),Card(1,H),Card(1,D),Card(2,C),Card(2,S));;
poker (Card(1,S),Card(1,H),Card(1,D),Card(1,H),Card(2,S));;


(* Esercizio 5 -> Sraight *)

type suit = S | H | D | C;;
type card = Card of int * suit;;

let min x y = not(x < y);;

let straight (Card (n1, a), Card (n2,b), Card (n3,c), Card (n4, d), Card (n5,e)) =
  if min n1 n2 then if(n1 = n2+1) && (n2 = n3+1) && (n3 = n4+1) && (n4 = n5+1) then true else false
else if(n1 = n2-1) && (n2 = n3-1) && (n3 = n4-1) && (n4 = n5-1) then true else false;;

straight (Card(1,S), Card(2,S), Card(3,S), Card(4,S), Card(5,S));;
straight (Card(5,S), Card(4,S), Card(3,S), Card(2,S), Card(1,S));;
straight (Card(5,S), Card(5,S), Card(5,S), Card(2,S), Card(1,S));;

(* Esercizio 6 -> Loaded Dice *)
let dice p = 
  if((1+Random.int(100))<= p) then 6 else 1+Random.int(5);;

dice 70;;

(* Esercizio 7 -> Morra *)

type winner = Player | Computer | Tie ;;

let win (hp,gp) = 
  let hc = Random.int(5) and gc = Random.int(10) in
  if ((hp+hc) = gc) then ((hc,gc),Computer)
  else if ((hp+hc) = gp) then ((hc,gc),Player)
  else ((hc,gc),Tie);;

(* Esercizio 8 -> Sum Range *)

let rec sumrange a b =
  if(a > b) then 0 else a + (sumrange (a+1) b)
;;

assert (sumrange 0 1 = 1);;

assert (sumrange 1 3 = 6);;

assert (sumrange 3 2 = 0);;

(* Esercizio 9 -> Count zeros of a function *)

let rec countzero (f: int -> int) a b  : int = 
let rec count n (f: int -> int) a b = match (f a, b) with 
(0, b) when a <= b -> count (n+1) f (a+1) b
| _ when a > b -> n
| _ -> count n f (a+1) b
in count 0 f a b;;

assert (countzero (fun x -> x) (-10) 10 = 1);;
assert (countzero (fun x -> x) 1 10 = 0);;
assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;
assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;

(* Esercizio 10 -> Has One *)

(* ricordati di passare la lista vuota quando richiami la funzione *)
let rec list_of_int n l = match n with
    n when n > 0 -> list_of_int (n/10) l@[(n mod 10)]
  | _ -> []
;;

let rec has_one n = 
  let rec scomposizione = function
    [] -> false
  | (1::w) -> true
  | (_::w) -> scomposizione w
in scomposizione (list_of_int n [])
;;


assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;