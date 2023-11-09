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

(* Esercizio 2 -> Bounce *)

let bounce y x = match x mod (2*y) with


(* Esercizio 3 -> Consensus3 *)



(* Esercizio 4 -> Tris *)

let tris (a,b,c,d) : (bool) = match (a,b,c,d) with
    (a,b,c,_) when a=b && a=c -> true
  | (a,b,_,d) when a=b && a=d -> true
  | (a,_,c,d) when a=c && a=d -> true
  | (_,b,c,d) when c=b && b=d -> true
  | _ -> false;;

let hand () = (Random.int(10)+1, Random.int(10)+1, Random.int(10)+1, Random.int(10)+1);;

tris(hand());;
tris(hand());;
tris(0,0,0,1);;

(* Esercizio 5 -> Poker *)

type suit = S | H | D | C;;
type card = Card of int * suit;;

let suitEx () = match (Random.int(4)) with
    0 -> (S, 1+Random.int(10))
  | 1 -> (H, 1+Random.int(10))
  | 2 -> (D, 1+Random.int(10))
  | 3 -> (C, 1+Random.int(10))
  | _ -> failwith "Errore";;

let rndHand () =
  (suitEx(),suitEx(),suitEx(),suitEx(),suitEx());;

rndHand();; 

(* Esercizio 6 -> Sraight *)

type suit = S | H | D | C;;
type card = Card of int * suit;;

let min x y = if x < y then false else true;;

let straight (Card (n1, a), Card (n2,b), Card (n3,c), Card (n4, d), Card (n5,e)) =
  if min n1 n2 then if(n1 = n2+1) && (n2 = n3+1) && (n3 = n4+1) && (n4 = n5+1) then true else false
else if(n1 = n2-1) && (n2 = n3-1) && (n3 = n4-1) && (n4 = n5-1) then true else false;;

straight (Card(1,S), Card(2,S), Card(3,S), Card(4,S), Card(5,S));;
straight (Card(5,S), Card(4,S), Card(3,S), Card(2,S), Card(1,S));;
straight (Card(5,S), Card(5,S), Card(5,S), Card(2,S), Card(1,S));;

(* Esercizio 7 -> Loaded Dice *)
let dice p = 
  if((1+Random.int(100))<= p) then 6 else 1+Random.int(5);;

dice 70;;

(* Esercizio 8 -> Morra *)

type winner = Player | Computer | Tie ;;

let win (hp,gp) = 
  let hc = Random.int(5) and gc = Random.int(10) in
  if ((hp+hc) = gc) then ((hc,gc),Computer)
  else if ((hp+hc) = gp) then ((hc,gc),Player)
  else ((hc,gc),Tie);;