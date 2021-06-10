(* ruchi 115,hw2, 2021*)


fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* all_except_option input -> string * string list
                     output -> string list option *) 
fun all_except_option (s1: string, s2: string list) =
    case s2 of
	[] => NONE
       |x::xs => case same_string(s1,x) of
		     true => SOME(xs)
		    |false => case all_except_option(s1,xs) of
				  NONE => NONE
				 |SOME y => SOME (x::y)
						 
(* get_substitutions1 input-> string list list * string
                      output -> string list *)
fun get_substitutions1 ( sll, s) =
    case sll of
        [] => []
       | x::xs => case all_except_option(s, x)  of
		     NONE => get_substitutions1(xs, s)
		   | SOME ys => ys @ get_substitutions1(xs,s)

						       
(* get_substitutions2 is get_substituions1 using tail recursion. *)
fun get_substitutions2 (sll, s) =
    let fun aux (sll, s, acc) =
	    case sll of
		[] => acc
	      | x::xs =>  case all_except_option(s, x)  of
			      NONE => aux(xs, s, acc)
			     |SOME ys  => aux( xs, s, acc @ ys)
    in
	aux (sll, s, [])
    end

	
(* similar_names input-> string list list (*part b or c*) * 
                         {first:string,middle:string,last:string}
                output-> [ subs of {first:string,middle:string,last:string} ] *)
type Name = {first:string, middle:string, last:string}
fun similar_names (sll, name) =
    let fun aux ( sll, acc) =
	    case sll of
		[] => acc
	       |x::xs => aux(xs, acc @[{first=x, middle=(#middle name), last=(#last name)}])
    in
	aux( get_substitutions2(sll,#first name), [name])
    end
  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* fun card_color input -> card
	          output -> color *)
fun card_color (suit,rank)=
    case suit of
	Spades => Black
      | Clubs => Black
      | Diamonds => Red
      | Hearts => Red
(* fun card_value input-> card
                  output->value*)
fun card_value (suit, rank) =
    case rank of
	 Num i => i
        | Ace => 11
	| Jack => 10
	| Queen => 10
	| King => 10 

(* fun remove_card input-> card cs list * card c * e
                  output -> all elements of cs except c *)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
       | x::xs => case c = x of
		       true => xs
		     | false => case remove_card(xs, c, e) of
				   [] => [x]
				  | y::ys => x::y::ys
  (* fun all_same_color input-> card list 					                                       output-> if all same color then true *)
fun all_same_color (cs) =
    case cs of
	[] => true
       |a::[]  => true
       |a::b::tail => case card_color(a) = card_color(b) of
			  true => all_same_color(b::tail)			                        | false => false
				       
(* fun sum_cards input-> list of cards
                output-> returns sum *)
fun sum_cards (cs) =
    let fun aux(cs, acc) =
	    case cs of
		[] => acc
	       |x::xs => aux(xs, acc + card_value(x))
    in
	aux(cs, 0)
    end
	
	      
fun score (cs, goal) = 
  let fun pre_score (cs) =
    case (sum_cards(cs), goal) of
      (sum, goal) => case sum > goal of
                       true => (sum - goal) * 3
                       | false => goal - sum
  in
    case all_same_color(cs) of
      true => pre_score(cs) div 2
      | false => pre_score(cs)
  end

fun officiate (cs, ms, goal) =
  let fun process_moves(cs, ms, held) =
      case ms of
        [] => held
        | m::ms_tail => case m of
                          Discard card => process_moves(cs, ms_tail, remove_card(held, card, IllegalMove))
                          | Draw => case cs of
                                      [] => held
                                      | c::_ => case sum_cards(c::held) > goal of
                                                  true => c::held
                                                  | false => process_moves(remove_card(cs, c, IllegalMove), ms_tail, c::held)
                                                       
                                                       
  in
    score(process_moves(cs, ms, []), goal) 
  end
