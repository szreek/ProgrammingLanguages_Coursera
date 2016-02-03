(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option0(str, xs)=
    case xs of
	[] => NONE
      | _ =>  
	let
	    fun helper(ys, os)=
		case ys of 
		    [] => os
		  | y::ys' => if same_string(y, str) = false
			      then helper(ys', y::os)
			      else helper(ys', os)
	in
	    let val output = helper(xs, [])
	    in
		if length output = length xs
		then NONE
		else SOME output
	    end
	end

fun all_except_option(str, xs)=
    case xs of
	[] => NONE
      | x::xs' => 
	case (same_string(str, x), all_except_option(str, xs')) of
	    (false, NONE) => NONE
	  | (true, _) => SOME xs'
	  | (false, SOME x') => SOME (x::x') 


fun get_substitutions1(lst, str)=
    case lst of
	[] => []
      | xs::xss => 
	case (all_except_option(str, xs), get_substitutions1(xss, str)) of
	    (SOME x, []) => x
	  | (NONE, []) => []
	  | (NONE, y::ys') => y::ys'
	  | (SOME x, y::ys') => x @ y::ys' 
    
fun get_substitutions2(lst, str)=
    let
	fun helper(option, pointer, elem)=
	    case (option, pointer, elem) of
		(NONE, y::ys, _) => helper(all_except_option(str, y), ys, elem)
	      | (NONE, [], _) => elem
	      | (SOME x, [], _) => x @ elem
	      | (SOME x, y::ys, _) => helper(all_except_option(str, y), ys, x @ elem) 
    in
	case lst of
	    [] => []
	  | xs::xss => helper(all_except_option(str, xs), xss, [])
    end

fun similar_names(lst, g as {first=a, middle=b, last=c})=
    let
	fun helper(substs, out)=
	    case substs of
		[] => out
	      | x::xs => helper(xs, [{first=x, middle=b, last=c}] @ out)
    in
	case lst of
	    [] => [g]
	  | x::xs => 
	    helper(get_substitutions2(lst, a), [g])
    end	
	


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(s,_)=
    case s of
	(Diamonds | Hearts) => Red
     |  _=> Black

fun card_value(card)=
    case card of
	(_, Ace) => 11
      | (_, Num(x)) => x
     | _ => 10  


fun remove_card0(cs, c, IllegalMove)=
    case cs of
	[] => raise IllegalMove
      | x::xs => 
       let
	   fun helper(ys, result, counter)=
		case ys of 
		    [] => result
		  | y::ys' => if y <> c
			      then helper(ys', y::result, counter)
			      else if y = c andalso counter < 1
			      then helper(ys', result, counter + 1)
			      else helper(ys', y::result, counter)
	in
	    let val output = helper(cs, [], 0)
	    in
		if length output = length cs
		then raise IllegalMove
		else output
	    end
	end


fun remove_card(cs, c, IllegalMove)=
       case cs of
	   [] => raise IllegalMove
	 | x::xs => if (x = c) then xs else x::remove_card(xs, c, IllegalMove)


fun all_same_color(list)=
    case list of
	[] => true
      | (x::[]) => true
      | (x::x'::xs) => (card_color(x) = card_color(x') andalso all_same_color(x'::xs)) 	


fun sum_cards(list)=
    case list of
	[] => 0
      | x::xs =>
	let
	    fun helper(pointer, result)=
		case pointer of
		    [] => result
		  | y::ys => helper(ys, card_value(y) + result) 
	in
	    helper(list, 0)
	end
	    
fun score(cs : card list, goal : int) = 
    let
	val sum = sum_cards(cs)
	val pre = if (sum > goal) then 3 * (sum - goal) else (goal - sum)
    in
	if (all_same_color cs) then pre div 2 else pre
    end


fun officiate(cards_list, moves_list, goal)=
    case cards_list of
	[] => raise List.Empty
      | x::xs =>
	let
	    fun helper(deck : card list, moves : move list, in_hand : card list, sum : int)=
	        case ((deck, moves, in_hand, sum), sum > goal) of
		    (([],_,_,_), _) => score(in_hand, goal)
		  | ((_,[],_,_), _) => score(in_hand, goal) 
		  | ((_,_,_,_), true) => score(in_hand, goal)  
		  | ((d::ds, m::ms, _, _), _) => case m of
						     Draw => helper(ds, ms, (d::in_hand), sum_cards(d::in_hand))
						   | Discard(c) => helper(deck, ms, remove_card(in_hand, c, IllegalMove),sum - card_value(c))
	in
	    helper(cards_list, moves_list, [], sum_cards([]))
	end
