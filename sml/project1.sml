(* This is my first problem set in sml*)
fun is_older(date1 : int*int*int, date2 : int*int*int)=
    let 
	fun are_equals(dateList1 : int list, dateList2 : int list)=
	    if null dateList1 andalso null dateList2
	    then true
	    else if hd dateList1 <> hd dateList2
	    then false
	    else are_equals(tl dateList1, tl dateList2)

	fun is_older2(dateList1 : int list, dateList2 : int list)=
	    if null dateList1 andalso null dateList2
	    then true
	    else if  hd dateList1 < hd dateList2
	    then true
	    else if hd dateList1 > hd dateList2
	    then false
	    else is_older2(tl dateList1, tl dateList2)
    in
	if are_equals([(#1 date1), (#2 date1), (#3 date1)], [(#1 date2), (#2 date2), (#3 date2)])
	then false
	else is_older2([(#1 date1), (#2 date1), (#3 date1)], [(#1 date2), (#2 date2), (#3 date2)])
    end


fun number_in_month(dates : (int*int*int) list, month : int)=
    let 
	val counter = 0;
	fun helper_function(dates : (int*int*int) list, month : int)=
	    if null dates 
	    then counter
	    else if #2 (hd dates) = month
	    then counter + 1 + helper_function(tl dates, month)
	    else counter + helper_function(tl dates, month)
    in 
	helper_function(dates, month)
    end

    
fun number_in_months(dates : (int*int*int) list, months: int list)=
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(*Probably recursion used in better style, try to apply this to above functions*)
fun dates_in_month(dates : (int*int*int) list, month : int)=
    if null dates
    then []
    else
	let val tl_ans = dates_in_month(tl dates, month)
	in
	    if #2 (hd dates) = month
	    then hd dates :: tl_ans
	    else tl_ans
	end


fun dates_in_months(dates : (int*int*int) list, months: int list)=
    if null months
    then []
    else
	let val tl_ans = dates_in_months(dates, tl months)
	in
	    dates_in_month(dates, hd months) @ tl_ans
	end


fun get_nth(words : string list, nth : int)=
    let
	fun  print_strings(x : string list, y : int)=
	     let val counter = y +1
	     in
		 if counter = nth
		 then hd x
		 else print_strings(tl x, counter)
	     end
    in
	print_strings(words, 0)
    end


fun date_to_string(date : int*int*int)=
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "Descember"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
    end


fun number_before_reaching_sum(sum : int, positive_list : int list)=
    if null positive_list orelse hd positive_list >= sum
    then 0  
    else  
	let
	    fun helper_function(helper_list : int list, helper_count : int, helper_less_sum : int)= 
		let 
		    val counter  = helper_count + 1;
		    val previous_sum = helper_less_sum;
		    val next_sum = previous_sum + hd helper_list;
		in	
		    if next_sum >= sum
		    then helper_count
		    else 
			helper_function(tl helper_list, counter, next_sum)
		end
	in
	    helper_function(positive_list, 0, 0)
	end


fun what_month(day : int)=
     number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1


fun month_range(day1 : int, day2 : int)=
    let
	val distance = day2 - day1;

	fun helper(day : int, counter : int, results : int list)=
	    if counter > distance
	    then results
	    else 
		let
		    val out = results @ (what_month(day) :: [])
		in 
		    helper(day + 1, counter + 1, out)
		end
    in
	helper(day1, 0, [])
    end
    
fun oldest(dates : (int*int*int) list)=
    if null dates 
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else
	let 
	    val tl_ans = oldest(tl dates)
	    fun is_older2(date1 : int*int*int, date2 : int*int*int)=
		if is_older(date1, date2)
		then date1
		else date2
	in
	    if isSome tl_ans
	    then SOME (is_older2(hd dates, valOf tl_ans))
	    else NONE
	end

	
	
