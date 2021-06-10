(* Ruchita Sonawale, hw1, 2021*)

fun is_older (a: int*int*int, b: int*int*int) =
    if #1 a < #1 b  
    then true
    else
	if #1 a = #1 b andalso #2 a < #2 b
	then true
	else
	    if #2 a = #2 b andalso #3 a < #3 b
	    then true
	    else false;

fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates then 0
    else
      if #2 (hd dates) = month
      then 1 + number_in_month (tl dates, month)
      else number_in_month (tl dates, month);

fun number_in_months (dates: (int * int * int )list, months : int list) =
    if null dates then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months); 
    
fun dates_in_month (dates: (int * int * int) list, month : int) = 
    if null dates then []
    else
	if #2 (hd dates) = month then hd dates :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month);

fun dates_in_months (dates: (int * int * int) list, months : int list) = 
    if null dates then []
    else
	dates_in_month( dates,hd months) @ dates_in_months(dates,tl  months);
       
fun get_nth ( e : string list, n : int) =
    if n = 1 then hd e
    else get_nth(tl e, n-1)

fun date_to_string( date : int*int*int) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August",          " September", "October" , "November", "December"]
    in
	get_nth(months, #2 date)
	^ " " ^ Int.toString(#3 date)
	^ ", " ^ Int.toString(#1 date)
			     
    end

fun number_before_reaching_sum ( sum: int, numbers : int list) =
    let
	fun count(numbers: int list, acc: int, n: int) =
	    if hd numbers + acc >= sum then n
	    else count(tl numbers, acc + hd numbers, n + 1)
    in
	count(numbers, 0, 0)
    end

fun what_month(days: int) =
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(days, days_in_month) + 1
    end

fun month_range(day1: int, day2: int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 +1, day2)

fun oldest(dates: (int*int*int) list) =
    if null dates then NONE
    else
	let
	    val tl_ans = oldest(tl dates)
	in
	    if isSome tl_ans andalso is_older(valOf tl_ans, hd dates) then tl_ans
	    else SOME (hd dates)
	end

fun delete(e: int, elements: int list) =
  if null elements then []
  else
    if e = hd elements then delete(e, tl elements)
    else hd elements :: delete(e, tl elements)


fun removeDuplicates(months: int list) =
  if null months then []
  else hd months :: removeDuplicates(delete(hd months, tl months))

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
  number_in_months(dates, removeDuplicates(months))

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
  dates_in_months(dates, removeDuplicates(months))

fun reasonable_date(date: int*int*int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val days_in_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    fun is_leap(year: int) =
      year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0

    fun get_nth(ints: int list, n: int) =
      if n = 1 then hd ints
      else get_nth(tl ints, n - 1)

    fun is_valid_year(year: int) = year > 0
    fun is_valid_month(month: int) = month >= 1 andalso month <= 12
    fun is_valid_day(day: int) = day >= 1 andalso day <= 31

    fun is_valid_date() =
      if is_leap(#1 date) then (#3 date) <= get_nth(days_in_month_leap, #2 date)
      else (#3 date) <= get_nth(days_in_month, #2 date)
  in
    is_valid_year(#1 date) andalso is_valid_month(#2 date)
    andalso is_valid_day(#3 date) andalso is_valid_date()
  end
