
fun is_older (date_1 : (int*int*int), date_2 : (int*int*int)) =
  (* Use orelse operator directly to compare two dates. *)
  (#1 date_1) < (#1 date_2) orelse
  ((#1 date_1) = (#1 date_2) andalso (#2 date_1) < (#2 date_2)) orelse
  ((#1 date_1) = (#1 date_2) andalso (#2 date_1) = (#2 date_2) andalso (#3 date_1) < (#3 date_2))
  

fun number_in_month(date_list : (int*int*int) list, month : int) = 
  if null date_list
  then 0
  else if (#2 (hd date_list)) = month
  then 1 + number_in_month((tl date_list), month)
  (* If the month of the first date in date list is not equal to given month, then just go find
  the rest dates of date list. *)
  else number_in_month((tl date_list), month)
  
fun number_in_months(date_list : (int*int*int) list, months : int list) =
  (* Handle the exception that given date list is empty: return 0 directly. *)
  if null date_list
  then 0
  (* Handle the exception that given month list is empty: return 0 directly. *)
  else if null months
  then 0
  else number_in_month(date_list, (hd months)) + number_in_months(date_list, (tl months))

fun dates_in_month(date_list : (int*int*int) list, month : int) = 
  (* Handle the exception that given date list is empty: return empty list directly. *)
  if null date_list
  then []
  else if (#2 (hd date_list)) = month
  then (hd date_list)::dates_in_month((tl date_list), month)
  (* If the month of the first date in date list is not equal to given month, then just go find
  the rest dates of date list. *)
  else dates_in_month((tl date_list), month)

fun dates_in_months(date_list : (int*int*int) list, months : int list) =
  (* Handle the exception that given date list is empty: return empty list directly. *)
  if null date_list
  then []
  (* Handle the exception that given month list is empty: return empty list directly. *)
  else if null months
  then []
  else 
    (* use local declaration to merge two date list *)
    let fun listMerge (list_1 : (int*int*int) list, list_2 : (int*int*int) list) =
      (* Notice: list_2 return directly may cause mutable problem. Nevertheless,
      we do not concern about this issue here *)
      if null list_1 then list_2
      else (hd list_1)::listMerge((tl list_1), list_2)
    in
      listMerge(dates_in_month(date_list, (hd months)), dates_in_months(date_list, (tl months)))
    end

fun get_nth(string_list : string list, n : int) = 
  if n = 1
  then (hd string_list)
  else get_nth((tl string_list), n - 1)

fun date_to_string(date : (int*int*int)) =
  (* use local declaration to get the month's name *)
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, (#2 date))^(" ")^(Int.toString (#3 date))^(", ")^(Int.toString (#1 date))
  end

fun number_before_reaching_sum(sum : int, num_list : int list) = 
  (* Handle the exception that given sum is 0: return 0 directly. *)
  if sum = 0
  then 0
  (* Handle the exception that given num list is empty: return 0 directly. *)
  else if null num_list
  then 0
  (* Tricky method: if current sum larger than current adding value,
    meaning that current adding value can be added in recursive loop. *)
  else if (hd num_list) < sum
  then 1 + number_before_reaching_sum(sum - (hd num_list), (tl num_list))
  else 0

fun what_month(date : int) =
  number_before_reaching_sum(date, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1
  (* Another method to solve the problem if you do not notice the relationship between
    what_month and number_before_reaching_sum *)
  (*
  let
    val day_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    fun countup(date : int, n : int, days : int list) = 
      if null days then 12
      else if (date - (hd days) > 0) then countup(date - (hd days), n + 1, (tl days))
      else n
  in
    countup(date, 1, day_list)
  end
  *)

fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(date_list : (int*int*int) list) =
  if null date_list
  then NONE
  else
    let val temp = oldest((tl date_list))
    in
      (* Type checking with option *)
      if isSome temp andalso not(is_older((hd date_list), valOf temp)) then temp
      else SOME (hd date_list)
    end
