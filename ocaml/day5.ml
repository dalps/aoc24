module L = CCList
module O = CCOption
module P = CCParse
module R = CCResult

let pa = P.(many (both (U.int <* char '|') U.int))

let pl = P.(each_line (sep ~by:(char ',') U.int))

let compare_assoc order x y : int =
  if L.mem (x, y) order then
    -1
  else if L.mem (y, x) order then
    1
  else
    0

let solve1 input_order input_pages =
  let open R in
  let* order = P.parse_file pa input_order in
  let* pages = P.parse_file pl input_pages in
  L.(filter (is_sorted ~cmp:(compare_assoc order))) pages
  |> L.(map (fun ps -> nth_opt ps (length ps / 2)))
  |> L.keep_some |> L.fold_left ( + ) 0 |> return

let solve2 input_order input_pages =
  let open R in
  let* order = P.parse_file pa input_order in
  let* pages = P.parse_file pl input_pages in
  L.(filter CCFun.(not % is_sorted ~cmp:(compare_assoc order))) pages
  |> L.(map (sort (compare_assoc order)))
  |> L.(map (fun ps -> nth_opt ps (length ps / 2)))
  |> L.keep_some |> L.fold_left ( + ) 0 |> return

let assoc = P.parse_file pa "5a.txt" |> Result.get_ok
let pages = P.parse_file pl "5b.txt" |> Result.get_ok
