module L = CCList
module O = CCOption
module P = CCParse
module R = CCResult

let pa = P.(many (both (U.int <* char '|') U.int))

let pl = P.(each_line (sep ~by:(char ',') U.int))

let is_transitive (l : (int * int) list) =
  let open L in
  let* x, y = l and* y', z = l in
  return ((not (Int.equal y y')) || List.mem (x, z) l)

let rec check ord = function
  | [] | [ _ ] -> true
  | a :: b :: xs -> List.mem (a, b) ord && check ord (b :: xs)
(* no *)

let check order pages =
  let open L in
  let* x, y = order in
  let* ps = pages in
  return
  @@
  let open O in
  let* ix = L.find_index (Int.equal x) ps in
  let* iy = L.find_index (Int.equal y) ps in
  if_ (fun _ -> ix <= iy) ps
(* no *)

let check order pages =
  let open L in
  filter_map
    (fun ps ->
      if
        for_all
          (fun (x, y) ->
            let open O in
            for_all (( = ) true)
            @@
            let* ix = L.find_index (Int.equal x) ps in
            let* iy = L.find_index (Int.equal y) ps in
            pure (ix <= iy))
          order
      then
        nth_opt ps (length ps / 2)
      else
        None)
    pages

let solve input_order input_pages =
  let open R in
  let* order = P.parse_file pa input_order in
  let* pages = P.parse_file pl input_pages in
  check order pages |> L.fold_left ( + ) 0 |> return
