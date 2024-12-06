module P = CCParse

let p = P.(each_line @@ many (U.int <* many space))

let rec diffs = function
  | [] | [ _ ] -> []
  | n1 :: n2 :: t -> (n1 - n2) :: diffs (n2 :: t)

let safe (ls : int list) =
  ls |> diffs |> fun ds ->
  CCList.(
    for_all (fun d -> 1 <= d && d <= 3) ds
    || for_all (fun d -> -3 <= d && d <= -1) ds)

let safe_damped_naive (ls : int list) =
  let open CCList in
  let* l = ls in
  return @@ safe (remove_one ~eq:Int.equal l ls)

let safe_damped (ls : int list) =
  let open CCList in
  let* i = 0 -- (length ls - 1) in
  return @@ safe (remove_at_idx i ls)

let solve input =
  let open CCResult in
  let+ lls = P.parse_file p input in
  let open CCList in
  let+ ls = filter (fun l -> l <> []) lls in
  safe ls

let solve2 input =
  let open CCResult in
  let+ lls = P.parse_file p input in
  let open CCList in
  let+ ls = filter (fun l -> l <> []) lls in
  List.exists Fun.id (safe_damped ls)
