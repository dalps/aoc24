open Containers.Parse
open CCList

let distance_bad xs ys =
  let* x = sort Int.compare xs and* y = sort Int.compare ys in
  return (abs (x - y))
(* cartesian product!!! *)

let example = [ (3, 4); (4, 3); (2, 5); (1, 3); (3, 9); (3, 3) ]

let distance (xs, ys) =
  fold_left2
    (fun acc x y -> abs (x - y) + acc)
    0 (sort Int.compare xs) (sort Int.compare ys)

let similarity_score (xs, ys) =
  fold_left (fun acc x -> (x * (filter (Int.equal x) ys |> length)) + acc) 0 xs

let parse_int_pair_list =
  CCParse.U.(
    list ~start:"" ~stop:"" ~sep:""
      (pair ~start:"" ~stop:"" ~sep:"" int int))

let solve (input : string) op =
  let open CCResult in
  let* res = parse_file parse_containers input in
  res |> split |> op |> return
