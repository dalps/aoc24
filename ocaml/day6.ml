module L = CCList
module O = CCOption
module P = CCParse
module R = CCResult
module A = CCArray

type tile = F | B | G | X
type heading = N | E | S | W
type player = { mutable x : int; mutable y : int; mutable h : heading }

let mk_player (x, y) h = { x; y; h }

let p =
  let open P in
  each_line
  @@ many (pure F <* char '.' <|> (pure B <* char '#') <|> (pure G <* char '^'))

let lookup (board : tile array array) i j =
  let open O in
  let* xs = A.get_safe board j in
  let* x = A.get_safe xs i in
  pure x

let set (board : tile array array) i j x =
  if O.is_some (lookup board i j) then
    board.(j).(i) <- x
  else
    ()

let turn player =
  player.h <- (match player.h with N -> E | E -> S | S -> W | W -> N)

let move player dx dy =
  player.x <- player.x + dx;
  player.y <- player.y + dy

let print_board =
  let open A in
  iter (fun ts ->
      iter
        (fun t ->
          print_char (match t with F -> '.' | G -> '^' | B -> '#' | X -> 'X'))
        ts;
      print_newline ())

let find_index2 p board =
  let open A in
  find_mapi
    (fun j ts -> find_mapi (fun i x -> if p x then Some (i, j) else None) ts)
    board

let walk board player =
  let rec go () =
    set board player.x player.y X;
    let dx, dy =
      match player.h with
      | N -> (0, -1)
      | E -> (1, 0)
      | S -> (0, 1)
      | W -> (-1, 0)
    in
    match lookup board (player.x + dx) (player.y + dy) with
    | Some B ->
        turn player;
        go ()
    | Some (F | X | G) ->
        move player dx dy;
        go ()
    | _ -> ()
  in
  go ()

let solve input =
  let open R in
  let* level = P.parse_file p input in
  let board = level |> L.filter (( <> ) []) |> L.map A.of_list |> A.of_list in
  let* start = find_index2 (( = ) G) board |> R.of_opt in
  let p = mk_player start N in
  walk board p;
  print_board board;
  let total =
    A.(fold (fold (fun acc t -> acc + if t = X then 1 else 0)) 0) board
  in
  pure total

let l =
  P.parse_file p "6.txt" |> Result.get_ok
  |> L.filter (( <> ) [])
  |> L.map A.of_list |> A.of_list

let g = mk_player (4, 6) N
