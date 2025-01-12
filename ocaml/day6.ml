open Utils

type heading = N | E | S | W
type tile = F | B | G | X of heading list | O
type player = { mutable x : int; mutable y : int; mutable h : heading }

let mk_player (x, y) h = { x; y; h }

let p =
  let open P in
  each_line
  @@ many (pure F <* char '.' <|> (pure B <* char '#') <|> (pure G <* char '^'))

let lookup (board : tile array array) i j =
  let open O in
  let* xs = A.get_safe board j in
  A.get_safe xs i

let my_set (board : tile array array) i j x =
  if O.is_some (lookup board i j) then
    board.(j).(i) <- x
  else
    ()

let turn player =
  player.h <- (match player.h with N -> E | E -> S | S -> W | W -> N)

let move player dx dy =
  player.x <- player.x + dx;
  player.y <- player.y + dy

let string_of_heading = function N -> "↑" | E -> "→" | S -> "↓" | W -> "←"
let string_of_tile = function
  | F -> "."
  | G -> "^"
  | B -> "#"
  (* | X hs -> string_of_int (L.length hs) *)
  | X [] -> "?"
  | X [ h ] -> string_of_heading h
  | X _ -> "+"
  | O -> "O"

let print_board =
  let open F in
  A.iter (print_newline % A.iter (print_string % string_of_tile))

let find_index2 p board =
  let open A in
  find_mapi
    (fun j ts -> find_mapi (fun i x -> if p x then Some (i, j) else None) ts)
    board

let walk board player =
  let rec go () =
    let dx, dy =
      match player.h with
      | N -> (0, -1)
      | E -> (1, 0)
      | S -> (0, 1)
      | W -> (-1, 0)
    in
    let new_x, new_y = (player.x + dx, player.y + dy) in
    (* Check what she's getting into *)
    match lookup board new_x new_y with
    | Some (B | O) ->
        turn player;
        go ()
    | Some (F | G) ->
        my_set board new_x new_y (X [ player.h ]);
        move player dx dy;
        go ()
    | Some (X hs) ->
        if L.mem player.h hs then
          (* been there, headed the same way, she must be going in a loop *)
          `Loop (new_x, new_y, player.h)
        else (
          my_set board new_x new_y (X (player.h :: hs));
          move player dx dy;
          go ())
    | _ -> `WanderOff
  in
  go ()

let get_board input =
  let open R in
  let* level = P.parse_file p input in
  let board = level |> L.filter (( <> ) []) |> L.map A.of_list |> A.of_list in
  let* start = find_index2 (( = ) G) board |> R.of_opt in
  let p = mk_player start N in
  pure (board, p)

let solve1 input =
  let open R in
  let* board, p = get_board input in
  ignore @@ walk board p;
  print_board board;
  let total =
    A.(fold (fold (fun acc t -> acc + match t with X _ -> 1 | _ -> 0)) 0)
      board
  in
  pure (total + 1)

let solve2 input =
  let open R in
  let* board, _ = get_board input in
  pure
  @@
  let open A in
  (* for every F cell, add a O blockade and walk the guard: if the guard enters a loop, add 1 to the total, otherwise 0 *)
  foldi
    (fun acc1 j ts ->
      foldi
        (fun acc2 i t ->
          acc2
          +
          match t with
          | F -> (
              let b, p = get_board input |> R.get_exn in
              my_set b i j O;
              match walk b p with
              | `Loop (x, y, _) ->
                  print_board b;
                  Printf.printf "blocked {%d,%d}\n" i j;
                  Printf.printf "loop at {%d,%d}\n" x y;
                  print_newline ();
                  1
              | _ -> 0)
          | _ -> 0)
        acc1 ts)
    0 board

let l () =
  P.parse_file p (data "6.txt") |> Result.get_ok
  |> L.filter (( <> ) [])
  |> L.map A.of_list |> A.of_list

let g = mk_player (4, 6) N
