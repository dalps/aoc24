open Utils

type robot = { mutable p : P2.t } [@@deriving show]
type box = { mutable p : P2.t } [@@deriving show]
type space = W | B | E | P [@@deriving show]

let string_of_space = function W -> "#" | B -> "O" | P -> "@" | _ -> "."

let p_program =
  P.chars_fold
    ~f:(fun acc c ->
      match c with
      | '^' -> `Continue (0 :: acc)
      | '>' -> `Continue (1 :: acc)
      | 'v' -> `Continue (2 :: acc)
      | '<' -> `Continue (3 :: acc)
      | '\n' -> `Continue acc
      | _ -> `Stop [])
    []

let p_board =
  P.(
    each_line
      (many (any_char >|= function '#' -> W | 'O' -> B | '@' -> P | _ -> E)))

let get_data input_a input_b =
  let open L in
  let board = P.parse_file_exn p_board (data input_a) |> AM.of_list in
  let program = P.parse_file_exn p_program (data input_b) |> fst |> rev in
  (board, program)

let rec push_box board dir box_pos =
  let open AM in
  let open P2 in
  let next = box_pos + compass.(dir) in
  board.-(next) |> O.get_exn_or "box OoB" |> function
  | E ->
      board.-(next) <- B;
      `Ok
  | B -> (
      match push_box board dir next with
      | `Ok ->
          board.-(next) <- B;
          `Ok
      | `Stop -> `Stop)
  | _ -> `Stop

let exec board program =
  let open AM in
  let bot =
    find_mapij
      (fun i j -> function
        | P ->
            let p = P2.make i j in
            board.-(p) <- E;
            Some { p }
        | _ -> None)
      board
    |> O.get_exn_or "Player not found"
  in
  let open L in
  iter
    (fun dir ->
      let open P2 in
      let next = bot.p + compass.(dir) in
      let o = board.-(next) |> O.get_exn_or "player OoB" in
      match o with
      | E -> bot.p <- next
      | B -> (
          match push_box board dir next with
          | `Ok ->
              board.-(next) <- E;
              bot.p <- next
          | `Stop -> ())
      | _ -> ())
    program

let solve1 input_a input_b =
  let b, p = get_data input_a input_b in
  exec b p;
  let boxes =
    AM.filter_mapij (fun x y -> function B -> Some (x, y) | _ -> None) b
  in
  L.fold_left (fun acc (x, y) -> (100 * y) + x + acc) 0 boxes

let%expect_test "solve1" =
  let r = solve1 "15a.txt" "15b.txt" in
  print_int r;
  [%expect {| 10092 |}]

let%expect_test "solve1'" =
  let r = solve1 "day15.input" "day15.moves" in
  print_int r;
