open Utils

type robot = { mutable p : P2.t } [@@deriving show]
type box = { id : int; mutable p : P2.t } [@@deriving show]
type space = W | B of int | E | P [@@deriving show]

let string_of_space = function
  | W -> "#"
  | B i -> digits_of_int i |> L.rev |> L.hd |> string_of_int
  | P -> "@"
  | _ -> "."

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
      (many (any_char >|= function '#' -> W | 'O' -> B 0 | '@' -> P | _ -> E)))

let get_data input_a input_b =
  let open L in
  let board = P.parse_file_exn p_board (data input_a) |> AM.of_list in
  let program = P.parse_file_exn p_program (data input_b) |> fst |> rev in
  (board, program)

let b, p = get_data "15a.txt" "15b.txt"

let get_boxes (board : space AM.t) =
  let open AM in
  let boxes = Hashtbl.create 99 in
  A.foldi
    (fun acc y row ->
      let r = A.to_list row in
      let rec search_row x i row : int =
        match row with
        | [] | [ _ ] -> i
        | B _ :: B _ :: xs ->
            board.-(P2.v x y) <- B i;
            board.-(P2.v (x + 1) y) <- B i;
            Hashtbl.add boxes i { id = i; p = P2.v x y };
            search_row (x + 2) (i + 1) xs
        | _ :: (B _ as b) :: xs -> search_row (x + 1) i (b :: xs)
        | _ :: _ :: xs -> search_row (x + 2) i xs
      in
      search_row 0 acc r)
    0 board
  |> ignore;
  boxes

let boxes_intersect (b1 : box) (b2 : box) : bool =
  let diff = P2.(b1.p - b2.p) in
  not (abs diff.x <= 1 && abs diff.y <= 1)

let rec push_box board dir box_pos =
  let open AM in
  let open P2 in
  let next = box_pos + compass.(dir) in
  board.-(next) |> O.get_exn_or "box OoB" |> function
  | E ->
      board.-(next) <- B 0;
      `Continue
  | B _ -> (
      match push_box board dir next with
      | `Continue ->
          board.-(next) <- B 0;
          `Continue
      | `Stop -> `Stop)
  | _ -> `Stop

let double_board =
  A.map (A.flat_map (function P -> [| P; E |] | x -> [| x; x |]))

let print_board = AM.print ~print_a:F.(print_string % string_of_space)

let%expect_test "test_double_board" =
  let b2 = double_board b in
  print_board b2

let move_box board (box : box) dir =
  let open AM in
  let open P2 in
  let p' = box.p + dir in
  if dir = north || dir = south then (
    board.-(p') <- B box.id;
    board.-(p' + east) <- B box.id;
    board.-(box.p) <- E;
    board.-(box.p + east) <- E)
  else if dir = west then (
    board.-(p') <- B box.id;
    board.-(box.p - dir) <- E)
  else (
    board.-(p') <- B box.id;
    board.-(p' + east) <- B box.id;
    board.-(box.p - dir) <- E);
  box.p <- p'

let move_bot board (bot : robot) dir =
  let open AM in
  let open P2 in
  let p' = bot.p + dir in
  board.-(bot.p) <- E;
  board.-(p') <- P;
  bot.p <- p'

let check_adjacent (board : space AM.t) box dir =
  let open P2 in
  let open AM in
  let open R in
  pr "Checking adjacencies at %s\n" (to_string box);
  let vs =
    if dir = north || dir = south then
      [ box + dir; box + dir + east ]
    else if dir = west then
      [ box + dir ]
    else
      [ box + (2 *. dir) ]
  in
  L.map (fun v -> (v, board.-(v))) vs |> function
  | (_, None) :: _ | (_, Some W) :: _ -> fail_printf "Touching wall"
  | [ (p, Some (B id)) ] -> pure [ { id; p } ]
  | (v1, Some (B b1)) :: (v2, Some (B b2)) :: _ ->
      if b1 = b2 then
        (* Returning new references!!! *)
        pure [ { id = b1; p = v1 } ]
      else
        pure [ { id = b1; p = v1 + west }; { id = b2; p = v2 } ]
  | _ ->
      pr "Seems to be empty...\n";
      pure []

let rec push_boxes q board dir box =
  let open P2 in
  let open R in
  let* nexts = check_adjacent board box.p compass.(dir) in
  let* _ =
    Utils.pr "Found %d adjacent boxes to box %d\n" (L.length nexts) box.id;
    L.fold_left
      (fun acc box' ->
        let* _ = acc in
        pr "Good to go\n";
        push_boxes q board dir box')
      (Ok q) nexts
  in
  pr "Scheduled move for box %d\n" box.id;
  Queue.add (fun () -> move_box board box compass.(dir)) q;
  pure q

let exec1 board (bot : robot) boxes =
  let open AM in
  let open R in
  fun dir ->
    let open P2 in
    let next = bot.p + compass.(dir) in
    let o = board.-(next) |> O.get_exn_or "player OoB" in
    match o with
    | E -> move_bot board bot compass.(dir)
    | B id ->
        pr "Hit box %d.\n" id;
        let box = Hashtbl.find boxes id in
        ignore
        @@
        let q = Queue.create () in
        let* q = push_boxes q board dir box in
        pr "Moving boxes...\n";
        Queue.iter (fun move -> move ()) q;
        pr "Boxes moved.\n";
        move_bot board bot compass.(dir);
        pure ()
    | _ -> ()

let exec board program =
  let open AM in
  let boxes = get_boxes board in
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
    (fun i ->
      exec1 board bot boxes i;
      print_board board)
    program

let%expect_test "test_exec" =
  let open L in
  let p = P.parse_file_exn p_program (data "15b.txt") |> fst |> rev in
  let b2 = double_board b in
  exec b2 (take 200 p)

let solve1 input_a input_b =
  let b, p = get_data input_a input_b in
  exec b p;
  let boxes =
    AM.filter_mapij (fun x y -> function B _ -> Some (x, y) | _ -> None) b
  in
  L.fold_left (fun acc (x, y) -> (100 * y) + x + acc) 0 boxes

(* let%expect_test "solve1" =
     let r = solve1 "15a.txt" "15b.txt" in
     print_int r;
     [%expect {| 10092 |}]

   let%expect_test "solve1'" =
     let r = solve1 "day15.input" "day15.moves" in
     print_int r *)
