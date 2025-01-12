open Utils

let p =
  P.(
    each_line @@ try_opt
    @@ both
         (string "p=" *> both (U.int <* char ',') U.int)
         (string " v=" *> both (U.int <* char ',') U.int))

type robot = { mutable position : Coord.t; velocity : Coord.t }
[@@deriving show]

let mk_robot position velocity : robot = { position; velocity }
let tiles = Coord.mk 101 103
let center = Coord.(tiles /. 2)

let update_pos robot =
  robot.position <- Coord.((robot.position + robot.velocity + tiles) % tiles)

let get_robots input =
  let open L in
  let robots =
    P.parse_file p input |> R.get_exn |> keep_some
    >|= fun ((px, py), (vx, vy)) ->
    mk_robot (Coord.make px py) (Coord.make vx vy)
  in
  robots

(* South East, clockwise *)
let quadrants =
  [ (( > ), ( > )); (( < ), ( > )); (( < ), ( < )); (( > ), ( < )) ]

let check_quadrants robots =
  let open L in
  foldi
    (fun acc i (fx, fy) ->
      let n_bots =
        robots
        |> filter (fun (r : robot) ->
               fx r.position.x center.x && fy r.position.y center.y)
        |> length
      in
      pr "Found %d bots in quadrant %d\n" n_bots i;
      n_bots * acc)
    1 quadrants

let solve1 input =
  let open L in
  let robots = get_robots input in
  for _ = 1 to 100 do
    iter update_pos robots
  done;
  check_quadrants robots
