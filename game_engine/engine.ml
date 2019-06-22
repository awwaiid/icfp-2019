open Printf
open Yojson.Basic.Util

exception Error of string
exception FatalError of string

(* type game_map_t = location list *)
(* type booster_location = booster_code * location *)
(* type obstacles = game_map_t list *)
(* type boosters = game_map_t list *)
(* type task = { *)
(*   map: game_map_t, *)
(*   start: location, *)
(*   obstacles: obstacles, *)
(*   boosters: boosters *)
(* } *)

type location = int * int
let location_to_json (x,y) = `List [ `Int x; `Int y ]
let location_to_string (x,y) = sprintf "(%d,%d)" x y

type booster = B | F | L | X | R | C
type booster_loc = int * int * booster

let booster_to_string = function
  | B -> "B"
  | C -> "C"
  | F -> "F"
  | L -> "L"
  | X -> "X"
  | R -> "R"

let booster_to_json booster = `String (booster_to_string booster)
let booster_loc_to_json booster_loc =
  let (x, y, booster) = booster_loc in
  `List [ `Int x; `Int y; `String (booster_to_string booster) ]

let booster_of_string = function
  | "B" -> B
  | "C" -> C
  | "F" -> F
  | "L" -> L
  | "X" -> X
  | "R" -> R
  | _ as s -> raise (Error ("Invalid Booster: " ^ s))

type cell =
  | Wall
  | Obstacle
  | Unwrapped
  | Wrapped

let cell_to_string = function
	| Obstacle -> "O"
	| Wall -> "W"
  | Unwrapped -> "-"
  | Wrapped -> "+"

module World = Map.Make(struct
  type t = location
  let compare = compare
end)

  type active_booster = {
    booster: booster;
    time_left: int;
  }

  let active_booster_to_json active_booster =
    `Assoc [
      "booster", booster_to_json active_booster.booster;
      "time_left", `Int active_booster.time_left;
    ]

  type direction = Up | Right | Down | Left
  let direction_to_string = function
    | Up -> "^"
    | Right -> ">"
    | Left -> "<"
    | Down -> "v"
  let direction_rotate_clockwise = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
  let direction_rotate_counterclockwise = function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

  type worker = {
    position: location;
    direction: direction;
    active_boosters: active_booster list;
    manipulators: location list;
  }

  let worker_to_json worker =
    `Assoc [
      "position", location_to_json worker.position;
      "active_boosters", `List ( List.map active_booster_to_json worker.active_boosters );
      "manipulators", `List ( List.map location_to_json worker.manipulators );
      "direction", `String (direction_to_string worker.direction);
    ]

  let initial_worker position = {
    position = position;
    manipulators = [ (0,0); (1,0); (1,1); (1,-1) ];
    active_boosters = [];
    direction = Right;
  }

type game_state = {
  status: string;
  world: cell World.t;
  world_width: int;
  world_height: int;
  bot_position: location;
  inventory: booster list;
  boosters: booster_loc list;
  action_string: string;
  workers: worker list;
}

let inventory_to_json inventory =
  `List ( List.map booster_to_json inventory )

let ray_intersects (ptx, pty) ((ix, iy), (jx, jy)) =
	(
    ((iy <= pty) && (pty < jy))
    || ((jy <= pty) && (pty < iy))
  ) && (ptx < (jx - ix) * (pty - iy) / (jy - iy) + ix)

let inside_polygon polygon pt =
  let shifted_polygon = (List.tl polygon) @ [List.hd polygon] in
  let polygon_pairs = List.combine polygon shifted_polygon in
  (List.length (List.filter (ray_intersects pt) polygon_pairs)) mod 2 == 1

let is_booster_at boosters (x,y) =
  List.exists (fun (a, b, booster) -> x == a && y == b) boosters

let booster_at boosters (x,y) =
  let (a, b, booster) = List.find (fun (a, b, booster) -> x == a && y == b) boosters in
  booster

let prob_game_map prob_json =
  prob_json
  |> member "contour"
  |> convert_each (fun n -> (index 0 n |> to_int), (index 1 n |> to_int))

let prob_boosters prob_json =
  prob_json
  |> member "boosters"
  |> convert_each (
    fun n ->
      let x = index 0 n in
      let y = index 1 n in
      let t = index 2 n in
      (to_int x, to_int y, booster_of_string (to_string t))
  )

let prob_start_loc prob_json =
  let start_loc_x = prob_json
    |> member "initial_loc"
    |> index 0
    |> to_int
  in
  let start_loc_y = prob_json
    |> member "initial_loc"
    |> index 1
    |> to_int
  in
  (start_loc_x, start_loc_y)

let prob_obstacles prob_json =
  prob_json
  |> member "obstacles"
  |> convert_each (
    fun obstacle ->
      obstacle |> convert_each (
        fun coord ->
          let x = index 0 coord in
          let y = index 1 coord in
          (to_int x, to_int y)
      )
  )

let initialize_state command_stream =
  let prob_json = Stream.next command_stream in

  let game_map = prob_game_map prob_json in
  let boosters = prob_boosters prob_json in
  let start_loc = prob_start_loc prob_json in
  let obstacles = prob_obstacles prob_json in

  let x_coords = List.map (fun (x,y) -> x) game_map in
  let width = List.fold_left max (List.hd x_coords) (List.tl x_coords) in

  let y_coords = List.map (fun (x,y) -> y) game_map in
  let height = List.fold_left max (List.hd y_coords) (List.tl y_coords) in

  let world = ref (World.empty) in

  for y = height - 1 downto 0 do
    for x = 0 to width - 1 do
      if List.exists (fun p -> inside_polygon p (x, y)) obstacles then begin
        world := World.add (x,y) Obstacle !world;
      end else if not (inside_polygon game_map (x, y)) then
				world := World.add (x,y) Wall !world
			(* else if is_booster_at boosters (x,y) then *)
			(* 	world := World.add (x,y) (Booster(booster_at boosters (x,y))) !world *)
      else
				world := World.add (x,y) Unwrapped !world
    done
  done;

  let game_state = {
    status = "OK";
    world = !world;
    world_width = width;
    world_height = height;
    bot_position = start_loc;
    boosters = boosters;
    inventory = [];
    action_string = "";
    workers = [ initial_worker start_loc ];
  } in

  game_state

let print_map world width height =
	for y = height - 1 downto 0 do
		for x = 0 to width - 1 do
			printf "%s" (cell_to_string (World.find (x,y) world))
		done;
		printf "\n"
	done

let game_state_to_string state =
  let s = ref "" in
  let worker = List.hd state.workers in
  let bot_position = worker.position in
	for y = state.world_height - 1 downto 0 do
		for x = 0 to state.world_width - 1 do
      if bot_position = (x,y) then
        s := !s ^ (direction_to_string worker.direction)
      else if is_booster_at state.boosters (x,y) then
        s := !s ^ (booster_to_string (booster_at state.boosters (x,y)))
      else
        s := !s ^ (sprintf "%s" (cell_to_string (World.find (x,y) state.world)))
		done;
    s := !s ^ (sprintf "\n")
  done;
  !s

let game_state_map_to_json state =
  let s = ref [] in
	for x = 0 to state.world_width - 1 do
    let col = ref [] in
    for y = 0 to state.world_height - 1 do
      col := !col @ [`String (cell_to_string (World.find (x,y) state.world))]
		done;
    s := !s @ [ `List !col ]
  done;
  `List !s

let print_game_state state =
  printf "%s" (game_state_to_string state)

let unwrapped_cells world =
  let unwrapped = World.filter (fun k v -> v == Unwrapped) world in
  List.map fst (World.bindings unwrapped)

let state_to_json state =
  `Assoc [
    "status", `String state.status;
    "state_string", `String (game_state_to_string state);
    "bot_position", location_to_json (List.hd state.workers).position;
    "map", game_state_map_to_json state;
    "map_width", `Int state.world_width;
    "map_height", `Int state.world_height;
    "inventory", inventory_to_json state.inventory;
    "boosters", `List ( List.map booster_loc_to_json state.boosters );
    "action_string", `String state.action_string;
    "workers", `List ( List.map worker_to_json state.workers );
    "unwrapped_cells", `List ( List.map location_to_json (unwrapped_cells state.world) );
  ]

let print_game_state_json state =
  Yojson.Basic.to_channel stdout (state_to_json state);
  printf "\n";
  flush stdout

let set_elem lst index new_value =
  List.mapi (fun index' el -> if index = index' then new_value else el) lst

let perform_action_move_up game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let (x,y) = worker.position in
  let worker = { worker with position = (x, y + 1) } in
  let workers = set_elem game_state.workers worker_num worker in
  { game_state with workers = workers }

let perform_action_move_down game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let (x,y) = worker.position in
  let worker = { worker with position = (x, y - 1) } in
  let workers = set_elem game_state.workers worker_num worker in
  { game_state with workers = workers }

let perform_action_move_left game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let (x,y) = worker.position in
  let worker = { worker with position = (x - 1, y) } in
  let workers = set_elem game_state.workers worker_num worker in
  { game_state with workers = workers }

let perform_action_move_right game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let (x,y) = worker.position in
  let worker = { worker with position = (x + 1, y) } in
  let workers = set_elem game_state.workers worker_num worker in
  { game_state with workers = workers }

let perform_action_do_nothing game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let workers = set_elem game_state.workers worker_num worker in
  { game_state with workers = workers }

let rotate_clockwise (x,y) = (y, -x)
let rotate_counterclockwise (x,y) = (-y, x)

let perform_action_turn_clockwise game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let rotated_manipulators = List.map rotate_clockwise worker.manipulators in
  let worker = { worker with manipulators = rotated_manipulators } in
  let worker = { worker with direction = direction_rotate_clockwise worker.direction } in
  let workers = set_elem game_state.workers worker_num worker in
  { game_state with workers = workers }

let perform_action_turn_counterclockwise game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let rotated_manipulators = List.map rotate_counterclockwise worker.manipulators in
  let worker = { worker with manipulators = rotated_manipulators } in
  let worker = { worker with direction = direction_rotate_counterclockwise worker.direction } in
  let workers = set_elem game_state.workers worker_num worker in
  { game_state with workers = workers }

let record_action game_state action =
  let action_string = game_state.action_string in
  { game_state with action_string = action_string ^ action }

let covered_cells (x1, y1) (x2, y2) =
  let x1 = (float_of_int x1) +. 0.5 in
  let y1 = (float_of_int y1) +. 0.5 in
  let x2 = (float_of_int x2) +. 0.5 in
  let y2 = (float_of_int y2) +. 0.5 in
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  let step = if abs_float dx >= abs_float dy then abs_float dx else abs_float dy in
  let dx = dx /. step in
  let dy = dy /. step in
  let x = ref x1 in
  let y = ref y1 in
  let covered = ref [] in
  for i = 0 to (int_of_float step) do
    covered := (int_of_float !x, int_of_float !y)::(!covered);
    x := !x +. dx;
    y := !y +. dy;
  done;
  !covered

let is_transparent world (x, y) =
  try
    let cell = World.find (x, y) world in
    match cell with
    | Obstacle -> false
    | Wall -> false
    | Unwrapped -> true
    | Wrapped -> true
  with Not_found -> true

let is_visible world (x1, y1) (x2, y2) =
  let cells = covered_cells (x1, y1) (x2, y2) in
  List.exists (is_transparent world) cells

let manipulator_positions worker =
  let (x,y) = worker.position in
  List.map (fun (i,j) -> (x+i, y+j)) worker.manipulators

let visible_manipulator_positions world worker =
  List.filter (is_visible world worker.position) (manipulator_positions worker)

(* Only wrap unwrapped locations, ignore the rest *)
let update_location_wrapped world location =
  try
    if World.find location world = Unwrapped then
      World.add location Wrapped world
    else
      world
  with Not_found -> world (* Don't worry about the edge of the world *)

let update_wrapped_state_worker game_state worker =
  let updated_world = List.fold_left
    update_location_wrapped
    game_state.world
    (visible_manipulator_positions game_state.world worker) in
  { game_state with world = updated_world }

let update_wrapped_state game_state =
  List.fold_left update_wrapped_state_worker game_state game_state.workers

let pick_up_boosters game_state worker_num =
  let worker = List.nth game_state.workers worker_num in
  let (x, y) = worker.position in
  let (found_boosters, unfound_boosters) =
    List.partition
    (fun (a, b, booster) -> a == x && b == y && booster != X)
    game_state.boosters in
  let found_booster_locs = List.map (fun (a, b, booster) -> a,b) found_boosters in
  let world = List.fold_left (fun world loc -> World.add loc Wrapped world) game_state.world found_booster_locs in
  { game_state with
    world = world;
    boosters = unfound_boosters;
    inventory = game_state.inventory @ (List.map (fun (a, b, booster) -> booster) found_boosters);
  }

let validate_location game_state worker_num =
  try
    let worker = List.nth game_state.workers worker_num in
    let (x,y) = worker.position in
    let cell = World.find worker.position game_state.world in
    if cell = Obstacle || cell = Wall || x > game_state.world_width || y > game_state.world_height || x < 0 || y < 0 then
      raise (Error "Invalid state")
    else
      game_state
  with _ -> raise (Error "Invalid state")

let perform_action cmd_json game_state worker_num =
    let action = cmd_json |> member "action" |> to_string in
    let game_state = (match action with
      | "W" -> perform_action_move_up game_state worker_num
      | "S" -> perform_action_move_down game_state worker_num
      | "A" -> perform_action_move_left game_state worker_num
      | "D" -> perform_action_move_right game_state worker_num
      | "Z" -> perform_action_do_nothing game_state worker_num
      | "E" -> perform_action_turn_clockwise game_state worker_num
      | "Q" -> perform_action_turn_counterclockwise game_state worker_num
      | _ -> raise (FatalError ("Unknown or unimplemented action: " ^ action))
    ) in
    let game_state = update_wrapped_state game_state in
    let game_state = pick_up_boosters game_state worker_num in
    let game_state = validate_location game_state worker_num in
    let game_state = record_action game_state action in
    game_state


let main () =

  eprintf "started\n%!";

  let command_stream = Yojson.Basic.stream_from_channel stdin in
  let game_state = ref (initialize_state command_stream) in
  game_state := update_wrapped_state !game_state; (* Time step zero update? *)
  printf "{\"status\": \"loaded\"}\n%!";
  eprintf "read game state\n%!";

  while true do
    flush stdout;
    try
      let cmd_json = Stream.next command_stream in
      let cmd = cmd_json |> member "cmd" |> to_string in
      (match cmd with
      | "print_state" -> print_game_state !game_state
      | "get_state" -> ()
      | "action" ->
          game_state := perform_action cmd_json !game_state 0
      (* | "get_path" -> get_path_cmd cmd_json game_state *)
      | "exit" -> exit 0
      | _ -> raise (Error ("Unknown command: " ^ cmd))
      );
      print_game_state_json !game_state
    with Error(m) ->
      game_state := { !game_state with status = ("error: " ^ m) };
      print_game_state_json !game_state
  done

let () = main ()
