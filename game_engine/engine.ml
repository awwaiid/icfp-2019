open Printf
open Yojson.Basic.Util

exception Error of string

type location = int * int
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

type booster = B | F | L | X | R

let booster_to_string = function
  | B -> "B"
  | F -> "F"
  | L -> "L"
  | X -> "X"
  | R -> "R"

let booster_of_string = function
  | "B" -> B
  | "F" -> F
  | "L" -> L
  | "X" -> X
  | "R" -> R
  | _ as s -> raise (Error ("Invalid Booster: " ^ s))

type cell =
  | Wall
  | Obstacle
  | Booster of booster
  | Unwrapped
  | Wrapped
  | Teleport

let cell_to_string = function
	| Obstacle -> " "
	| Wall -> "·"
  | Booster b -> booster_to_string b
  | Unwrapped -> "□"
  | Wrapped -> "■"
  | Teleport -> "T"

module World = Map.Make(struct
  type t = location
  let compare = compare
end)

type game_state = {
  world: cell World.t;
  world_width: int;
  world_height: int;
  bot_position: location;
  inventory: booster list
}

let print_map world width height =
	for y = height downto 0 do
		for x = 0 to width do
			printf "%s" (cell_to_string (World.find (x,y) world))
		done;
		printf "\n"
	done

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

let main () =

  let command_stream = Yojson.Basic.stream_from_channel stdin in

  let prob_json = Stream.next command_stream in
  let game_map = prob_json
    |> member "contour"
    |> convert_each (fun n -> (index 0 n |> to_int), (index 1 n |> to_int))
  in

  let boosters = prob_json
    |> member "boosters"
    |> convert_each (
      fun n ->
        let x = index 0 n in
        let y = index 1 n in
        let t = index 2 n in
        (to_int x, to_int y, booster_of_string (to_string t))
    )
  in

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
  let start_loc = (start_loc_x, start_loc_y) in

  let obstacles = prob_json
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
  in

  (* Dum.to_stdout game_map; *)
  (* Dum.to_stdout boosters; *)
  (* Dum.to_stdout start_loc; *)
  (* Dum.to_stdout obstacles; *)

  let x_coords = List.map (fun (x,y) -> x) game_map in
  let width = List.fold_left max (List.hd x_coords) (List.tl x_coords) in
  (* Dum.to_stdout width; *)

  let y_coords = List.map (fun (x,y) -> y) game_map in
  let height = List.fold_left max (List.hd y_coords) (List.tl y_coords) in
  (* Dum.to_stdout height; *)

  let world = ref (World.empty) in
  (* Dum.to_stdout world; *)

  let game_state = ref {
    world = World.empty;
    world_width = width;
    world_height = height;
    bot_position = start_loc;
    inventory = []
  } in

(* printf "\n\nGame state:"; *)
(* Dum.to_stdout game_state; *)

(*   printf "\n\n"; *)

  for y = height downto 0 do
    for x = 0 to width do
      if List.exists (fun p -> inside_polygon p (x, y)) obstacles then begin
        world := World.add (x,y) Obstacle !world;
      end else if not (inside_polygon game_map (x, y)) then
				world := World.add (x,y) Wall !world
			else if is_booster_at boosters (x,y) then
				world := World.add (x,y) (Booster(booster_at boosters (x,y))) !world
      else
				world := World.add (x,y) Unwrapped !world
    done
  done;

  print_map !world width height


  (* printf game_map *)
  (* printf "%s" game_map *)

  (* while List.length !state.State.bots > 0 do *)
  (*   state := State.execute_step !state trace_stream *)
  (* done *)

let () = main ()
