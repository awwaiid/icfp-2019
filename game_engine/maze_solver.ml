
(* ========================================================================== *
 * Domain-specific functions
 * ========================================================================== *)

(* open Astar *)

let findi f =
  let rec aux i = function
    | [] -> None
    | xi :: l -> match f i xi with None -> aux (i+1) l | Some _ as res -> res
  in
  aux 0

let findij f = findi (fun i -> findi (f i))

let at x (i, j) = findij (fun k l e -> if i=k && j=l then Some e else None) x

let get_next_states field (i, j) =
  [(i-1, j); (i+1, j); (i, j-1); (i, j+1)]
  |> List.filter (fun pos -> match at field pos with
      | None | Some '#' -> false
      | Some _ -> true)

let draw_path field states =
  let mapij f = List.mapi (fun i -> List.mapi (f i)) in
  List.fold_left (fun acc (i, j) ->
      mapij (fun i' j' xij -> if i=i' && j=j' && xij=' ' then '*' else xij) acc)
    field states

(** Solve a given maze. *)
let solve field =
  let open Astar in
  let eq_char c i j xij = if xij = c then Some (i,j) else None in
  match (findij (eq_char 'S') field), (findij (eq_char 'G') field) with
  | Some start, Some goal ->
    let cost (i, j) (k, l) = abs (i-k) + abs (j-l) in (* Manhattan distance *)
    let problem = {
      Astar.cost = cost;
      Astar.goal = goal;
      Astar.get_next_states = get_next_states field;
    } in
    draw_path field (Astar.search problem start)
  | _ -> failwith "Not found goal and/or start"

(** Convert a string into a list of characters. *)
let explode s =
  let rec aux i l = if i < 0 then l else aux (i - 1) (s.[i] :: l) in
  aux (String.length s - 1) []

let () =
  ["#####     ###########           #       ";
   "#     ###       # #     ####### # ### # ";
   "# ### ####  # # # # # # #       # # # # ";
   "#   #   ### # # # # # # ##### # # # # # ";
   "### # #S####### # # # #       #     # # ";
   "    # ###         # # ####### ##### # # ";
   "  ###   # ######### #   #   #   #   # # ";
   "    ##### # ##   #  # # # # ### # # # # ";
   "### #   # #    #    # # # # # # # # # # ";
   "    # # # # ########### # # # # # ######";
   " #### #   #    # #   #  # #   # #  #    ";
   "      # ######   # # #  # # # # #  # ## ";
   "#######   #    # # #    # # # # #  # #  ";
   "        # # #  # # # #### ### # #    # #";
   " ########   # #### # #        #    ###  ";
   "        #######    ###  ##### # #    ## ";
   " #####                        # # ## #G "]
  |> List.map explode
  |> solve
  |> List.map (List.map (Bytes.make 1))
  |> List.map (String.concat " ")
  |> String.concat "\n"
  |> print_endline
