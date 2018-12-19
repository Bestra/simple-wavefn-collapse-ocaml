open Base

module Direction = struct
  module T = struct
    type t = 
      | Up
      | Down
      | Left
      | Right
      [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make(T)

  let to_vec direction =
    match direction with
    | Up -> (0, 1)
    | Down -> (0, -1)
    | Left -> (-1, 0)
    | Right -> (1, 0)
end

module Wavefunction = struct

  let init_coefficients size tiles =
    let x, y = size in
    Array.make_matrix ~dimx:x ~dimy:y (Set.of_list (module Char) tiles)

end

module Compatibility = struct
  module T = struct
    type t = char * char * Direction.t [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make(T)
end

(** calculates all the in-bound directions for a given position *)
let valid_dirs (x, y) (w, h): Direction.t list =
  let add_to_list_if_true check item list =
    match check with
    | true -> List.cons item list
    | false -> list
  in

  []
  |> add_to_list_if_true (x > 0) Direction.Left
  |> add_to_list_if_true (x < w - 1) Direction.Right
  |> add_to_list_if_true (y > 0) Direction.Down
  |> add_to_list_if_true (y < h - 1) Direction.Up


module Wavefunction = struct

  (**

  Initializes a 2-D wavefunction matrix of coefficients.

  The matrix has size `size`, and each element of the matrix
  starts with all tiles as possible. No tile is forbidden yet.

  NOTE: coefficients is a slight misnomer, since they are a
  set of possible tiles instead of a tile -> number/bool dict. This
  makes the code a little simpler. We keep the name `coefficients`
  for consistency with other descriptions of Wavefunction Collapse.
  @param size a tuple
   *)
  let init_coefficients (size: int * int) (tiles: char list): 'a array array =
    let w, h = size in
    let coefficients = [||] in

    (List.range 0 w)



end

let nested_foldi (nested_array: 'a array array) ~init ~f =
  Array.foldi nested_array ~init:init ~f:(fun i_outer accum outer ->
    Array.foldi outer ~init:accum ~f:(fun i_inner accum' inner ->
      f (i_outer, i_inner) accum' inner
    )
  )


let parse_example_matrix matrix =
  let matrix_width = Array.length matrix in
  let matrix_height = Array.length (matrix.(0)) in
  let initial_weights = Map.empty (module Char) in
  let initial_compatibilities = Set.empty (module Compatibility) in

  let weights, compatibilities = Array.foldi
    matrix
    ~init:(initial_weights, initial_compatibilities)
    ~f:(fun x (w, c) row ->
      Array.foldi row ~init:(w, c) ~f:(fun y (w', c') cur_tile ->
        let new_weights = Map.update w' cur_tile ~f:(fun tile_weight ->
          match tile_weight with
          | None -> 1
          | Some i -> i + 1
        ) in

        let directions = valid_dirs (x, y) (matrix_width, matrix_height) in
        let empty_set = (Set.empty (module Compatibility)) in
        let new_compatibilities = List.fold directions ~init:empty_set ~f:(
          fun acc direction ->
            let vx, vy = Direction.to_vec direction in
            let other_tile = matrix.(x + vx).(y + vy) in
            Set.add acc (cur_tile, other_tile, direction)
      ) in
      (new_weights, new_compatibilities)
    ))
  in
  (weights, compatibilities)

let () =
  Stdlib.print_endline "HI"