open Core

(* This should look familiar by now! *)

let process pixel max n =
  let diff = max / (n - 1) in
  let poss = List.init n ~f:(fun i -> i * diff) in
  let red, green, blue = pixel in
  let diff = Int.max_value, Int.max_value in
  (*diff,value*)
  let r =
    List.fold poss ~init:diff ~f:(fun (diff, new_value) value ->
      if Int.abs (value - red) < diff
      then Int.abs (value - red), new_value
      else diff, new_value)
  in
  let _, new_red = r in
  let b =
    List.fold poss ~init:diff ~f:(fun (diff, new_value) value ->
      if Int.abs (value - blue) < diff
      then Int.abs (value - blue), new_value
      else diff, new_value)
  in
  let _, new_blue = b in
  let g =
    List.fold poss ~init:diff ~f:(fun (diff, new_value) value ->
      if Int.abs (value - green) < diff
      then Int.abs (value - green), new_value
      else diff, new_value)
  in
  let _, new_green = g in
  new_red, new_green, new_blue
;;

let change_adjacent gray_image error x y =
  let right_const = Int.to_float error *. (7. /. 16.) in
  let left_const = Int.to_float error *. (3. /. 16.) in
  let below_const = Int.to_float error *. (5. /. 16.) in
  let bottom_right_const = Int.to_float error *. (1. /. 16.) in
  if x + 1 < Image.width gray_image
  then (
    let new_red =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x:(x + 1) ~y)
            + Float.to_int right_const)
           0)
        (Image.max_val gray_image)
    in
    let new_green =
      Int.min
        (Int.max
           (Pixel.green (Image.get gray_image ~x:(x + 1) ~y)
            + Float.to_int right_const)
           0)
        (Image.max_val gray_image)
    in
    let new_blue =
      Int.min
        (Int.max
           (Pixel.blue (Image.get gray_image ~x:(x + 1) ~y)
            + Float.to_int right_const)
           0)
        (Image.max_val gray_image)
    in
    let right = new_red, new_green, new_blue in
    Image.set gray_image ~x:(x + 1) ~y right)
  else ();
  (* right *)
  if x - 1 >= 0 && y + 1 <= Image.height gray_image - 1
  then (
    let new_red =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x:(x - 1) ~y:(y + 1))
            + Float.to_int left_const)
           0)
        (Image.max_val gray_image)
    in
    let new_green =
      Int.min
        (Int.max
           (Pixel.green (Image.get gray_image ~x:(x - 1) ~y:(y + 1))
            + Float.to_int left_const)
           0)
        (Image.max_val gray_image)
    in
    let new_blue =
      Int.min
        (Int.max
           (Pixel.blue (Image.get gray_image ~x:(x - 1) ~y:(y + 1))
            + Float.to_int left_const)
           0)
        (Image.max_val gray_image)
    in
    let left = new_red, new_green, new_blue in
    Image.set gray_image ~x:(x - 1) ~y:(y + 1) left)
  else ();
  (* bottom left *)
  if y + 1 <= Image.height gray_image - 1
  then (
    let new_val =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x ~y:(y + 1))
            + Float.to_int below_const)
           0)
        (Image.max_val gray_image)
    in
    let below = Pixel.of_int new_val in
    Image.set gray_image ~x ~y:(y + 1) below)
  else ();
  (* directily below *)
  if y + 1 <= Image.height gray_image - 1 && x + 1 < Image.width gray_image
  then (
    let new_val =
      Int.min
        (Int.max
           (Pixel.red (Image.get gray_image ~x:(x + 1) ~y:(y + 1))
            + Float.to_int bottom_right_const)
           0)
        (Image.max_val gray_image)
    in
    let bottom_right = Pixel.of_int new_val in
    Image.set gray_image ~x:(x + 1) ~y:(y + 1) bottom_right)
  else ();
  gray_image
;;

(*type mutable error_map = [(int*int*int)] *)

let transform image n =
  let gray_image = Grayscale.transform image in
  Image.foldi gray_image ~init:gray_image ~f:(fun ~x ~y gray_image pixel ->
    let new_pixel = process pixel (Image.max_val gray_image) n in
    Image.set gray_image ~x ~y new_pixel;
    let error = Pixel.red pixel - Pixel.red new_pixel in
    change_adjacent gray_image error x y)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

let%expect_test "dither" =
  let image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  let dithered = transform image in
  let acc = [] in
  let are_equal =
    Image.foldi dithered ~init:acc ~f:(fun ~x ~y acc pixel ->
      let other = Image.get ~x ~y reference in
      if not (Pixel.equal pixel other) then List.append acc [ x, y ] else acc)
  in
  match are_equal with
  | [] -> ()
  | _ ->
    let head = List.take are_equal 10 in
    print_s [%message (head : (int * int) list)]
;;
