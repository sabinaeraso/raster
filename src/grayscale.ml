open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)

let to_gray r g b =
  let total = r + b + g in
  let value = total / 3 in
  Pixel.of_int value
;;

let transform image = Image.map image ~f:(fun (r, g, b) -> to_gray r g b)

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

let%expect_test "gray_scale" =
  let data = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  let gray = transform data in
  let acc = [] in
  let are_equal =
    Image.foldi gray ~init:acc ~f:(fun ~x ~y acc pixel ->
      let other = Image.get ~x ~y reference in
      if not (Pixel.equal pixel other) then List.append acc [ x, y ] else acc)
  in
  match are_equal with
  | [] -> ()
  | _ ->
    let head = List.take are_equal 10 in
    print_s [%message (head : (int * int) list)]
;;
