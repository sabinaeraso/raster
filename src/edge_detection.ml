open Core

let get_g slice grid =
  Image.fold slice ~init:(0., 0) ~f:(fun gx' pixel ->
    let gx, index = gx' in
    let value, _, _ = pixel in
    gx +. (Int.to_float value *. List.nth_exn grid index), index + 1)
;;

let get_gradient_val gray_image x y =
  let gx_grid = [ -1.; 0.; 1.; -2.; 0.; 2.; -1.; 0.; 1. ] in
  let gy_grid = [ -1.; -2.; -1.; 0.; 0.; 0.; 1.; 2.; 1. ] in
  let x_start = if x - 1 < 0 then 0 else x - 1 in
  let x_end =
    if x + 2 > Image.width gray_image - 1
    then Image.width gray_image - 1
    else x + 2
  in
  let y_start = if y - 1 < 0 then 0 else y - 1 in
  let y_end =
    if y + 2 > Image.height gray_image - 1
    then Image.height gray_image - 1
    else y + 2
  in
  let slice = Image.slice gray_image ~x_start ~x_end ~y_start ~y_end in
  let gx = get_g slice gx_grid in
  let gy = get_g slice gy_grid in
  let gx_val, _ = gx in
  let gy_val, _ = gy in
  Float.to_int (Float.sqrt ((gx_val *. gx_val) +. (gy_val *. gy_val)))
;;

let transform image threshold =
  let thresh =
    Float.to_int (Int.to_float (Image.max_val image) *. (threshold /. 100.))
  in
  let gray_image = Blur.transform (Grayscale.transform image) ~radius:2 in
  Image.mapi gray_image ~f:(fun ~x ~y _pixel ->
    let magnitude = get_gradient_val gray_image x y in
    if magnitude > thresh
    then Pixel.of_int (Image.max_val gray_image)
    else Pixel.of_int 0)
;;

let command =
  Command.basic
    ~summary:"Edge an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:"User provided threshold as a percent"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let transformed = transform image threshold in
        Image.save_ppm
          transformed
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;

let%expect_test "edge" =
  let image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_edge.ppm"
  in
  let dithered = transform image 40. in
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
