open Core

let divide image width height =
  let num_levels = Image.height image / height in
  let num_width = Image.width image / width in
  let pos_list =
    List.join
      (List.init num_levels ~f:(fun h ->
         List.init num_width ~f:(fun w ->
           ( 0 + (w * width)
           , width + (w * width)
           , 0 + (h * height)
           , height + (h * height) ))))
  in
  let regions =
    List.fold
      pos_list
      ~init:[]
      ~f:(fun reg (x_start, x_end, y_start, y_end) ->
        List.append
          reg
          [ ( Image.slice image ~x_start ~x_end ~y_start ~y_end
            , x_start
            , x_end
            , y_start
            , y_end )
          ])
  in
  regions
;;

let equal region1 region2 =
  let _, x_start1, x_end1, y_start1, y_end1 = region1 in
  let _, x_start2, x_end2, y_start2, y_end2 = region2 in
  equal x_start1 x_start2
  && equal x_end1 x_end2
  && equal y_start1 y_start2
  && equal y_end1 y_end2
;;

(* Image.foldi image2 ~init:true ~f:(fun ~x ~y b pixel2 -> if not
   (Pixel.equal pixel2 (Image.get image1 ~x ~y)) then false else b ) *)

let get_mse image1 image2 =
  Image.foldi image2 ~init:0 ~f:(fun ~x ~y mse pixel2 ->
    let pixel1 = Image.get image1 ~x ~y in
    let red, green, blue = pixel1 in
    let red2, green2, blue2 = pixel2 in
    mse
    + (Int.abs (red - red2)
       + Int.abs (green - green2)
       + Int.abs (blue - blue2)))
;;

let find_similar targets region1 =
  let image1, _, _, _, _ = region1 in
  let _lowest, region2 =
    List.fold
      targets
      ~init:(Int.max_value, region1)
      ~f:(fun (lowest, most_sim) target ->
        let this_image, _, _, _, _ = target in
        if equal region1 target
        then lowest, most_sim
        else if get_mse image1 this_image < lowest
        then get_mse image1 this_image, target
        else lowest, most_sim)
  in
  region2
;;

let transform image width height moves =
  let targets = divide image width height in
  let _ =
    List.init moves ~f:(fun _num ->
      let region1 = List.random_element_exn targets in
      let _, x_start, _, y_start, _ = region1 in
      let region2 = find_similar targets region1 in
      let image2, x_start_2, _, y_start_2, _ = region2 in
      ignore
        (List.init (width * height) ~f:(fun i ->
           let x1 = x_start + (i % width) in
           let y1 = y_start + (i / width) in
           let x2 = x_start_2 + (i % width) in
           let y2 = y_start_2 + (i / width) in
           Image.set image ~x:x1 ~y:y1 (Image.get image2 ~x:x2 ~y:y2))))
  in
  image
;;

let command =
  Command.basic
    ~summary:"Mosaic an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and width =
        flag "width" (required Command.Param.int) ~doc:"User provided width"
      and height =
        flag
          "height"
          (required Command.Param.int)
          ~doc:"User provided height"
      and moves =
        flag "moves" (required Command.Param.int) ~doc:"User provided moves"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let transformed = transform image width height moves in
        Image.save_ppm
          transformed
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;

let%expect_test "mosaic" =
  let image = Image.load_ppm ~filename:"../images/beach_portrait.ppm" in
  let reference =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_mosaic.ppm"
  in
  let mosaic = transform image 10 10 10000 in
  let acc = [] in
  let are_equal =
    Image.foldi mosaic ~init:acc ~f:(fun ~x ~y acc pixel ->
      let other = Image.get ~x ~y reference in
      if not (Pixel.equal pixel other) then List.append acc [ x, y ] else acc)
  in
  match are_equal with
  | [] -> ()
  | _ ->
    let head = List.take are_equal 10 in
    print_s [%message (head : (int * int) list)]
;;
