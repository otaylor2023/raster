open! Core

let calculate_mse (region1 : Image.t) (region2 : Image.t) =
  let width = Image.width region1 in
  let height = Image.height region1 in
  let summed =
    Image.foldi
      region1
      ~init:0
      ~f:(fun ~x ~y current_mse (red1, green1, blue1) ->
        let red2, green2, blue2 = Image.get region2 ~x ~y in
        let pixel_mse =
          Int.pow (red1 - red2) 2
          + Int.pow (green1 - green2) 2
          + Int.pow (blue1 - blue2) 2
        in
        current_mse + pixel_mse)
  in
  let final_mse = Float.of_int summed /. Float.of_int (width + height) in
  final_mse
;;

(* if Float.equal final_mse 0. then Float.max_value else final_mse *)

let highlight_waldo image ~waldo_section:(waldo_slice, (start_x, start_y)) =
  (* let end_x1 = start_x1 + width in let end_y1 = start_y1 + height in let
     end_x2 = start_x2 + width in let end_y2 = start_y2 + height in *)
  let max_val = Image.max_val waldo_slice in
  let red_pixel = max_val, 0, 0 in
  let height = Image.height waldo_slice in
  let width = Image.width waldo_slice in
  let thickness = 2 in
  (* let _ = Image.mapi waldo_slice ~f:(fun ~x ~y pixel -> Image.set image
     ~x:(start_x + x) ~y:(start_y + y) (0, 0, 0); pixel) in *)
  let _top_bottom_borders =
    List.init width ~f:(fun x ->
      let image_x = start_x + x in
      let _thickness =
        List.init thickness ~f:(fun i ->
          Image.set image ~x:image_x ~y:(start_y + i) red_pixel;
          Image.set image ~x:image_x ~y:(start_y + height - i) red_pixel;
          i)
      in
      x)
  in
  let _left_right_borders =
    List.init height ~f:(fun y ->
      let image_y = start_y + y in
      let _thickness =
        List.init thickness ~f:(fun i ->
          Image.set image ~x:(start_x + i) ~y:image_y red_pixel;
          Image.set image ~x:(start_x + width - i) ~y:image_y red_pixel;
          i)
      in
      y)
  in
  ()
;;

(* print_s [%message (final_mse : int)]; *)

let divide_image image ~width ~height =
  let num_xs = Image.width image / width in
  let num_ys = Image.height image / height in
  let x_values =
    List.init num_xs ~f:(fun x_start -> (x_start * width) + 20)
  in
  let y_values =
    List.init num_ys ~f:(fun y_start -> (y_start * height) + 15)
  in
  List.fold x_values ~init:[] ~f:(fun slice_list x_start ->
    let x_end = x_start + width in
    match x_end > Image.width image - 1 with
    | true -> slice_list
    | false ->
      List.fold y_values ~init:slice_list ~f:(fun slice_list y_start ->
        let y_end = y_start + height in
        match y_end > Image.height image - 1 with
        | true -> slice_list
        | false ->
          let section =
            ( Image.slice image ~x_start ~y_start ~x_end ~y_end
            , (x_start, y_start) )
          in
          (* highlight_waldo image ~waldo_section:section; *)
          section :: slice_list))
;;

(* Image.slice image ~x_start ~y_start ~x_end ~y_end *)

let transform waldo_map waldo_head =
  let width = Image.width waldo_head in
  let height = Image.height waldo_head in
  let slice_list = divide_image waldo_map ~width ~height in
  let first_slice, first_coords = List.hd_exn slice_list in
  (* let (mse_sum, mse_list) = List.fold_map ~init:0. slice_list ~f:(fun sum
     (curr_slice, curr_coords) -> let curr_mse = calculate_mse waldo_head
     curr_slice in let new_sum = sum +. curr_mse in new_sum, (curr_mse,
     (curr_slice, curr_coords))) in let average_mse = mse_sum /.
     (Float.of_int (List.length mse_list)) in let top_ten = List.filter
     mse_list ~f:(fun (curr_mse, (curr_slice, curr_coords) ->

     )) *)
  let first_mse = calculate_mse waldo_head first_slice in
  let _lowest_mse, chosen_section =
    List.fold
      ~init:(first_mse, (first_slice, first_coords))
      slice_list
      ~f:
        (fun
          (best_mse, (best_section, best_coords))
          (curr_slice, curr_coords)
        ->
        let curr_mse = calculate_mse waldo_head curr_slice in
        match Float.( < ) curr_mse best_mse with
        | true -> curr_mse, (curr_slice, curr_coords)
        | false -> best_mse, (best_section, best_coords))
  in
  highlight_waldo waldo_map ~waldo_section:chosen_section;
  waldo_map
;;

let command =
  Command.basic
    ~summary:"Find waldo"
    [%map_open.Command
      let map_filename =
        flag
          "map"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and waldo_filename =
        flag
          "head"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let waldo_map = Image.load_ppm ~filename:map_filename in
        let waldo_head = Image.load_ppm ~filename:waldo_filename in
        let image' = transform waldo_map waldo_head in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn map_filename ~suffix:".ppm"
             ^ "_waldo.ppm")]
;;
