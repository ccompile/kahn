open Graphics;;

let print_spectr t spect=
  for i=0 to Array.length spect do
    set_color (rgb spect.(i) spect.(i) spect.(i)) 
    (*TODO : Adjust linearization*)
    plot t i;     
  done

let defiler x1 y1 x2 y2 =
  let a = get_image x1 y1 x2 y2 in
  draw_image a (x1-1) (y1-1)

let swip_and_print spect x1 y1 x2 y2 in
  defiler x1 y1 x2 y2;
  print_spectr (x2+1) spect  
