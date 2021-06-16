type filename = string

let is_gif url =
  let input = Av.open_input url in
  let audio = Av.get_audio_streams input in
  Av.close input;
  List.is_empty audio
