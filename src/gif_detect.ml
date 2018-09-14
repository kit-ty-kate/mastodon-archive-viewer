type filename = string

let is_gif url =
  let input = FFmpeg.Av.open_input url in
  let audio = FFmpeg.Av.get_audio_streams input in
  FFmpeg.Av.close input;
  List.is_empty audio
