let () = prerr_endline "Warning: Couldn't distinguish gifs from videos. Please recompile with the ffmpeg library \
                        installed if you want this feature"

let is_gif _ = false
