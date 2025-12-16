ASCII_CHARS <- " .:-=+*#%@"
WIDTH <- 80
HEIGHT <- 40

download_video <- function(url) {
  cat("Downloading video...\n")
  system(paste0("yt-dlp -f worst -o video.mp4 '", url, "'"))
}

rgb_to_ascii <- function(r, g, b) {
  brightness <- (r + g + b) / 3
  index <- floor(brightness * (nchar(ASCII_CHARS) - 1) / 255) + 1
  substr(ASCII_CHARS, index, index)
}

extract_and_display_frame <- function(time) {
  cmd <- sprintf("ffmpeg -ss %.2f -i video.mp4 -vframes 1 -vf scale=%d:%d -f rawvideo -pix_fmt rgb24 - 2>/dev/null",
                 time, WIDTH, HEIGHT)
  
  pixels <- system(cmd, intern = TRUE, ignore.stdout = FALSE)
  
  if (length(pixels) > 0) {
    cat("\033[2J\033[H")
    
    for (y in 0:(HEIGHT-1)) {
      for (x in 0:(WIDTH-1)) {
        idx <- (y * WIDTH + x) * 3 + 1
        if (idx + 2 <= length(pixels)) {
          r <- as.integer(charToRaw(substr(pixels, idx, idx)))
          g <- as.integer(charToRaw(substr(pixels, idx+1, idx+1)))
          b <- as.integer(charToRaw(substr(pixels, idx+2, idx+2)))
          cat(rgb_to_ascii(r, g, b))
        }
      }
      cat("\n")
    }
  }
}

url <- "https://youtu.be/FtutLA63Cp8"
download_video(url)

fps <- 10
duration <- 30
time <- 0

while (time < duration) {
  extract_and_display_frame(time)
  Sys.sleep(1 / fps)
  time <- time + 1 / fps
}
