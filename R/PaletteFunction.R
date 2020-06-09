QualVibes <- readRDS('Palettes/QualVibes.rds')
SeqVibes <- readRDS('Palettes/SeqVibes.rds')
DivVibes <- readRDS('Palettes/DivVibes.rds')

#' palette generator 
#'
#' These are some colour palettes from a collection 
#'
#' @param n Number of colors desired. 
#'   Qual palettes have n=5, Seq palettes have n=8 
#'   schemes are derived from \href{https://www.colourpod.com/}, based on image found at \href{https://firefaerie81.tumblr.com/image/181811531294}.
#'   If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#'   \code{Balance}, \code{BlueberryIceCream},  \code{CautionIsNotColdness},
#'   \code{CoralWaves}, \code{CosmicHeartbeat},  \code{DontEatAzaleas}, \code{DragonEgg},
#'   \code{FizzyOcean},  \code{FlowerStalk} , \code{HeartToRemember} ,
#'   \code{IJustWorkHere}, \code{LavenderBiscuit}, \code{MelodyOfFreefall}, \code{Omens},
#'   \code{PsychicSurge}, \code{Reverb}, \code{SilentGold}, \code{SnakesEyeView},
#'   \code{SummerSunset}, \code{ThatsAPuzzle}, \code{WhenImInTokyo}      
#'   @importFrom graphics rgb rect par image text
#' @param type Type of palette desired. choices are: 
#'   \code{seq}, \code{qual},  \code{div} 
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' park_palette("SmokyMountains")
#' park_palette("Yellowstone", 3)

vibe_palette <- function(name, type, n) {
  
  if (type=="seq") {
     pal <- SeqVibes[[name]]
  }
  
  if (type=="div") {
    pal <- DivVibes[[name]]
  }
  
  if (type=="qual") { 
    pal <- QualVibes[[name]] 
    
  }  
     
  if (is.null(pal))
    stop("Palette not found.")
  
  if (is.null(pal))
    stop("Type not specified. Options are seq, qual, or div")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- pal[1:n]
  
  structure(out, class = "palette", name = name)

  print.palette(out)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), family = "Helvetica", col = "black")
}
