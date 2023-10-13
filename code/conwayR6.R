packages <- c("R6", "ggplot2", "reshape2", "animation", "magick")
load <- lapply(packages, library, character.only = TRUE)

# create R6 class to store the game
GoF <- R6Class(
  "Conway's Game of Life",
  public = list(
    width = NULL,
    height = NULL,
    grid = NULL,
    initialize = function(width = NA, height = NA) {
      self$width <- width
      self$height <- height
    },
    makeGrid = function() {
      self$grid <- matrix(0, nrow = self$height, ncol = self$width)
    },
    bringLife = function(x, y) {
      self$grid[x, y] <- 1
    },
    isAlive = function(x, y) {
      self$grid[x, y] == 1
    },
    aliveNeigh = function(x, y) {
      row.cord <- seq(max(x - 1, 1), min(x + 1, self$height - 1), by = 1)
      col.cord <-
        seq(max(y - 1, 1), min(y + 1, self$width - 1), by = 1)
      cord.comb <- as.matrix(expand.grid(row.cord, col.cord))
      colnames(cord.comb) <- c("x", "y")
      
      sum.alive <- 0
      for (i in 1:nrow(cord.comb)) {
        if (cord.comb[i, 1] != x | cord.comb[i, 2] != y) {
          sum.alive <- sum.alive + self$grid[cord.comb[i, 1], cord.comb[i, 2]]
        }
      }
      return(sum.alive)
    },
    advance = function() {
      new.grid <- matrix(0, nrow = self$width, ncol = self$width)
      for (x in 1:self$height) {
        for (y in 1:self$width) {
          # an alive cell with < 2 alive neighbors dies...
          if (self$isAlive(x, y) == TRUE &
              self$aliveNeigh(x, y) < 2) {
            new.grid[x, y] <- 0
          }
          # an alive cell with 2 or 3 alive neighbors survives...
          if (self$isAlive(x, y) == TRUE &
              (self$aliveNeigh(x, y) == 2 |
               self$aliveNeigh(x, y) == 3)) {
            new.grid[x, y] <- 1
          }
          # an alive cell with > 3 alive neighbors dies...
          if (self$isAlive(x, y) == TRUE &
              self$aliveNeigh(x, y) > 3) {
            new.grid[x, y] <- 0
          }
          # a dead cell with 3 alive neighbors comes to life...
          if (self$isAlive(x, y) == FALSE &
              self$aliveNeigh(x, y) == 3) {
            new.grid[x, y] <- 1
          }
        }
      }
      self$grid <- new.grid
    }
  )
)

# initialize a 20 x 20 grid
game <- GoF$new(20, 20)
game$makeGrid()
game$bringLife(10, 10)
game$bringLife(11, 10)
game$bringLife(11, 9)
game$bringLife(11, 11)

i <- 1
img.list <- list()

# advance the game for 20 simulation steps
while (i <= 20) {
  lifemat <- game$grid
  melt.lifemat <- melt(lifemat)
  colnames(melt.lifemat) <- c("x", "y", "value")
  melt.lifemat[["value"]] <- as.factor(melt.lifemat[["value"]])
  ggplot(melt.lifemat, aes(x = y, y = x, fill = value)) + geom_tile() +
    labs(
      title = paste0("Simulation step: ", as.character(i)),
      x = "",
      y = ""
    ) +
    scale_fill_manual(values = c("lightgray", "black")) + theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) + scale_y_continuous(trans = "reverse")
  ggsave(
    paste0("Step-", as.character(i), ".png"),
    device = "png",
    unit = "in",
    height = 3,
    width = 3,
    dpi = 400
  )
  img.list <-
    append(img.list, image_read(paste0("Step-", as.character(i), ".png")))
  file.remove(paste0("Step-", as.character(i), ".png"))
  game$advance()
  i <- i + 1
}

# create .gif
img.joined <- image_join(img.list)
gif <- image_animate(img.joined, fps = 2)
image_write(image = gif, path = "conway_gif.gif")
