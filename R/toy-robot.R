library(methods)

make.move <- function() {
  return(list(
    "NORTH" = list("x" =  0, "y" =  1),
    "SOUTH" = list("x" =  0, "y" = -1),
    "EAST"  = list("x" =  1, "y" =  0),
    "WEST"  = list("x" = -1, "y" =  0)
  ))
}

make.turn <- function() {
  return(list(
    "NORTH" = list("LEFT" = "WEST",  "RIGHT" = "EAST"),
    "SOUTH" = list("LEFT" = "EAST",  "RIGHT" = "WEST"),
    "EAST"  = list("LEFT" = "NORTH", "RIGHT" = "SOUTH"),
    "WEST"  = list("LEFT" = "SOUTH", "RIGHT" = "NORTH")
  ))
}

setClass(
  "Robot",

  slots=list(
    x="numeric",
    y="numeric",
    facing="character"),

  prototype=list(
    x=0,
    y=0,
    facing="NORTH")
)

setGeneric(name="report", def=function(object) {
    standardGeneric("report")
})
# setMethod("report", "Robot", function(object) {
setMethod(f="report", signature="Robot", definition=function(object) {
    cat(object@x)
    cat(",", object@y)
    cat(",", object@facing, "\n")
})

setGeneric(name="left", def=function(object) {
    standardGeneric("left")
})
setMethod(f="left", signature="Robot", definition=function(object) {
    object@facing <- make.turn()[[object@facing]][["LEFT"]]
    return(object)
})

setGeneric(name="right", def=function(object) {
    standardGeneric("right")
})
setMethod(f="right", signature="Robot", definition=function(object) {
    object@facing <- make.turn()[[object@facing]][["RIGHT"]]
    return(object)
})

setGeneric(name="move", def=function(object) {
    standardGeneric("move")
})
setMethod(f="move", signature="Robot", definition=function(object) {
    object@x <- object@x + make.move()[[object@facing]][["x"]]
    object@y <- object@y + make.move()[[object@facing]][["y"]]

    if (object@x < 0 || object@x > 4) {
      object@x <- object@x - make.move()[[object@facing]][["x"]]
    }

    if (object@y < 0 || object@y > 4) {
      object@y <- object@y - make.move()[[object@facing]][["y"]]
    }

    return(object)
})

setGeneric(name="exec", def=function(object, command, args="") {
    standardGeneric("exec")
})
setMethod(f="exec", signature="Robot",
  definition=function(object, command, args="") {
    new_object <- switch(command,
      "MOVE" = move(object),
      "LEFT" = left(object),
      "RIGHT" = right(object),
      "REPORT" = report(object)
    )

    if (is.null(new_object)) {
      return(object)
    }
    else {
      return(new_object)
    }
})


setClass("ToyRobot", slots=list(nil="character"), prototype=list(nil=""))

setGeneric(name="run", def=function(object, args) {
    standardGeneric("run")
})
setMethod(f="run", signature="ToyRobot", definition=function(object, args) {
    # args = commandArgs(trailingOnly=TRUE)
    # robot <- new("Robot", x=0, y=0, facing="NORTH")
    robot <- new("Robot")

    if (length(args) == 0) {
      # stop("At least one argument must be supplied (input file).\n", call.=FALSE)
      report(robot)
    }
    else {
      filename = args[1]
      print(filename)
    }
})
