library(methods)

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

# == References:
# - [2\. S4 Classes — R Tutorial]
#   (http://www.cyclismo.org/tutorial/R/s4Classes.html)
