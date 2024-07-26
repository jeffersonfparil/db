#' An S4 class to represent an error type
methods::setClass(Class="dbError", representation=representation(code="numeric", message="character"), prototype(code=0, message="Empty error message. Please define."))

# Error chaining method intialisation for "dbError"\
methods::setGeneric("chain", function(x, y){
    standardGeneric("chain")
})

# Error chaining method definition for "dbError"
methods::setMethod(f="chain",
  signature=c(x="dbError", y="dbError"),
  function(x, y) {
    codes = c(x@code, y@code)
    messages = c(x@message, y@message)
    return(methods::new("dbError", code=codes, message=messages))
  }
)
