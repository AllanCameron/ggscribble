# Calculate the number of npc units in a unit object, even if they are
# in nested calculated units

get_npc <- function(x) {

  if(inherits(x, "simpleUnit")) {

    if(attributes(x)$unit == 0) {
      return(as.numeric(x))
    } else {
      return(rep(0, length(x)))
    }
  }

  unlist(lapply(unclass(x), function(u) {
    if(is.null(u[[2]])) {
      if(u[[3]] == 0) {
        return(u[[1]])
      } else {
        return(0)
      }
    }
    if(u[[3]] == 201) {
      return(sum(unlist(get_npc(u[[2]]))))
    }
    if(u[[3]] == 202) {
      return(min(unlist(get_npc(u[[2]]))))
    }
    if(u[[3]] == 203) {
      return(max(unlist(get_npc(u[[2]]))))
    }
    stop("Invalid unit")
  }))
}
