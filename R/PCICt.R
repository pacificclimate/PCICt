origin.year <- 1970
origin.year.POSIXlt <- 1900

setOldClass("PCICt")

## TODO:
## - Implement axis functions (Axis.POSIXt/axis.POSIXct) so that plots will line up nicely
## - S4 class to avoid stripping of attributes?
## - Proleptic gregorian?

PCICt.get.months <- function(cal) {
  m.365 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  m.360 <- c(30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30)
  switch(cal, "365_day"=m.365, "360_day"=m.360, "365"=m.365, "360"=m.360)
}

.PCICt <- function(x, cal) {
  if(missing(cal)) stop("Can't create a PCICt with no calendar type")
  ## FIXME: Add check for sane calendar type.
  structure(x, cal=cal, months=PCICt.get.months(cal), class=c("PCICt", "POSIXct", "POSIXt"), dpy=switch(cal, "365_day"=365, "360_day"=360, "365"=365, "360"=360), tzone="GMT", units="secs")
}

range.PCICt <- function(..., na.rm=FALSE) {
  args <- list(...)
  stopifnot(length(unique(lapply(args, function(x) { attr(x, "cal") }))) == 1)
  args.flat <- unlist(args)
  ret <- c(min(args.flat, na.rm=na.rm), max(args.flat, na.rm=na.rm))
  ret <- copy.atts.PCICt(args[[1]], ret)
  class(ret) <- c("PCICt", "POSIXct", "POSIXt")
  return(ret)
}

c.PCICt <- function(..., recursive=FALSE) {
  ##stopifnot(length(unique(lapply(..., function(x) { attr(x, "cal") }))) == 1)
  cal <- attr(..1, "cal")
  .PCICt(c(unlist(lapply(list(...), unclass))), cal)
}

## FIXME: Broken for difftime objects
`+.PCICt` <- `-.PCICt` <- Ops.PCICt <- function (e1, e2){
  cal <- attr(e1, "cal")
  if(inherits(e2, "POSIXt")) {
    stopifnot(inherits(e2, "PCICt") & attr(e1, "cal") == attr(e2, "cal"))
    class(e2) <- c("POSIXct", "POSIXt")
  }
  class(e1) <- c("POSIXct", "POSIXt")
  x <- NextMethod()
  if(inherits(x, "POSIXct")) {
    x <- copy.atts.PCICt(e1, x)
    class(x) <- c("PCICt", "POSIXct", "POSIXt")
  }
  return(x)
}

rep.PCICt <- function(x, ...) {
  y <- NextMethod()
  .PCICt(y, cal=attr(x, "cal"))
}

mean.PCICt <- function(x, ...) {
  .PCICt(mean(unclass(x), ...), attr(x, "cal"))
}

min.PCICt <- function(x, ...) {
  res <- NextMethod()
  return(copy.atts.PCICt(x, res))
}

max.PCICt <- function(x, ...) {
  res <- NextMethod()
  return(copy.atts.PCICt(x, res))
}

seq.PCICt <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
  stopifnot(attr(from, "cal") == attr(to, "cal"))
  class(from) <- class(to) <- c("POSIXct", "POSIXt")
  ret <- NextMethod()
  class(ret) <- NULL
  return(.PCICt(ret, cal=attr(from, "cal")))
}

trunc.PCICt <- function(x, units = c("secs", "mins", "hours", "days"), ...) {
      units <- match.arg(units)
      val <- unclass(x)
      round.to <- switch(units, secs = 1, mins = 60, hours = 3600, days = 86400)
      val <- floor(val / round.to) * round.to
      class(val) <- class(x)
      return(copy.atts.PCICt(x, val))
}

copy.atts.PCICt <- function(from, to) {
  return(structure(to, cal=attr(from, "cal"), months=attr(from, "months"), class=class(from), dpy=attr(from, "dpy"), tzone=attr(from, "tzone"), units=attr(from, "units")))
}

`[.PCICt` <- function(x, ...) {
  cl <- class(x)
  class(x) <- NULL
  val <- NextMethod("[")
  val <- copy.atts.PCICt(x, val)
  class(val) <- cl
  val
}

`[<-.PCICt` <- function (x, ..., value) {
  if (!as.logical(length(value)))
    return(x)
  origin <- .PCICt(0, attr(x, "cal"))
  stopifnot(class(value) == class(x) & attr(x, "cal") == attr(value, "cal"))
  cl <- oldClass(x)
  class(x) <- class(value) <- c("POSIXct", "POSIXt")
  x <- NextMethod("[<-")
  x <- copy.atts.PCICt(value, x)
  class(x) <- cl
  x
}

as.PCICt <- function(x, cal, ...) {
  UseMethod("as.PCICt")
}

as.character.PCICt <- function(x, ...) {
  if(attr(x, "dpy") == 360) {
    format.POSIXlt.360(as.POSIXlt(x), ...)
  } else {
    x <- NextMethod()
    x
  }
}

format.PCICt <- function(x, format="", tz="", usetz=FALSE, ...) {
  if (!inherits(x, "PCICt"))
    stop("wrong class")
  if(attr(x, "dpy") == 360) {
    structure(format.POSIXlt.360(as.POSIXlt(x, tz), format,
                             ...), names = names(x))
  } else {
    structure(format.POSIXlt(as.POSIXlt(x, tz), format, usetz,
                             ...), names = names(x))
  }
}

print.PCICt <- function (x, ...) {
  max.print <- getOption("max.print", 9999L)
  if (max.print < length(x)) {
    print(as.character(x[1:(max.print + 1)]), ...)
    cat(" [ reached getOption(\"max.print\") -- omitted",
        length(x) - max.print, "entries ]\n")
  }
  else print(as.character(x), ...)
  invisible(x)
}


strptime.360 <- function(x, format) {
  .Call("do_strptime_360", x, format)
}

format.POSIXlt.360 <- function(x, format="") {
  if (!inherits(x, "POSIXlt"))
    stop("wrong class")
  if (format == "") {
    times <- unlist(unclass(x)[1L:3L])
    secs <- x$sec
    secs <- secs[!is.na(secs)]
    np <- getOption("digits.secs")
    if (is.null(np))
      np <- 0L
    else np <- min(6L, np)
    if (np >= 1L)
      for (i in seq <- length(np) - 1L) if (all(abs(secs - round(secs, i)) < 1e-06)) {
        np <- i
        break
      }
    format <- if (all(times[!is.na(times)] == 0))
      "%Y-%m-%d"
    else if (np == 0L)
      "%Y-%m-%d %H:%M:%S"
    else paste0("%Y-%m-%d %H:%M:%OS", np)
  }
  y <- .Call("do_formatPOSIXlt_360", x, format)
  names(y) <- names(x$year)
  y
  
}

as.POSIXct.POSIXlt.360 <- function(x) {
  .Call("do_asPOSIXct_360", x, format)
}

as.POSIXlt.POSIXct.360 <- function(x) {
  .Call("do_asPOSIXlt_360", x, format)
}

as.PCICt.default <- function(x, cal, ...) {
  tz <- "GMT"
  if (inherits(x, "PCICt"))
    return(x)
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    if(cal == "360" || cal == "360_day") {
      x <- unclass(x)
      xx <- x[!is.na(x)]
      if (!length(xx))
        res <- strptime(x, "%Y/%m/%d")
      else if (all(!is.na(strptime.360(xx, f <- "%Y-%m-%d %H:%M:%OS"))) ||
               all(!is.na(strptime.360(xx, f <- "%Y/%m/%d %H:%M:%OS"))) ||
               all(!is.na(strptime.360(xx, f <- "%Y-%m-%d %H:%M"))) ||
               all(!is.na(strptime.360(xx, f <- "%Y/%m/%d %H:%M"))) ||
               all(!is.na(strptime.360(xx, f <- "%Y-%m-%d"))) ||
               all(!is.na(strptime.360(xx, f <- "%Y/%m/%d"))))
        res <- strptime.360(x, f)
      attr(res, "tzone") <- tz
      return(as.PCICt(res, cal, ...))
    } else {
      return(as.PCICt(as.POSIXlt(x, tz, ...), cal, ...))
    }
  }
  if (is.logical(x) && all(is.na(x)))
    return(.PCICt(as.numeric(x), cal))
  stop(gettextf("do not know how to convert '%s' to class \"PCICt\"", deparse(substitute(x))))
}

as.PCICt.numeric <- function(x, cal, origin, ...) {
  if (missing(origin))
    stop("'origin' must be supplied")

  if(inherits(origin, "PCICt") && attr(origin, "cal") == cal)
    return(origin + x)
  else
    return(as.PCICt(origin, cal) + x)
}

as.PCICt.POSIXlt <- function(x, cal, ...) {
  proleptic.correction <- 0
  seconds.per.day <- 86400
  tz <- "GMT"
  year.length <- switch(cal, "360_day"=360, "365_day"=365, "365"=365, "360"=360, "noleap"=365, "gregorian"=NULL, "proleptic_gregorian"=NULL, "standard"=NULL)
  ## FIXME: Add check for sane calendar type.

  ## Correct calendar output if proleptic gregorian
  if(cal == "proleptic_gregorian") {
    year.adjusted <- x$year + origin.year.POSIXlt + as.numeric(x$mon >= 3) - 1
    diff.days <- floor(year.adjusted / 100) - floor(year.adjusted / 400) - 2
    diff.days[x$year >= 1582 & x$mon >= 10 & x$day >= 4] <- 0
    proleptic.correction <- diff.days * seconds.per.day
  }
  if(is.null(year.length)) {
    d <- as.POSIXct(x, tz="GMT")
    class(d) <- NULL
    return(.PCICt(d + proleptic.correction, "gregorian"))
  } else {
    months <- PCICt.get.months(cal)
    months.off <- cumsum(c(0, months[1:(length(months) - 1)]))
    seconds.per.hour <- 3600
    return(.PCICt((x$year + origin.year.POSIXlt - origin.year) * year.length * seconds.per.day +
                  months.off[x$mon + 1] * seconds.per.day + (x$mday - 1) * seconds.per.day + x$hour * seconds.per.hour + x$min * 60 + x$sec, cal=cal))
  }
}

as.PCICt.POSIXct <- function(x, cal, ...) {
  if(cal == "360" || cal == "360_day") {
    as.PCICt.POSIXlt(as.POSIXlt.POSIXct.360(x), cal, ...)
  } else {
    as.PCICt.POSIXlt(as.POSIXlt(x), cal, ...)
  }
}

as.POSIXlt.PCICt <- function(x, tz="", ...) {
  seconds.per.day <- 86400
  seconds.per.hour <- 3600

  tzone <- attr(x, "tzone")
  if (length(tz) == 0 && !is.null(tzone))
    tz <- tzone[1L]

  if(is.null(attr(x, "months"))) {
    class(x) <- c("POSIXct", "POSIXt")
    return(as.POSIXlt(x))
  } else {
    months <- attr(x, "months")
    months.off <- cumsum(c(0, months[1:(length(months) - 1)]))
    months.idx <- unlist(lapply(1:12, function(x) { rep(x, months[x]) } ))

    days.per.year <- attr(x, "dpy")
    remainder <- as.numeric(x) %% (days.per.year * seconds.per.day)
    remainder[remainder < 0] <- days.per.year * seconds.per.day - remainder[remainder < 0]

    year <- floor(as.numeric(x) / (days.per.year * seconds.per.day)) + origin.year
    yday <- floor(remainder / seconds.per.day) + 1
    month <- months.idx[yday]
    day <- yday - months.off[month]

    ## Need to compute wday
    wday <- (as.numeric(x) / 86400) %% 7
    hms.remainder <- remainder %% seconds.per.day
    hour <- floor(hms.remainder / seconds.per.hour)
    minute <- floor((hms.remainder %% seconds.per.hour) / 60)
    second <- hms.remainder %% 60
    return(.POSIXlt(list(sec=second, min=minute, hour=hour, mday=day, mon=month - 1, year=year - origin.year.POSIXlt, wday=wday, yday=yday - 1, isdst=0), tz))
  }
}

as.POSIXct.PCICt <- function(x, tz="", ...) {
  if(attr(x, "cal") == "360" || attr(x, "cal") == "360_day") {
    warning("360-day PCICt objects can't be properly represented by a POSIXct object")
  }
  return(as.POSIXct(as.POSIXlt(x, tz)))
}

cut.PCICt <- function (x, breaks, labels = NULL, start.on.monday = TRUE, right = FALSE, ...) {
  stopifnot(attr(x, "cal") != "360" && attr(x, "cal") != "360_day");

  cut.POSIXt(as.POSIXct(x), breaks, labels, start.on.monday, right, ...)
}

diff.PCICt <- function (x, lag = 1L, differences = 1L, ...) {
  class(x) <- c("POSIXct", "POSIXt")
  diff(x, lag, differences, ...)
}

is.numeric.PCICt <- function(x) FALSE

julian.PCICt <- function (x, origin=NULL, ...) {
  if(is.null(origin))
    origin <- "1970-01-01"
  else
    stopifnot(attr(x, "cal") == attr(origin, "cal"))

  origin <- as.PCICt(origin, cal=attr(x, "cal"))
  class(x) <- class(origin) <- c("POSIXct", "POSIXt")
  if (length(origin) != 1L)
    stop("'origin' must be of length one")

  res <- difftime(x, origin, units = "days")
  structure(res, origin = origin)
}
