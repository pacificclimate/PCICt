origin.year <- 1970
origin.year.POSIXlt <- 1900

setOldClass("PCICt")

## TODO:
## - Implement axis functions (Axis.POSIXt/axis.POSIXct) so that plots will line up nicely
## - S4 class to avoid stripping of attributes?

PCICt.get.months <- function(cal) {
  m.365 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  m.360 <- c(30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30)
  switch(cal, "365"=m.365, "360"=m.360)
}

dpy.for.cal <- function(cal) {
  switch(cal, "365"=365, "360"=360)
}

.PCICt <- function(x, cal) {
  cal.list <- c("365_day", "365", "noleap", "360_day", "360", "gregorian", "standard", "proleptic_gregorian")
  cal.map <- c("365", "365", "365", "360", "360", "gregorian", "gregorian", "proleptic_gregorian")
  if(missing(cal)) stop("Can't create a PCICt with no calendar type")
  if(!cal %in% cal.list) stop(paste("Calendar type not one of", paste(cal.list, sep=", ")))
  cal.cleaned <- cal.map[cal.list %in% cal]

  structure(x, cal=cal.cleaned, months=PCICt.get.months(cal.cleaned), class=c("PCICt", "POSIXct", "POSIXt"), dpy=dpy.for.cal(cal.cleaned), tzone="GMT", units="secs")
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

`+.PCICt` <- `-.PCICt` <- Ops.PCICt <- function (e1, e2){
  cal <- ifelse(inherits(e1, "PCICt"), cal <- attr(e1, "cal"), cal <- attr(e2, "cal"))
  if(inherits(e2, "PCICt")) {
    stopifnot(cal == attr(e2, "cal"))
    class(e2) <- c("POSIXct", "POSIXt")
  }
  if(inherits(e1, "PCICt")) {
    stopifnot(cal == attr(e1, "cal"))
    class(e1) <- c("POSIXct", "POSIXt")
  }
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
  if (missing(from))
    stop("'from' must be specified")
  if (!inherits(from, "PCICt"))
    stop("'from' must be a PCICt object")
  if (length(from) != 1L)
    stop("'from' must be of length 1")
  if (!missing(to)) {
    if (!inherits(to, "PCICt"))
      stop("'to' must be a PCICt object")
    if (length(to) != 1)
      stop("'to' must be of length 1")
    if(to < from)
      stop("'to' must be less than 'from'")
  }
  if (!missing(along.with)) {
    length.out <- length(along.with)
  }
  else if (!is.null(length.out)) {
    if (length(length.out) != 1L)
      stop("'length.out' must be of length 1")
    length.out <- ceiling(length.out)
  }
  status <- c(!missing(to), !missing(by), !is.null(length.out))
  if (sum(status) != 2L)
    stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
  if (missing(by)) {
    from <- unclass(from)
    to <- unclass(to)
    res <- seq.int(from, to, length.out = length.out)
    return(.PCICt(res, attr(from, "cal")))
  }
  if (length(by) != 1L)
    stop("'by' must be of length 1")
  valid <- 0L
  if (inherits(by, "difftime")) {
    by <- switch(attr(by, "units"), secs = 1, mins = 60,
                 hours = 3600, days = 86400, weeks = 7 * 86400) *
                   unclass(by)
  } else if (is.character(by)) {
    by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L)
      stop("invalid 'by' string")
    valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours",
                                        "days", "weeks", "months", "years", "DSTdays"))
    if (is.na(valid))
      stop("invalid string for 'by'")
    if (valid <= 5L) {
      by <- c(1, 60, 3600, 86400, 7 * 86400)[valid]
      if (length(by2) == 2L)
        by <- by * as.integer(by2[1L])
    }
    else by <- if (length(by2) == 2L)
      as.integer(by2[1L])
    else 1
  }
  else if (!is.numeric(by))
    stop("invalid mode for 'by'")
  if (is.na(by))
    stop("'by' is NA")
  if (valid <= 5L) {
    from <- unclass(from)
    if (!is.null(length.out))
      res <- seq.int(from, by = by, length.out = length.out)
    else {
      to0 <- unclass(to)
      res <- seq.int(0, to0 - from, by) + from
    }
    return(.PCICt(res, attr(from, "cal")))
  } else {
    r1 <- as.POSIXlt(from)
    if (valid == 7L) {
      if (missing(to)) {
        yr <- seq.int(r1$year, by = by, length.out = length.out)
      } else {
        to0 <- as.POSIXlt(to)
        yr <- seq.int(r1$year, to0$year, by)
      }
      r1$year <- yr
    } else if (valid == 6L) {
      if (missing(to)) {
        mon <- seq.int(r1$mon, by = by, length.out = length.out)
      } else {
        to0 <- as.POSIXlt(to)
        mon <- seq.int(r1$mon, 12 * (to0$year - r1$year) +
                       to0$mon, by)
      }
      r1$mon <- mon
    } else if (valid == 8L) {
      if (!missing(to)) {
        length.out <- 2L + floor((unclass(to) -
                                  unclass(from))/86400)
      }
      r1$mday <- seq.int(r1$mday, by = by, length.out = length.out)
    }
    r1$isdst <- -1L
    res <- as.PCICt(r1, attr(from, "cal"))
    if (!missing(to)) {
      res <- if (by > 0)
        res[res <= to]
      else res[res >= to]
    }
    res
  }
}

trunc.PCICt <- function(x, units = c("secs", "mins", "hours", "days"), ...) {
      units <- match.arg(units)
      val <- unclass(x)
      round.to <- switch(units, secs = 1, mins = 60, hours = 3600, days = 86400)
      val <- floor(val / round.to) * round.to
      class(val) <- class(x)
      return(copy.atts.PCICt(x, val))
}

round.PCICt <- function (x, digits = c("secs", "mins", "hours", "days")) {
  if (is.numeric(digits) && digits == 0)
    digits <- "secs"
  digits <- match.arg(digits)
  x <- x + switch(digits, secs = 0.5, mins = 30, hours = 1800,
                  days = 43200)
  trunc(x, units = digits)
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
  if(missing(cal)) stop("Can't create a PCICt with no calendar type")
  UseMethod("as.PCICt")
}

as.character.PCICt <- function(x, ...) {
  if(!is.null(attr(x, "dpy")) && attr(x, "dpy") == 360) {
    format.POSIXlt.360(as.POSIXlt(x), ...)
  } else {
    x <- NextMethod()
    x
  }
}

unique.PCICt <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (!inherits(x, "PCICt"))
    stop("wrong class")
  z <- unique(unclass(x), incomparables, fromLast, ...)
  return(copy.atts.PCICt(x, z))
}

summary.PCICt <- function (object, digits = 15, ...) {
  x <- summary.default(unclass(object), digits = digits, ...)
  if (m <- match("NA's", names(x), 0)) {
    NAs <- as.integer(x[m])
    x <- x[-m]
    attr(x, "NAs") <- NAs
  }
  x <- copy.atts.PCICt(object, x)
  class(x) <- c("summaryDefault", "table", oldClass(object))
  x
}

format.PCICt <- function(x, format="", tz="", usetz=FALSE, ...) {
  if (!inherits(x, "PCICt"))
    stop("wrong class")
  if(!is.null(attr(x, "dpy")) && attr(x, "dpy") == 360) {
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
    print(as.character(x[1:max.print]), ...)
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
    return(.PCICt((x$year + origin.year.POSIXlt - origin.year + floor(x$mon / 12)) * year.length * seconds.per.day +
                  months.off[(x$mon %% 12) + 1] * seconds.per.day + (x$mday - 1) * seconds.per.day + x$hour * seconds.per.hour + x$min * 60 + x$sec, cal=cal))
  }
}

as.PCICt.POSIXct <- function(x, cal, ...) {
  if(cal == "360" || cal == "360_day") {
    as.PCICt.POSIXlt(as.POSIXlt.POSIXct.360(x), cal, ...)
  } else {
    as.PCICt.POSIXlt(as.POSIXlt(x), cal, ...)
  }
}

## FIXME: Better NA handling
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
  stopifnot(is.null(attr(x, "dpy")) || attr(x, "dpy") != 360)

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

get.sec.incr <- function(x, secs, incr=1, mul=1.1) {
  if(length(secs) == 0)
    NA

  if(mul * (incr * secs[1]) > x)
    incr
  else
    get.sec.incr(x, secs[-1], incr * secs[1], mul)
}

axis.PCICt <- function(side, x, at, format, labels = TRUE, ...) {
  mat <- missing(at) || is.null(at)
  if (!mat)
    x <- as.PCICt(at)
  else
    x <- as.PCICt(x)

  range <- par("usr")[if (side%%2) 1L:2L else 3L:4L]

  d <- range[2L] - range[1L]
  z <- c(.PCICt(range, cal=attr(x, "cal")), x[is.finite(x)])

  sc <- get.sec.incr(d, c(60, 60, 24, 7))
  if(missing(format) && !is.na(sc))
    format <- switch(as.character(sc), "1"="%S", "60"="%M:%S", "3600"="%H:%M", "86400"="%a %H:%M", "604800"="%a")

  if (d < 60 * 60 * 24 * 50) {
    zz <- pretty(z/sc)
    z <- zz * sc
    if (sc == 60 * 60 * 24)
      z <- round(z, "days")
    if (missing(format))
      format <- "%b %d"
  } else if (d < 1.1 * 60 * 60 * 24 * 365) {
    zz <- as.POSIXlt(z)
    zz$mday <- zz$wday <- zz$yday <- 1
    zz$isdst <- -1
    zz$hour <- zz$min <- zz$sec <- 0
    zz$mon <- pretty(zz$mon)
    m <- length(zz$mon)
    M <- 2 * m
    m <- rep.int(zz$year[1L], m)
    zz$year <- c(m, m + 1)
    zz <- lapply(zz, function(x) rep(x, length.out = M))
    z <- as.PCICt(zz, attr(x, "cal"))
    if (missing(format))
      format <- "%b"
  } else {
    zz <- as.POSIXlt(z)
    zz$mday <- zz$wday <- zz$yday <- 1
    zz$isdst <- -1
    zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
    zz$year <- pretty(zz$year)
    M <- length(zz$year)
    zz <- lapply(zz, function(x) rep(x, length.out = M))
    z <- as.PCICt(.POSIXlt(zz), attr(x, "cal"))
    if (missing(format))
      format <- "%Y"
  }
  if (!mat)
    z <- x[is.finite(x)]
  keep <- z >= range[1L] & z <= range[2L]
  z <- z[keep]
  if (!is.logical(labels))
    labels <- labels[keep]
  else if (identical(labels, TRUE))
    labels <- format(z, format = format)
  else if (identical(labels, FALSE))
    labels <- rep("", length(z))
  axis(side, at = z, labels = labels, ...)
}
