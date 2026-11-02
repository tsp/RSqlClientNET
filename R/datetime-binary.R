# Binary SQL datetime/datetime2/smalldatetime wall-clock conversion.
# SCB2 kind W transports IEEE-754 seconds decoded from 100-ns .NET ticks,
# representing the SQL wall clock *as though UTC*. The numeric vector is NOT
# a UTC instant. Resolve UTC offsets by local calendar day, not by parsing a
# million textual timestamps. Only transition dates need per-row validation.
# DST gap/fold handling intentionally matches .sql_native_timestamp().
.sql_binary_wall_to_posixct <- function(wall, tz = "", row_offset = 0,
                                        column_name = NULL, dst_fallback = "na",
                                        dst_state = NULL) {
  dst_fallback <- .sql_dst_fallback(dst_fallback)
  tz <- .sql_validate_tz(tz)
  if (!is.numeric(wall)) stop("Binary SQL wall clocks must be numeric.", call.=FALSE)
  result <- rep(NA_real_, length(wall))
  at <- which(!is.na(wall))
  if (!length(at)) return(as.POSIXct(result, origin="1970-01-01", tz=tz))
  if (any(!is.finite(wall[at])))
    stop("Non-finite binary SQL wall timestamp.", call.=FALSE)

  value <- wall[at]
  day_id <- floor(value / 86400)
  days <- unique(day_id)
  day_index <- match(day_id, days)
  calendar_days <- as.Date(days, origin="1970-01-01")
  # Difference of parsed instants also retains historical UTC offsets with
  # second precision (e.g. pre-standard-time zones); %z only gives HHMM.
  noon_offset <- function(x) {
    text <- paste(as.character(x), "12:00:00")
    utc_noon <- suppressWarnings(as.POSIXct(text,
      format="%Y-%m-%d %H:%M:%S", tz="UTC"))
    local_noon <- suppressWarnings(as.POSIXct(text,
      format="%Y-%m-%d %H:%M:%S", tz=tz))
    as.numeric(utc_noon) - as.numeric(local_noon)
  }
  middle <- noon_offset(calendar_days)
  before <- noon_offset(calendar_days - 1L)
  after <- noon_offset(calendar_days + 1L)
  utc <- value - middle[day_index]
  ambiguous <- rep(FALSE, length(value))
  bad <- !is.finite(utc)

  # Most days do not have a timezone transition. Resolve only days where the
  # preceding/following local-noon offsets differ. Validate both candidate
  # UTC instants by a local-wall round trip; detect folds and gaps exactly.
  transitions <- which(!is.na(before) & !is.na(after) & before != after)
  for (d in transitions) {
    idx <- which(day_index == d)
    if (!length(idx)) next
    stamp <- as.POSIXct(value[idx], origin="1970-01-01", tz="UTC")
    target <- format(stamp, "%Y-%m-%dT%H:%M:%S", tz="UTC", usetz=FALSE)
    early <- value[idx] - before[[d]]
    late <- value[idx] - after[[d]]
    matches_before <- format(as.POSIXct(early, origin="1970-01-01", tz="UTC"),
      "%Y-%m-%dT%H:%M:%S", tz=tz, usetz=FALSE) == target
    matches_after <- format(as.POSIXct(late, origin="1970-01-01", tz="UTC"),
      "%Y-%m-%dT%H:%M:%S", tz=tz, usetz=FALSE) == target
    matches_before[is.na(matches_before)] <- FALSE
    matches_after[is.na(matches_after)] <- FALSE
    ambiguous[idx] <- matches_before & matches_after
    bad[idx] <- !matches_before & !matches_after
    utc[idx] <- ifelse(matches_before, early,
                      ifelse(matches_after, late, NA_real_))
  }

  # The whole stream is needed for the ordered DST heuristic. A state
  # environment survives across dbFetch() and callback batch boundaries.
  if (identical(dst_fallback, "ordered") && any(ambiguous)) {
    if (is.null(dst_state)) dst_state <- new.env(parent=emptyenv())
    previous_batch <- dst_state$last_wall %||% NA_real_
    winter_day <- dst_state$winter_day %||% NA_real_
    winter <- isTRUE(dst_state$winter)
    amb_idx <- which(ambiguous)
    for (i in amb_idx) {
      day <- day_id[[i]]
      if (is.na(winter_day) || day != winter_day) {
        winter_day <- day
        winter <- FALSE
      }
      previous <- if (i > 1L) value[[i-1L]] else previous_batch
      # SQL NULLs were removed from value, hence the previous value is the
      # preceding non-NULL wall clock (as in the existing textual algorithm).
      if (!is.na(previous) && floor(previous / 86400) == day &&
          value[[i]] < previous) winter <- TRUE
      if (winter) utc[[i]] <- value[[i]] - after[[day_index[[i]]]]
      else utc[[i]] <- value[[i]] - before[[day_index[[i]]]]
    }
    dst_state$winter_day <- winter_day
    dst_state$winter <- winter
    .sql_warn_wall_rows("ambiguous (repeated clock-hour)",
      row_offset + at[ambiguous], tz, column_name, resolution="ordered")
  } else if (any(ambiguous)) {
    .sql_warn_wall_rows("ambiguous (repeated clock-hour)",
      row_offset + at[ambiguous], tz, column_name)
    utc[ambiguous] <- NA_real_
  }
  if (identical(dst_fallback, "ordered") && !is.null(dst_state))
    dst_state$last_wall <- utils::tail(value, 1L)

  # UTC offset could be unavailable for out-of-range R / platform dates.
  bad <- bad | !is.finite(utc)
  bad[ambiguous & dst_fallback == "na"] <- FALSE
  if (any(bad)) .sql_warn_wall_rows("nonexistent/unrepresentable",
    row_offset + at[bad], tz, column_name)
  utc[bad] <- NA_real_
  result[at] <- utc
  as.POSIXct(result, origin="1970-01-01", tz=tz)
}
