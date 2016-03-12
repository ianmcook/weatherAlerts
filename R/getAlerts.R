#' Get active National Weather Service alerts
#'
#' Retrieve information about current United States National Weather Service
#' (\acronym{NWS}) watches, warnings, and advisories.
#'
#' @param includeStates 2-letter state abbreviations for the states for which
#'   alerts should be retrieved. Default is \code{NULL} indicating all 50 states
#'   and the District of Columbia are included.
#' @param excludeStates 2-letter state abbreviations for the states to exclude
#'   from the output. A common choice is \code{c("AK", "HI")}. Default is
#'   \code{NULL} indicating no states are excluded.
#' @param spatial logical indicating whether to return alert area geometries. See value.
#' @details Alerts are currently limited to the 50 United States and the
#'   District of Columbia.
#' @return If package \pkg{weatherAlertAreas} is installed, then a
#'   \code{\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}}
#'   containing alert information and corresponding alert area geometries. If
#'   package \pkg{weatherAlertAreas} is not installed, then a data frame
#'   containing alert information.
#' @seealso Package \pkg{weatherAlertAreas}
#' @export
#' @importFrom RCurl getURL
#' @importFrom XML xmlParse
#' @importFrom XML xmlToList
getAlerts <- function(includeStates = NULL, excludeStates = NULL, spatial = TRUE) {

  states <- stateCodes

  if(!is.null(includeStates)) {
    includeStates <- toupper(includeStates)
    includeStates <- unique(includeStates)

    if(!all(includeStates %in% stateCodes)) {
      stop("Argument includeStates must be a vector of 2-character state codes.")
    }

    states <- includeStates
  }

  if(!is.null(excludeStates)) {
    excludeStates <- toupper(excludeStates)
    excludeStates <- unique(excludeStates)

    if(!all(excludeStates %in% stateCodes)) {
      stop("Argument excludeStates must be a vector of 2-character state codes.")
    }

    states <- setdiff(stateCodes, excludeStates)
  }

  if(length(states) == 1) {
    nwsUrl <- paste0("http://alerts.weather.gov/cap/", tolower(states), ".php?x=0")
  } else {
    nwsUrl <- "http://alerts.weather.gov/cap/us.php?x=0"
  }

  response <- getURL(nwsUrl)

  xmlDoc <- xmlParse(response, asText=TRUE)
  xmlList <- xmlToList(xmlDoc)
  entries <- xmlList[names(xmlList) == "entry"]

  entriesList <- lapply(entries, function(entry) {
    id <- entry$id
    if(is.null(id)) {
      return(NULL)
    }
    updated <- entry$updated
    if(is.null(updated)) {
      updated <- NA
    }
    published <- entry$published
    if(is.null(published)) {
      published <- NA
    }
    alertTitle <- entry$title
    if(is.null(alertTitle)) {
      alertTitle <- NA
    }
    alertSummary <- entry$summary
    if(is.null(alertSummary)) {
      alertSummary <- NA
    }
    event <- entry$event
    if(is.null(event)) {
      event <- NA
    }
    effective <- entry$effective
    if(is.null(effective)) {
      effective <- NA
    }
    expires <- entry$expires
    if(is.null(expires)) {
      expires <- NA
    }
    status <- entry$status
    if(is.null(status)) {
      status <- NA
    }
    msgType <- entry$msgType
    if(is.null(msgType)) {
      msgType <- NA
    }
    alertCategory <- entry$category
    if(is.null(alertCategory)) {
      alertCategory <- NA
    }
    urgency <- entry$urgency
    if(is.null(urgency)) {
      urgency <- NA
    }
    severity <- entry$severity
    if(is.null(severity)) {
      severity <- NA
    }
    certainty <- entry$certainty
    if(is.null(certainty)) {
      certainty <- NA
    }
    areaDesc <- entry$areaDesc
    if(is.null(areaDesc)) {
      areaDesc <- NA
    }

    areaPolygon <- entry$polygon
    if(is.null(areaPolygon)) {
      areaPolygon <- ""
    }

    geocode <- entry$geocode
    geocodeKeys <- which(names(geocode)=="valueName")
    fipsKey <- as.integer(geocodeKeys[which(geocode[geocodeKeys]=="FIPS6")]+1)
    if(length(geocode) >= fipsKey) {
      fipsValue <- geocode[[fipsKey]]
    } else {
      fipsValue <- ""
    }
    if(is.null(fipsValue)) {
      fipsValue <- ""
    }

    ugcKey <- as.integer(geocodeKeys[which(geocode[geocodeKeys]=="UGC")]+1)
    if(length(geocode) >= ugcKey) {
      ugcValue <- geocode[[ugcKey]]
    } else {
      ugcValue <- ""
    }
    if(is.null(ugcValue)) {
      fipsValue <- ""
    }

    stateCode <- substr(ugcValue,1,2)
    if(!stateCode %in% states) {
      return(NULL)
    }
    stateName <- stateNames[stateCode]

    data.frame(id=id, updated=updated, published=published, title=alertTitle, summary=alertSummary, event=event,
               effective=effective, expires=expires, status=status, msgType=msgType, category=alertCategory,
               urgency=urgency, severity=severity, certainty=certainty,
               area=areaDesc, state=stateCode, stateName=stateName,
               polygon=areaPolygon, FIPS=fipsValue, UGC=ugcValue, stringsAsFactors=F)
  })

  if(length(entriesList) > 0) {
    entriesDataFrame <- do.call(rbind.data.frame, entriesList)
    entriesDataFrame$updated <- convertTime(entriesDataFrame$updated)
    entriesDataFrame$published <- convertTime(entriesDataFrame$published)
    entriesDataFrame$effective <- convertTime(entriesDataFrame$effective)
    entriesDataFrame$expires <- convertTime(entriesDataFrame$expires)
    rownames(entriesDataFrame) <- NULL
  } else {
    entriesDataFrame <-
      structure(list(id = character(0), updated = structure(numeric(0),
      class = c("POSIXct", "POSIXt")), published = structure(numeric(0),
      class = c("POSIXct", "POSIXt")), title = character(0), summary = character(0),
      event = character(0), effective = structure(numeric(0), class = c("POSIXct","POSIXt")),
      expires = structure(numeric(0), class = c("POSIXct","POSIXt")),
      status = character(0), msgType = character(0), category = character(0),
      urgency = character(0), severity = character(0), certainty = character(0),
      area = character(0), state = character(0), stateName = character(0),
      polygon = character(0), FIPS = character(0), UGC = character(0)),
      class = "data.frame", row.names = character(0))
  }

  if(!is.null(states)) {
    entriesDataFrame <- entriesDataFrame[entriesDataFrame$state %in% states, ]
  }

  if(spatial) {
    if(!requireNamespace("weatherAlertAreas", quietly = TRUE)) {
      spatial <- FALSE
      packageStartupMessage("Alert area geometries not returned because package weatherAlertAreas is not installed.")
    }
  }

  if (spatial) {

    library(sp)

    polygonAreas <- nchar(entriesDataFrame$polygon) > 0
    UGCAreas <- nchar(entriesDataFrame$UGC) >= 6
    noAreas <- !polygonAreas & !UGCAreas
    if(sum(noAreas) > 0) {
      entriesDataFrame <- entriesDataFrame[!noAreas, ]
      warning(sum(noAreas), " alerts are missing area information and have been removed.")
    }

    entriesPolygons <- as.list(rep(NA, nrow(entriesDataFrame)))

    if(any(polygonAreas)) {
      entriesStringPolygons <- apply(
        X = entriesDataFrame[polygonAreas, ],
        MARGIN = 1,
        FUN = function(entryRow) {
          entryId <- entryRow[["id"]]
          polygonString <- entryRow[["polygon"]]
          coords <- t(vapply(
            X = strsplit(polygonString, " ")[[1]],
            FUN = function(coord) {
              as.numeric(rev(strsplit(coord, ",")[[1]]))
            },
            FUN.VALUE = numeric(2),
            USE.NAMES = FALSE
          ))
          Polygons(list(Polygon(coords)), ID = entryId)
        }
      )

      entriesPolygons[polygonAreas] <- entriesStringPolygons
    }

    if(any(!polygonAreas)) {
      entriesUGCPolygons <- apply(
        X = entriesDataFrame[!polygonAreas, ],
        MARGIN = 1,
        FUN = function(entryRow) {
          entryId <- entryRow[["id"]]
          entryUGC <- strsplit(entryRow[["UGC"]], " ")[[1]]
          # UGC codes ending with C000, Z000, CALL, and ZALL all reference whole states
          entryUGC <- gsub("[CZ](000|ALL)$", "", entryUGC)
          rgeos::gUnaryUnion(
            spgeom = weatherAlertAreas::alertAreas[entryUGC],
            id = rep(entryId, length(entryUGC))
          )@polygons[[1]]
        }
      )

      entriesPolygons[!polygonAreas] <- entriesUGCPolygons
    }

    entriesSpatialPolygons <- SpatialPolygons(
      entriesPolygons,
      proj4string = CRS("+proj=longlat +datum=WGS84")
    )

    return(SpatialPolygonsDataFrame(
      entriesSpatialPolygons,
      entriesDataFrame,
      match.ID = FALSE
    ))

  } else {

    return(entriesDataFrame)

  }
}

# helper functions and objects

stateCodes <-
  c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL",
    "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
    "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
    "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

stateNames <-
  c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "District of Columbia", "Delaware", "Florida",
    "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
    "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
    "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
    "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
    "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
    "West Virginia", "Wisconsin", "Wyoming")

names(stateNames) <- stateCodes

convertTime <- function(timeStr) {
  as.POSIXct(gsub("\\:([0-9]{2}$)","\\1", timeStr), "GMT", format="%Y-%m-%dT%H:%M:%S%z")
}

.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("weatherAlertAreas", quietly = TRUE)) {
    packageStartupMessage("You should install the weatherAlertAreas pacakge!")
  }
}

