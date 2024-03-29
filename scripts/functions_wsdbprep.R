#functions used in WSDB prep

#lat long clean----------
# Coordinate conversion from Degrees Decimal.Minutes to DD
#read in csv of data with Degrees Decimal Minute Lat/Long in the first 4 separate variables 

convert_to_decimal_degrees <- function(coord) {
  # Remove degree symbols and other unwanted characters such as cardinal directions
  coord <- iconv(coord, from = "latin1", to = "ASCII", sub = "")
  coord <- gsub("°|'|[NSEW]", "", coord)
    # Handle coordinates with 'd' for degrees 
  coord <- gsub("d", " ", coord)
  coord <- trimws(coord)
  

  # Insert a space between degrees and minutes if needed for concatenated formats
  # Adjusting for coordinates that start with '0'
  if (grepl("^0\\d{3,}", coord)) {
    coord <- paste0(substr(coord, 1, 3), " ", substr(coord, 4, nchar(coord)))
  } else if (grepl("^\\d{4,}", coord)) {
    coord <- paste0(substr(coord, 1, 2), " ", substr(coord, 3, nchar(coord)))
  }
  
  # Remove any characters except numbers, space, and decimal point
  cleaned_coord <- gsub("[^-0-9. ]", "", coord)
  
  # Correct instances of double decimal points
  if (grepl("\\.{2,}", cleaned_coord)) {
    cleaned_coord <- gsub("\\.{2,}", ".", cleaned_coord)
  }
  
  
  # Split by space to get degrees, minutes, and seconds
  components <- unlist(strsplit(cleaned_coord, " "))
  
  # Check the number of components to determine the format
  n <- length(components)
  
  if (n == 1) {
    # Already in decimal format
    return(round(as.numeric(components[1]), 5))
  } else if (n == 2) {
    # Degrees and decimal minutes
    degrees <- as.numeric(components[1])
    minutes <- as.numeric(components[2])
    return(round(degrees + (minutes / 60), 5))
  } else if (n == 3) {
    # Degrees, minutes, and seconds
    degrees <- as.numeric(components[1])
    minutes <- as.numeric(components[2])
    seconds <- as.numeric(components[3])
    return(round(degrees + (minutes / 60) + (seconds / 3600), 5))
  } else {
    # Unknown format
    return(NA)
  }
}

# Function to troubleshoot and find the problematic latitude
troubleshoot_latitudes <- function(latitudes) {
  problems <- NULL
  
  for (i in seq_along(latitudes)) {
    result <- convert_to_decimal_degrees(latitudes[i])
    if (is.na(result)) {
      problems <- c(problems, latitudes[i])
    }
  }
  
  return(problems)
}

#checks for extra whitespace, double colons, no colons. Time in hh:mm and hh:mm:ss

standardize_time <- function(time_string) {
  # Check if time_string is NA or empty
  if (is.na(time_string) || time_string == "") {
    return(NA)
  }
  
  # Remove any whitespace from the time string
  time_string <- gsub("\\s+", "", time_string)
  
  # Replace double or more colons with a single colon
  time_string <- gsub(":{2,}", ":", time_string)
  
  # Check if the format is without colons and has 3 or 4 digits (HHMM or HMM)
  if (grepl("^[0-9]{3,4}$", time_string)) {
    # Insert colon between hours and minutes, accommodating both HMM and HHMM formats
    time_string <- sub("^(\\d{1,2})(\\d{2})$", "\\1:\\2", time_string)
  }
  
  
  # Extract hours, minutes, and optionally seconds
  parts <- unlist(strsplit(time_string, ":"))
  
  # Check for valid time parts and pad with leading zeros if necessary
  if (length(parts) >= 2 && length(parts) <= 3) {
    hours <- sprintf("%02d", as.numeric(parts[1]))
    minutes <- sprintf("%02d", as.numeric(parts[2]))
    
    # Format time depending on whether seconds are present
    if (length(parts) == 3) {
      seconds <- sprintf("%02d", as.numeric(parts[3]))
      formatted_time <- paste(hours, minutes, seconds, sep = ":")
    } else {
      formatted_time <- paste(hours, minutes, sep = ":")
    }
    
    return(formatted_time)
  }
  
  # Return NA for invalid formats
  return(NA)
}





#clean and format date to excel date #--------
### format various Date formats 
convert_to_clean_date <- function(date_strings) {
  
  # Function to extract the year from the filename
  extract_year_from_filename <- function(filename) {
    matches <- regmatches(filename, regexpr("[0-9]{4}", filename))
    if (length(matches) > 0 && nchar(matches) == 4) {
      return(matches)
    } else {
      return(NA) # Return NA if no four-digit number is found or if it's not the right length
    }
  }
  
  # Extract year from filename
  file_year <- extract_year_from_filename(input_file)
  
  
  # Initialize an empty vector to store parsed dates
  parsed_dates <- vector("list", length(date_strings))
  
  # Loop through each date string
  for (i in seq_along(date_strings)) {
    date_string <- date_strings[i]
    
    # Check for "day-month" format without the year e.g., 14-Sep
    if (grepl("^[0-9]{1,2}-[A-Za-z]{3}$", date_string) && !is.na(file_year)) {
      date_string <- paste(date_string, file_year, sep = "-")
      parsed_dates[[i]] <- dmy(date_string)
    }
    
    # Check for "30-Dec-22" format
    if (grepl("-[A-Za-z]{3}-", date_string)) {
      parsed_dates[[i]] <- dmy(date_string, tz = NULL)
    }
    # Check for "day-month-year" numeric format
    else if (grepl("^[0-9]{1,2}-[0-9]{1,2}-", date_string)) {
      parsed_dates[[i]] <- dmy(date_string, tz = NULL)
    }
    # Assume "month-day-year" format if other formats don't match
    else {
      parsed_dates[[i]] <- mdy(date_string, tz = NULL)
    }
  }
  
  # Replace NA values with a placeholder or keep as NA
  valid_dates <- !sapply(parsed_dates, is.na)
  
  # Convert parsed dates to "YYYY-MM-DD" format
  formatted_dates <- rep(NA_character_, length(parsed_dates))
  formatted_dates[valid_dates] <- sapply(parsed_dates[valid_dates], function(x) format(x, "%Y-%m-%d"))
  
  return(formatted_dates)
}

convert_to_xls_date <- Vectorize(function(clean_date) {
  # Calculate the difference in days
  excel_date_number <- as.numeric(difftime(clean_date, excel_origin, units = "days"))
  
  return(excel_date_number)
})


####
#adjust time based on region------
####

adjust_to_utc <- function(time, region) {

# Define time zone offsets in minutes
  offsets <- c("AR" = 3*60, "GULF" = 3*60, "MAR" = 3*60, "NL" = 2.5*60, 
               "QC" = 4*60, "PAC" = 7*60, "O&P" = 5*60, "OTHER" = NA)

# Adjust the time
if (!is.na(region) && region %in% names(offsets)) {
  adjusted_time <- time + dminutes(offsets[region])
  # Convert to UTC
  adjusted_time <- with_tz(adjusted_time, tzone = "UTC")
} else {
  warning("Invalid or missing region code: ", region)
  return(NA)
}

return(adjusted_time)
}

adjust_to_local <- function(time, region) {
  
  # Define time zone offsets in minutes
  offsets <- c("AR" = 3*60, "GULF" = 3*60, "MAR" = 3*60, "NL" = 2.5*60, 
               "QC" = 4*60, "PAC" = 7*60, "O&P" = 5*60, "OTHER" = NA)
  
  # Adjust the time
  if (!is.na(region) && region %in% names(offsets)) {
    adjusted_time <- time - dminutes(offsets[region])
    # Convert to UTC
    adjusted_time <- with_tz(adjusted_time, tzone = "UTC")
  } else {
    warning("Invalid or missing region code: ", region)
    return(NA)
  }
  
  return(adjusted_time)
}
