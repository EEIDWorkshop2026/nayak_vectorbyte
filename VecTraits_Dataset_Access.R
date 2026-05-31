# VectorByte Dataset Access Script
# Usage Instructions at: https://docs.google.com/document/d/1e0WLCyGJZfdzbLneyqfm4yKiTBzBugCvoA4Lu5MnIec/preview


useQA <<- FALSE


getWebData <- function(dataURL) {
  if (!"httr" %in% installed.packages()) {
    cat("Installing necessary httr library...\n")
    install.packages("httr")
  }
  if (!"jsonlite" %in% installed.packages()) {
    cat("Installing necessary jsonlite library...\n")
    install.packages("jsonlite")
  }
  if (!exists("webDataLibrariesOpen")) {
    library(httr)
    library(jsonlite)
    webDataLibrariesOpen <- TRUE
  }
  webData <- GET(url = dataURL)
  if (status_code(webData) >= 300 || status_code(webData) < 200) {
    returnValue <- data.frame(
      message = "Data fetch failed.",
      HTTPcode = status_code(webData)
    )
    return(returnValue)
  }
  returnValue <- fromJSON(
    content(webData, "text", encoding = "UTF-8"),
    flatten = TRUE
  )
  return(returnValue)
}


addLeadingZeros <- function(num, digits = 2) {
  if (nchar(as.character(num)) >= digits) {
    return(as.character(num))
  } else {
    return(paste(c(rep("0",digits-nchar(as.character(num))),as.character(num)),collapse=""))
  }
}


getDataset <- function(ID = -1) {
  totalDatasets <<- as.integer(getWebData(paste(c(
    "https://vectorbyte",
    c("-qa","")[as.integer(useQA)],
    ".crc.nd.edu/portal/api/vectraits-explorer/?format=json"
  ), collapse = ""))$data$count)
  if (as.integer(ID) <= totalDatasets && as.integer(ID) > 0) {
    datasetID <- ID
  } else {
    while (TRUE) {
      Sys.sleep(0.2)
      datasetID <- readline(prompt = "Enter a dataset ID: ")
      if (as.integer(datasetID) <= totalDatasets && as.integer(datasetID) > 0) {
        break
      } else {
        cat(paste("The dataset ID", datasetID, "is invalid or is out of range.\n"))
        cat(paste("Please choose a number between 1 and", totalDatasets, "\b.\n"))
      }
    }
  }
  dataset <- getWebData(
    paste(
      c(
        "https://vectorbyte",
        c("-qa","")[as.integer(useQA)],
        ".crc.nd.edu/portal/api/vectraits-dataset/",
        datasetID,
        "/?format=json"
      ),
      collapse = ""
    )
  )
  if (as.character(dataset)[1] == "Data fetch failed.") {
    cat("Uh Oh!\nAn HTTP Error Occured and dataset", datasetID, "could not be retrieved.\n")
    cat("HTTP Error Code:", dataset$HTTPcode, "\b\n\n")
  }
  return(dataset)
}


getDatasets <- function(IDS, safety = TRUE, l = 25L) {
  Sys.sleep(0.2)
  IDs <- IDS
  if (length(IDs) > l) {
    cat("You may not retrieve more than", l, "datasets at a time.\n")
    cat("Would you like to retrieve only the first", l, "datasets?\n")
    answer <- tolower(readline())
    if (grepl("y", answer) && !grepl("n", answer)) {
      IDs <- IDs[1:l]
    } else {
      return()
    }
  }
  if (safety && length(IDs) > 50) {
    cat("Are you sure you want to retrieve all", length(IDs), "datasets?\n")
    cat(paste("This would take about", ceiling(length(IDs) / 50), "min.\n"))
    answer <- tolower(readline())
  } else {
    cat(paste("This will take about", ceiling(60 * length(IDs) / 50), "seconds.\n"))
    answer <- "y"
  }
  if (grepl("y", answer) && !grepl("n", answer)) {
    total <- length(IDs)
    setNumber <- 0
    failedDatasets <<- c()
    datasets <- list()
    cat("Retrieving datasets....\n> 00%")
    for (datasetID in IDs) {
      setNumber <- setNumber + 1
      flush.console()
      datasets[[setNumber]] <- getWebData(
        paste(
          c(
            "https://vectorbyte",
            c("-qa","")[as.integer(useQA)],
            ".crc.nd.edu/portal/api/vectraits-dataset/",
            as.character(datasetID),
            "/?format=json"
          ),
          collapse = ""
        )
      )
      # Extend loading bar:
      if (as.character(datasets[[setNumber]])[1] == "Data fetch failed.") {
        cat("\b\b\b\b\bX")
        failedDatasets <<- c(failedDatasets, datasetID)
        datasets[[setNumber]] <- NA
      } else {
        cat("\b\b\b\b\b=")
      }
      cat("> ")
      flush.console()
      # Display new percentage:
      cat(addLeadingZeros(floor(100 * setNumber / total)), "\b%")
    }
    cat("\b\b\b\b\b\b 100% \nData retrieval complete!\n")
    if (length(failedDatasets) > 1) {
      cat("The following", length(failedDatasets), "datasets contained HTTP errors and could not be retrieved:\n")
      print(failedDatasets)
    } else if (length(failedDatasets) == 1) {
      cat("Dataset", failedDatasets[1], "\b, however, could not be found.\n")
    }
    return(datasets)
  }
}


searchDatasets <- function(KEYWORD = "", safety = TRUE) {
  keyword <- KEYWORD
  while (nchar(keyword) < 3) {
    Sys.sleep(0.2)
    keyword <- readline(prompt = "Enter a keyword to search for in all datasets: ")
    if (nchar(keyword) < 3) {
      cat("Please enter a more descriptive keyword.\n")
    }
  }
  cat("Searching Datasets....\n")
  flush.console()
  setSearch <- getWebData(
    paste(
      c(
        "https://vectorbyte",
        c("-qa","")[as.integer(useQA)],
        ".crc.nd.edu/portal/api/vectraits-explorer/?format=json&keywords=",
        gsub(" ", "%20", keyword)
      ),
      collapse = ""
    )
  )
  if (as.character(setSearch)[1] == "Data fetch failed.") {
    cat("Uh Oh!\nAn HTTP error has occurred:", setSearch$HTTPcode, "\n")
    cat("This could be because the search term you entered was too general (too many results).\n")
    cat("Please try again:\n")
    searchDatasets()
  } else {
    cat(length(setSearch$ids), "relevant datasets found.\n")
    if (length(setSearch$ids) > 0) {
      return(getDatasets(setSearch$ids, safety))
    } else {
      return(list())
    }
  }
}


searchDatasetsMulti <- function(KEYWORDS = c(), safety = TRUE) {
  if (length(KEYWORDS) == 0) {
    Sys.sleep(0.2)
    cat("Please enter a list of keywords to search for in the datasets:\n")
    keywords <- c()
    while (length(keywords) == 0) {
      keywords <- scan(what = "")
    }
  } else {
    keywords <- KEYWORDS
  }
  cat("Searching Datasets....\n")
  flush.console()
  setSearch <- getWebData(
    paste(
      c(
        "https://vectorbyte",
        c("-qa","")[as.integer(useQA)],
        ".crc.nd.edu/portal/api/vectraits-explorer/?format=json&keywords=",
        gsub(" ", "%20", paste(keywords, collapse = "%20"))
      ),
      collapse = ""
    )
  )
  if (as.character(setSearch)[1] == "Data fetch failed.") {
    cat("Uh Oh!\nAn HTTP error has occurred:", setSearch$HTTPcode, "\n")
    cat("This could be because the search term you entered had too many results or no results.\n")
    cat("Please try again:\n")
    searchDatasetsMulti()
  } else {
    cat(length(setSearch$ids), "relevant datasets found.\n")
    if (length(setSearch$ids) > 0) {
      return(getDatasets(setSearch$ids, safety))
    } else {
      return(list())
    }
  }
}


smartSearch <- function(VARIABLE_NAME, VARIABLE_VALUE, OPERATOR = "contains", safety = TRUE) {
  operator <- tolower(OPERATOR)
  if (operator != "contains") {
    if (operator == "contain" || operator == "has") { operator <- "contains" }
    if (operator == "!contain" || operator == "!contains" || operator == "!has" || operator == "!have" || operator == "does not contain") { operator <- "ncontains" }
    if (operator == "=" || operator == "==" || operator == "equal" || operator == "equals") { operator <- "eq" }
    if (operator == "!=" || operator == "not" || operator == "!equal" || operator == "!equals") { operator <- "neq" }
    if (operator == "starts with" || operator == "start with" || operator == "starts" || operator == "start") { operator <- "sw" }
    if (operator == "not start with" || operator == "!start" || operator == "!starts") { operator <- "nsw" }
  }
  variable_name <- VARIABLE_NAME
  if (tolower(variable_name) == "genus") { variable_name <- "Interactor1Genus" }
  if (tolower(variable_name) == "species") { variable_name <- "Interactor1Species" }
  if (tolower(variable_name) == "gender") { variable_name <- "Interactor1Sex" }
  if (tolower(variable_name) == "who") { variable_name <- "SubmittedBy" }
  if (tolower(variable_name) == "stage") { variable_name <- "Interactor1Stage" }
  cat("Searching Datasets....\n")
  flush.console()
  setSearch <- getWebData(
    paste(
      c(
        "https://vectorbyte",
        c("-qa","")[as.integer(useQA)],
        ".crc.nd.edu/portal/api/vectraits-explorer/?format=json&field=",
        gsub(" ", "%20", variable_name),
        "&operator=",
        operator,
        "&term=",
        gsub(" ", "%20", VARIABLE_VALUE)
      ),
      collapse = ""
    )
  )
  if (as.character(setSearch)[1] == "Data fetch failed.") {
    if (setSearch$HTTPcode == 400) {
      cat("Uh Oh!\nThe server does not wish to fulfill your request.\n")
      cat("This could be because you included unsupported/reserved URL characters, such as:\n")
      cat(", ! @ # $ % ^ & : ; \\ \" ' ? / < > emojis etc.\n")
      cat("This could also be because too many results matched your search.\n")
      cat("(Don't search for \"Animalia\" in \"Interactor1Kingdom\", for example.)\n")
      cat("This could also be because you used the incorrect capitalization in the variable name entry.\n")
      if (OPERATOR != "contains") {
        cat("This could also very likely be because the operator you entered is not supported.\n")
        cat("The following operators are supported by the server:\n")
        cat("contains, ncontains, eq (equals), neq (not equal to), sw (starts with),\nnsw (doesn't start with), in, nin\n")
      }
    } else {
      cat("Uh Oh!\nAn HTTP error has occurred:", setSearch$HTTPcode, "\n")
      if (setSearch$HTTPcode == 404) {
        cat("The most likely reason for this is that no results matched your search.")
      }
    }
  } else {
    cat(length(setSearch$ids), "relevant datasets found.\n")
    if (length(setSearch$ids) > 0) {
      return(getDatasets(setSearch$ids, safety))
    } else {
      return(list())
    }
  }
}


sciName <- function(DATASET) {
  return(paste(
    DATASET$results$Interactor1Genus[1],
    DATASET$results$Interactor1Species[1]
  ))
}


datasetRows <- function(DATASET) {
  return(length(DATASET$results$DatasetID))
}


datasetColumns <- function(DATASET) {
  return(length(DATASET$results[1,]))
}


datasetSummary <- function(DATASET) {
  cat(paste(
    toupper(DATASET$results$OriginalTraitDef[1]),
    "in",
    toupper(DATASET$results$OriginalTraitUnit[1]),
    "\b:\n"
  ))
  print(summary(DATASET$results$OriginalTraitValue))
  if (!is.na(DATASET$results$Interactor1Common[1])) {
    cat("Experiments done on", DATASET$results$Interactor1Common[1])
    if (is.na(DATASET$results$Interactor2Common[1])) {
      cat("s.\n")
    } else {
      cat(paste(
        c(
          "s and ",
          DATASET$results$Interactor2Common[1],
          "s.\n"
        ),
        collapse = ""
      ))
    }
  }
  cat(datasetRows(DATASET), "samples total.\n")
}


pick <- function(SELECTION = 0) {
  if (SELECTION == 0) {
    cat("MENU:\n [1] Retrieve dataset by ID\n [2] Retrieve datasets by IDs\n [3] Search for datasets by keyword\n [4] Search for datasets by list of keywords\n [5] Search for datasets by variable and value\n")
    Sys.sleep(0.25)
    answer <- as.integer(readline(prompt = "Enter a number from the menu above to select it. "))
  } else {
    answer <- SELECTION
  }
  Sys.sleep(0.25)
  if (answer == 1) {
    return(getDataset())
  }
  if (answer == 2) {
    cat("Enter a list of dataset IDs below.\n")
    datasetIDs <- scan(what = integer())
    return(getDatasets(datasetIDs))
  }
  if (answer == 3) {
    return(searchDatasets())
  }
  if (answer == 4) {
    return(searchDatasetsMulti())
  }
  if (answer == 5) {
    vname <- readline(prompt = "Variable Name: ")
    Sys.sleep(0.2)
    vval <- readline(prompt = "Variable Value: ")
    return(smartSearch(vname, vval))
  }
}

library(tidyverse)
library(rgl)

# Put some data in here
x <- c(1,2,3,4)
y <- c(9.5, 11, 19.6, 20)

# Plot it - looks kind of like a line
plot(x, y)

#Let's look through a few parameter values for the slope and see what the result looks like

# Make a search grid of parameter values
beta0 <- seq(0,10,0.1)
beta1 <- seq(0,10,0.1)
betas <- expand.grid(beta0 = beta0, 
                     beta1 = beta1)

# Get SSR values for each beta set
for (i in 1:nrow(betas)){
  
  beta0 <- betas$beta0[i]
  beta1 <- betas$beta1[i]
  SSR <- sum((y - (beta0 + beta1*x))^2)
  
  betas$SSR[i] <- SSR
  
}