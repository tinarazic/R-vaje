# ========================================================================
# Imena in razpredelnice 
#
# Do komponent vektorjev lahko dostopamo tudi z njihovimi imeni. Na primer
# `islands` je vektor površin največjih otokov na Zemlji. Do komponent lahko
# dostopamo kot poprej
# 
#     islands[c(13, 16, 8)] # 13., 16. in 8. otok, urejeno po abecedi
#     islands[islands < 100] # otoki s površino, manjšo od 100 kvadratnih milj
# 
# ali direktno po imenih
# 
#     islands["Britain"] # površina Velike Britanije
#     islands[c("Honshu", "Kyushu", "Hokkaido", "Shikoku")] # Japonski otoki
# 
# Če imena ni, dobimo vrednost NA (not available).
# 
#     islands["Blejski otok"]
# 
# V ozadju R uporablja vektor imen, do katerega lahko dostopamo z ukazom
# 
#     names(islands)
# 
# Vektorje z imeni lahko s funkcijo `c` sestavimo tudi direktno
# 
#     matematika <- c(4, 2, 5, 3)
#     names(matematika) <- c("Janez", "Micka", "Lojzka", "Franci")
# 
# Če želimo uvesti urejenost za imenske spremenljivke, si lahko pomagamo s
# funkcijo `factor`.
# 
#     opisne.ocene <- c("manj uspešno", "uspešno", "zelo uspešno")
#     telovadba <- c("manj uspešno", "zelo uspešno", "uspešno", "uspešno")
#     Telovadba <- factor(telovadba, levels = opisne.ocene, ordered = TRUE)
# 
# S funkcijo `data.frame` lahko podatke iz vektorjev spravimo v razpredelnico.
# 
#     Sola <- data.frame(matematika, sportna.vzgoja = Telovadba)
# 
# Iz razpredelnice lahko dobimo posamezne vrstice in stolpce oziroma celice.
# 
#     Sola["Franci",]
#     Sola[, "matematika"]
#     Sola["Micka", "sportna.vzgoja"]
# ================================================================@003574=
# 1. podnaloga
# Sestavite vektor `otoki`, ki vsebuje iste podatke kot vektor `islands`,
# le da so površine izražene v kvadratnih kilometrih.
# ========================================================================
otoki <- islands * 1.609344^2
# ================================================================@003575=
# 2. podnaloga
# Sestavi razpredelnico `Otoki` s podatki iz vektorjev `islands` in
# `otoki` s stolpci `kvadratni.kilometri`, `kvadratne.milije` in
# `kategorija`, pri čemer je zadnja urejenostna spremenljivka z
# vrednostmi
# 
# * `"celina"`, če je površina otoka nad 5000 km^2,
# * `"velik otok"`, če je površina med 100 km^2 in 5000 km^2,
# * `"majhen otok"`, če je površina pod 100 km^2.
# 
# Velikosti v kvadratnih kilometrih naj bodo zaokrožene na celo število.
# ========================================================================
kategorija <- character(length(islands))
kategorija[otoki > 5000] <- "celina"
kategorija[otoki <= 5000 & otoki >= 100] <- "velik otok"
kategorija[otoki < 100] <- "majhen otok"
kategorije <- c("majhen otok","velik otok","celina")
Otoki <- data.frame(kvadratni.kilometri = round(otoki),
                    kvadratne.milije = islands,
                    kategorija = factor(kategorija,
                                        levels =kategorije,
                                        ordered = TRUE))








































































































# =======================================================================@
# Kode pod to črto nikakor ne spreminjajte.
# ========================================================================

"TA VRSTICA JE PRAVILNA."
"ČE VAM R SPOROČI, DA JE V NJEJ NAPAKA, SE MOTI."
"NAPAKA JE NAJVERJETNEJE V ZADNJI VRSTICI VAŠE KODE."
"ČE JE NE NAJDETE, VPRAŠAJTE ASISTENTA."




























































if (length(showConnections()) > 1) {
  .filename <- showConnections()[1, "description"]
} else {
  .filename <- Find(Negate(is.null), Map(function(f) { f$ofile }, sys.frames()), right=TRUE)
}


.check <- function() {
  .error <- FALSE
.errfun <- function(e) {
    warning(e)
    .error <<- TRUE
}
tryCatch({
    library(rjson)
}, error = .errfun)
tryCatch({
    library(httr)
}, error = .errfun)

if (.error) {
    stop("Required libraries are unavailable. Please make sure that rjson and httr are available.")
}

regex_break <- function(whole_regex, regexes, source) {
    whole_matches <- gregexpr(paste("(?sm)", whole_regex, sep=""), source, perl=TRUE)[[1]]
    whole_matches <- mapply(
        function(start, end) substr(source, start, end),
        whole_matches,
        whole_matches + attr(whole_matches, "match.length") - 1
    )
    m <- length(whole_matches)
    n <- length(regexes)
    matches <- matrix("", nrow=m, ncol=n)
    for (i in 1:m) {
        whole <- whole_matches[i]
        for (j in 1:n) {
            rest_regex <- paste(regexes[-(1 : j)], collapse="")
            part_regex <- paste("(?sm)\\A", regexes[j], "(?=", rest_regex, "\\Z)", sep="")
            match <- regexpr(part_regex, whole, perl=TRUE)
            end <- attr(match, "match.length")
            matches[i, j] <- substr(whole, 1, end)
            whole <- substr(whole, end + 1, nchar(whole))
        }
    }
    matches
}

strip <- function(str) gsub("^\\s+|\\s+$", "", str)
rstrip <- function(str) gsub("\\s+$", "", str)

super_strip <- function(str) {
    str <- gsub("(^|\n)# ?", "\n", str)
    gsub("\\A\\s+|\\s+\\Z", "", str, perl=TRUE)
}

pretty.print <- function(x) {
  output <- capture.output(print(x))
  if(length(output) == 0) {
    return("NULL")
  } else if(length(output) == 1) {
    return(output)
  } else {
    return(paste("    ", c("", output, ""), collapse = "\n"))
  }
}


  check <- list()

check$initialize <- function(parts) {
  init.part <- function(part) {
    part$valid <- TRUE
    part$feedback <- list()
    part$secret <- list()
    if (part$part) part$id <- part$part
    return(part)
  }
  check$parts <<- lapply(parts, init.part)
  check$current <<- NA
  check$part.counter <<- NA
}

check$part <- function() {
  if(is.na(check$part.counter)) {
    check$part.counter <<- 1
  } else {
    check$part.counter <<- check$part.counter + 1
  }
  return(strip(check$parts[[check$part.counter]]$solution) != "")
}

check$feedback <- function(msg, ...) {
  check$parts[[check$part.counter]]$feedback <<-
    c(check$parts[[check$part.counter]]$feedback, sprintf(msg, ...))
}

check$error <- function(msg, ...) {
  check$parts[[check$part.counter]]$valid <<- FALSE
  check$feedback(msg, ...)
}

check$secret <- function(x, hint = "") {
  pair <- c(toString(check$canonize(x)), toString(hint))
  check$parts[[check$part.counter]]$secret<<-
    c(check$parts[[check$part.counter]]$secret, list(pair))
}

check$run <- function(example, state) {
  # yet to be implemented
}

check$canonize <- function(x, digits = 6) {
  if(typeof(x) == "double" || typeof(x) == "complex") {
    return(round(x, digits))
  } else if(typeof(x) == "list") {
    return(lapply(x, function(y) check$canonize(y, digits)))
  } else {
    return(x)
  }
}

check$equal <- function(example, value = NA, exception = NA,
                        clean = function(x) x,
                        precision = 1.0e-6, strict.float = FALSE, check.attributes = FALSE) {
  difference <- function(x, y) {
    if(identical(x, y)) return(NA)
    else if(isTRUE(all.equal(x, y, check.attributes = check.attributes))) return(NA)
    else if(typeof(x) != typeof(y) && (strict.float || !(mode(x) != mode(y))))
      return("različna tipa")
    else if(length(x) != length(y))
      return("različno število komponent")
    else if(mode(x) == 'numeric' && mode(y) == 'numeric') {
      if(any(abs(x - y) > precision))
        return("numerična napaka")
      else
        return(NA)
    }
    else return("različni vrednosti")
  }
  example <- substitute(example)

  if(!is.na(exception)) {
    tryCatch({
      returned <- eval(example)
      check$error("Izraz %s vrne %s namesto da bi sprožil izjemo '%s'.",
                  deparse(example), pretty.print(returned), exception)
    }, error = function(e) {
      if(e$message != exception)
        check$error("Izraz %s sproži izjemo '%s' namesto '%s'.",
                    deparse(example), e$message, exception)
    })
  } else {
    returned <- eval(example)
    reason <- difference(clean(returned), clean(value))
    if(!is.na(reason)) {
      check$error("Izraz %s vrne %s namesto %s (%s)",
                  deparse(example), pretty.print(returned), pretty.print(value), reason)
    }
  }
}

check$random <- function(example, period = 10, sample = 100, uniqueness = 0.9) {
  example <- substitute(example)
  results <- replicate(sample, toString(check$canonize(replicate(period, eval(example)))))
  if (length(unique(results)) < uniqueness * sample) {
    check$error("Izraz %s ne vrača naključnih rezultatov.", deparse(example))
  }
}

check$probability <- function(example, interval, sample = 100) {
  example <- substitute(example)
  results <- replicate(sample, isTRUE(eval(example)))
  prob <- sum(results) / sample
  if (!(interval[1] < prob && prob <= interval[2])) {
    check$error("Izraz %s velja z verjetnostjo %.2f, ki je izven pričakovanega intervala [%.2f, %.2f].", deparse(example), prob, interval[1], interval[2])
  }
}

check$expected <- function(example, interval, sample = 100) {
  example <- substitute(example)
  results <- replicate(sample, eval(example))
  prob <- sum(results) / sample
  if (!(interval[1] < prob && prob <= interval[2])) {
    check$error("Povprečna vrednost izraza %s je %.2f, kar je izven pričakovanega intervala [%.2f, %.2f].", deparse(example), prob, interval[1], interval[2])
  }
}

check$summarize <- function() {
  for(i in 1:length(check$parts)) {
    if(strip(check$parts[[i]]$solution) == "") {
      cat("Podnaloga", i, "je brez rešitve.\n")
    } else if (! check$parts[[i]]$valid) {
      cat("Podnaloga", i, "nima veljavne rešitve.\n")
    } else {
      cat("Podnaloga", i, "ima veljavno rešitev.\n")
    }
    for (message in check$parts[[i]]$feedback) {
        cat("- ", message, "\n", sep = "")
    }
  }
}

  check$challenge <- check$secret

  .source <- paste(readLines(.filename), collapse="\n")

  matches <- regex_break(paste(
      '# =+@(\\d+)=\n',    # beginning of header
      '(#( [^\n]*)?\n)+',  # description
      '# =+\n',            # end of header
      '.*?',               # solution
      '(?=\n# =+@)',       # beginning of next part
      sep=""
  ),  c(
      '# =+@',             # beginning of header
      '(\\d+)',            # beginning of header (?P<part>)
      '=\n',               # beginning of header
      '(#( [^\n]*)?\n)+',  # description
      '# =+\n',            # end of header
      '.*?'                # solution
  ), .source)

  check$initialize(
    apply(matches, 1, function(match) list(
        part = as.numeric(match[2]),
        solution = match[6]
      )
    )
  )
  check$parts[[length(check$parts)]]$solution <- rstrip(check$parts[[length(check$parts)]]$solution)

  body <- list()
  indices <- c()
  
  if (check$part()) {
    tryCatch({
      check$equal(round(otoki['Java'], 3), 126.909)
      check$equal(round(otoki['Britain'], 3), 217.559)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    tryCatch({
      if(!is.data.frame(Otoki)) {
        check$error("Spremenljivka Otoki ni razpredelnica.")
      }
      if(any(names(Otoki) != c("kvadratni.kilometri", "kvadratne.milije", "kategorija"))) {
        check$error("Razpredelnica Otoki nima pravih stolpcev.")
      }
      if(!is.ordered(Otoki[,"kategorija"])) {
        check$error("Stolpec kategorija ni urejenostna spremenljivka.")
      }
      if(!all(Otoki[Otoki["kategorija"] == "celina","kvadratni.kilometri"] > 5000)
         || !all(Otoki[Otoki["kategorija"] == "velik otok","kvadratni.kilometri"] >= 100)
         || !all(Otoki[Otoki["kategorija"] == "majhen otok","kvadratni.kilometri"] < 100)) {
        check$error("Kategorije niso pravilno dodeljene!.")
      }
      if(Otoki["Europe", "kategorija"] <= Otoki["Baffin", "kategorija"]
         || Otoki["Borneo", "kategorija"] <= Otoki["Banks", "kategorija"]) {
        check$error("Kategorije niso pravilno razvrščene!.")
      }
      check$equal(Otoki['Java', "kvadratni.kilometri"], 127)
      check$equal(Otoki['Britain', "kvadratne.milije"], 84)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  

  cat('Shranjujem rešitve na strežnik... ')
  tryCatch({
    r <- POST(
      'https://www.projekt-tomo.si/api/attempts/submit/',
      body = lapply(body, function(part) {
        part$secret <- lapply(part$secret, function(x) x[1])
        part
      }),
      encode = "json",
      add_headers(Authorization = 'Token 840c6db2891f669ba2f3e6e7088911585701a262')
    )
    response <- content(r)
    cat('Rešitve so shranjene.\n')
    updates <- list()
    for (part in response$attempts) {
      updates[[part$part]] <- part
    }
    for(i in 1:length(body)) {
      valid.before <- body[[i]]$valid
      if (!is.null(updates[[body[[i]]$part]])) {
        for (field in names(updates[[body[[i]]$part]])) {
          body[[i]][[field]] <- updates[[body[[i]]$part]][[field]]
        }
      }
      valid.after <- body[[i]]$valid
      if (valid.before && ! valid.after) {
        wrong.index <- response$wrong_indices[[as.character(body[[i]]$part)]]
        if (! is.null(wrong.index)) {
          hint <- body[[i]]$secret[[wrong.index+1]][2]
          if (nchar(hint) > 0) {
            body[[i]]$feedback <- c(body[[i]]$feedback, paste("Namig:", hint))
          }
        }
      }
      check$parts[[indices[i]]] <- body[[i]]
    }
    if("update" %in% names(response)) {
      cat("Posodabljam datoteko... ")
      index <- 1
      while(file.exists(paste(.filename, ".", index, sep = "")))
        index <- index + 1
      backup.filename = paste(.filename, ".", index, sep = "")
      file.copy(.filename, backup.filename)
      r <- readLines(response$update, encoding="UTF-8", warn=FALSE)
      f <- file(.filename, encoding="UTF-8")
      writeLines(r, f)
      close.connection(f)
      cat("Stara datoteka je preimenovana v ", basename(backup.filename), ".\n", sep = "")
      cat("Če se datoteka v urejevalniku ni osvežila, jo shranite ter ponovno zaženite.\n")
    }
    check$summarize()
  },
  error = function(r) {
    cat('Pri shranjevanju je prišlo do napake.\n')
    check$summarize()
    cat('Pri shranjevanju je prišlo do napake. Poskusite znova.\n')
  })
}

.check()
