# ========================================================================
# Funkcije 
#
# Funkcije definiramo z
# 
#     ime.funkcije <- function(argumenti) {
#       definicija
#     }
# 
# Načeloma je vrednost funkcije enaka vrednosti zadnjega izraza v njeni
# definiciji. Če pa želimo, pa lahko rezultat funkcije vrnemo z ukazom
# `return(rezultat)`. Ukaz `return` vrne rezultat ter konča izvajanje
# funkcije.
# 
# Na primer, kvadratno funkcijo bi definirali kot
# 
#     kvadrat <- function(x) {
#       x ^ 2
#     }
# 
# lahko pa tudi kot
# 
#     kvadrat <- function(x) {
#       return(x ^ 2)
#     }
# 
# Funkcije lahko sprejmejo tudi več argumentov, na primer:
# 
#     povprecje <- function(x, y) {
#       (x + y) / 2
#     }
# 
# Tako kot v Pythonu so nekateri argumenti lahko tudi neobvezni.
# Določimo jih s tem, da jim pripišemo privzeto vrednost, na primer:
# 
#     eksponentna <- function(x, baza = exp(1)) {
#       baza ^ x
#     }
# 
# Take funkcije lahko kličemo:
# 
# 1. kot običajno: `eksponentna(3, 2)` vrne `8`;
# 2. brez neobveznih argumentov: `eksponentna(3)` vrne `20.08554`;
# 3. s poimenovanimi argumenti: `eksponentna(2, baza = 3)` vrne `9`.
# ================================================================@003561=
# 1. podnaloga
# Sestavite funkcijo `stopinje(radiani)`, ki pretvarja iz radianov v
# kotne stopinje.
# ========================================================================
stopinje <- function(radiani) {
  radiani *180/pi
}
# ================================================================@003562=
# 2. podnaloga
# Sestavite funkcijo `celziji(fahrenheiti)`, ki pretvarja iz
# Fahrenheitov v stopinje Celzija.
# ========================================================================
celziji <- function(fahrenheiti){
  (fahrenheiti -32)*5/9
}
# ================================================================@003563=
# 3. podnaloga
# Sestavite funkcijo `zaokrozi(stevilo)`, ki dano število zaokroži na 
# 0,5 natančno.
# 
# _Namig:_ uporabite funkcijo `round` in manjši trik.
# ========================================================================
zaokrozi <- function(stevilo){
  round(stevilo*2)/2
}
# ================================================================@003564=
# 4. podnaloga
# Sestavite funkcijo `pozdrav(ime, vzklik)`, ki vrne niz, ki predstavlja
# pozdrav, ki se začne z danim vzklikom, ki ima privzeto vrednost
# `"Zdravo"`.
# Na primer, `pozdrav("Miha", "Ojla")` naj vrne niz `"Ojla, Miha!"`,
# `pozdrav("Nina")` pa naj vrne `"Zdravo, Nina!"`.
# ========================================================================
pozdrav <- function(ime,vzklik="Zdravo"){
  paste0(vzklik,", ",ime,"!")
}
# ================================================================@003565=
# 5. podnaloga
# Sestavite funkcijo `velikost.slike(diagonala, razmerje = 16 / 9)`, ki
# sprejme velikost diagonale televizije ter vrne niz, ki predstavlja
# dimenziji slike. Funkcija naj sprejme neobvezen argument, ki pove
# razmerje slike. Dimenzije naj bodo zaokrožene na celo število.
# 
# Na primer, `velikost.slike(100)` vrne niz `"87 x 49"`.
# ========================================================================
velikost.slike <- function(diagonala,razmerje= 16/9){
  visina <- diagonala/ (razmerje^2 +1)^(1/2)
  sirina <- razmerje * visina
  paste(round(sirina),"x",round(visina))
}








































































































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

  
  if (check$part()) {
    tryCatch({
      check$equal(stopinje(pi / 4), 45)
      check$equal(stopinje(3 * pi), 540)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(celziji(455), 235)
      check$equal(celziji(914), 490)
      check$equal(celziji(-40), -40)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(zaokrozi(13.3), 13.5)
      check$equal(zaokrozi(13.6), 13.5)
      check$equal(zaokrozi(13.8), 14)
      check$equal(zaokrozi(-13.8), -14)
      check$equal(zaokrozi(-13.6), -13.5)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(pozdrav("Miha", "Ojla"), "Ojla, Miha!")
      check$equal(pozdrav("Nina"), "Zdravo, Nina!")
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(velikost.slike(100), "87 x 49")
      check$equal(velikost.slike(150), "131 x 74")
      check$equal(velikost.slike(30, razmerje = 4 / 3), "24 x 18")
      check$equal(velikost.slike(40, razmerje = 1.618034), "34 x 21")
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  

  cat('Shranjujem rešitve na strežnik... ')
  tryCatch({
    r <- POST(
      'https://www.projekt-tomo.si/api/attempts/submit/',
      body = lapply(check$parts, function(part) {
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
    for(i in 1:length(check$parts)) {
      valid.before <- check$parts[[i]]$valid
      if (!is.null(updates[[check$parts[[i]]$part]])) {
        for (field in names(updates[[check$parts[[i]]$part]])) {
          check$parts[[i]][[field]] <- updates[[check$parts[[i]]$part]][[field]]
        }
      }
      valid.after <- check$parts[[i]]$valid
      if (valid.before && ! valid.after) {
        wrong.index <- response$wrong_indices[[as.character(check$parts[[i]]$part)]]
        if (! is.null(wrong.index)) {
          hint <- check$parts[[i]]$secret[[wrong.index+1]][2]
          if (nchar(hint) > 0) {
            check$parts[[i]]$feedback <- c(check$parts[[i]]$feedback, paste("Namig:", hint))
          }
        }
      }
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
