# ========================================================================
# Krmilni stavki 
#
# Krmilni stavki v R-ju se obnašajo podobno kot v Pythonu.
# Pogojni stavek napišemo kot:
# 
#     if(pogoj) {
#       ukazi, ki se izvedejo, če pogoj drži
#     } else {
#       ukazi, ki se izvedejo, če pogoj ne drži
#     }
# 
# Zanka `while` prav tako deluje podobno kot v Pythonu:
# 
#     while(pogoj) {
#       ukazi, ki se izvajajo, dokler pogoj drži
#     }
# 
# Poleg zanke `while` R pozna tudi zanko `repeat`, ki se obnaša tako kot
# zanka `while`, v kateri je pogoj vedno enak `TRUE`.
# Če želimo zaustaviti izvajanje zanke `repeat`, uporabimo ukaz `break`,
# ki se obnaša tako kot v Pythonu, ali pa ukaz `return`.
# Če želimo skočiti na naslednji korak trenutne zanke, pa uporabimo
# ukaz `next`, ki se obnaša tako kot `continue` v Pythonu.
# 
#     prastevilo <- function(n) {
#       i <- 2
#       while(i < n) {
#         if(n %% i == 0)
#           return(FALSE)
#         i <- i + 1
#       }
#       return(TRUE)
#     }
# ================================================================@003566=
# 1. podnaloga
# Sestavite funkcijo `gcd(m, n)`, ki z Evklidovim algoritmom izračuna
# največji skupni delitelj števil `m` in `n`.
# ========================================================================
gcd <- function(m,n){
  while(n != 0) {
    r <- m %% n
    m <- n
    n <- r
  }
  return(m)
}
# ================================================================@003567=
# 2. podnaloga
# Sestavite funkcijo `fibonacci(n, a = 0, b = 1)`, ki izračuna `n`-to Fibonaccijevo
# število. Neobvezna parametra `a` in `b` naj določata ničto oziroma prvo število v zaporedju.
# Upoštevajte še, da so lahko indeksi tudi negativni.
# ========================================================================
fibonacci <-function(n,a=0,b=1){
  if(n > 0){
    i <- 2
    while(n > 1){
      n <- n-1
      c <- a + b
      a <- b
      b <- c
    }
    return(b)
  }else {
    while (n < 0){
      n <- n + 1 
      c <- b - a
      b <- a
      a <- c
    }
    return(a)
  }
}
# ================================================================@003568=
# 3. podnaloga
# Sestavite funkcijo `collatz(n)`, ki izračuna dolžino Collatzovega
# zaporedja, ki se začne s številom `n`.
# ========================================================================
collatz <- function(n){
  i <- 0
  while(n!=1){
    if(n %% 2 == 0){
        n <- n/2
        i <- i + 1
      }
    else{ 
      n <- 3*n +1
    i <- i + 1}
  }
  return(i)
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
      check$equal(gcd(42, 12), 6)
      check$equal(gcd(13, 5), 1)
      check$equal(gcd(8, 20), 4)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(fibonacci(1), 1)
      check$equal(fibonacci(2), 1)
      check$equal(fibonacci(4), 3)
      check$equal(fibonacci(-3), 2)
      check$equal(fibonacci(-4), -3)
      check$equal(fibonacci(5, 6, 7), 53)
      check$equal(fibonacci(-2, -3, -4), -2)
      check$equal(fibonacci(14), 377)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(collatz(5), 5)
      check$equal(collatz(10), 6)
      check$equal(collatz(27), 111)
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
