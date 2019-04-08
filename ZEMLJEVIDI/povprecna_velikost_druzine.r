# ========================================================================
# Povprečna velikost družine 
#
# Zemljevide bomo brali z ukazom `uvozi.zemljevid`, ki zemljevid na podanem
# naslovu pobere (če ga še nimate) in nato uvozi. Ker bomo za risanje
# zemljevidov uporabili knjižnico `ggplot2`, bomo dobljeni zemljevid obdelali
# še s funkcijo `pretvori.zemljevid`. Obe funkciji lahko vidite
# [v vzorčnem repozitoriju](https://github.com/jaanos/APPR-2017/blob/master/lib/uvozi.zemljevid.r)
# in ju že imate v repozitoriju vašega projekta.
# ================================================================@003590=
# 1. podnaloga
# Z ukazom
# 
#     obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip",
#                               "OB/OB", encoding = "Windows-1250") %>%
#       pretvori.zemljevid()
# 
# v spremenljivko `obcine` naložite zemljevid občin. Če ste vse pravilno
# naložili, bi moral ukaz
# 
#     ggplot() + geom_polygon(data = obcine, aes(x = long, y = lat,
#                                                group = group, fill = id)) +
#         guides(fill = FALSE)
# 
# narisati pisan zemljevid Slovenije.
# ========================================================================
# Uvoz potrebnih knjižnic
library(ggplot2)
library(dplyr)
source("https://raw.githubusercontent.com/jaanos/APPR-2017/master/lib/uvozi.zemljevid.r")

obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip",
                          "OB/OB", encoding = "Windows-1250") %>%
                          pretvori.zemljevid()
ggplot() + geom_polygon(data = obcine, 
                        aes(x = long, y = lat,
                            group = group, fill = id)) +
           guides(fill = FALSE)   #legende za barvo ne pokaže

# ================================================================@003591=
# 2. podnaloga
# Na strani Statističnega urada Republike Slovenije poglejte podatke o
# [občinah](http://pxweb.stat.si/pxweb/Database/Dem_soc/05_prebivalstvo/18_Druzine/15_05F70_druzine_OBC/15_05F70_druzine_OBC.asp).
# 
# 1. Izberite tabelo:
#     > Družine z otroki po številu otrok, občine, Slovenija, večletno
# 2. v stolpcu občin izberite vse vnose razen prvega (SLOVENIJA);
# 3. v stolpcu leto izberite leto 2015;
# 4. v zadnjem stolpcu izberite vse vrstice razen prve.
# 5. za način izpisa podatkov izberite
#     > Razmejena datoteka brez glave (.csv)
#    ter dobljeno CSV (comma separated values) datoteko shranite v isto
#    mapo kot poprej ter jo preimenujte v `druzine.csv`
# 
# Z ukazom
#druzine <- read_csv2("druzine.csv", col_names = c("obcina", 1:4),
#                    locale = locale(encoding = "Windows-1250")) %>%
#  melt(id.vars = "obcina", variable.name = "velikost", value.name = "stevilo") %>%
#  mutate(velikost = parse_number(velikost))
# 
# 
# dobljeno datoteko shranite v spremenljivko `druzine`.
# 
# Primerjajte imena v stolpcu `OB_UIME` razpredelnice `obcine` z imeni v
# stolpcu `obcina` razpredelnice `druzine` ter popravite slednji stolpec tako,
# da bo neurejen faktor z istimi vrednostmi kot prvi stolpec.
# ========================================================================
library(readr)
library(reshape2)

druzine <- read_csv2("druzine.csv", col_names = c("obcina", 1:4),
                     locale = locale(encoding = "Windows-1250")) %>%
  melt(id.vars = "obcina", variable.name = "velikost", value.name = "stevilo") %>%
  mutate(velikost = parse_number(velikost))
#levels vrne vrednosti faktorjev

lvls <- levels(obcine$OB_UIME)
druz <- unique(druzine$obcina)    #znakovni vektor in levels ne bo deloval, unique vrne vektor brez ponovitev
razlicni <- lvls != druz

primerjava <-data.frame(lvls,druz,
                        stringsAsFactors = FALSE)[razlicni,]
row.names(primerjava) <- NULL
primerjava [4:5, 2] <- primerjava[5:4, 2]
primerjava [13:14, 2] <- primerjava[14:13, 2]

for (i in 1:nrow(primerjava)) { 
  druzine$obcina[druzine$obcina
                 == primerjava$druz[i]] <- primerjava$lvls[i]
}
druzine$obcina <- factor(druzine$obcina, levels = lvls)
# ================================================================@003592=
# 3. podnaloga
# Ustvarite razpredelnico `povprecna.druzina` s stolpcema `obcina` in `povprecje`,
# ki za vsako občino pove velikost povprečne družine Pri računanju povprečja
# lahko za družine z več kot tremi otroki predpostavite, da imajo štiri otroke.
# ========================================================================
povprecna.druzina <- druzine %>% group_by(obcina) %>% 
  summarise(povprecje = sum(velikost * stevilo)/sum(stevilo))
# ================================================================@003593=
# 4. podnaloga
# Izrišite zemljevid Slovenije, pri katerem naj bo vsaka občina pobarvana
# glede na povprečno velikost družine.
# ========================================================================

ggplot() + 
  geom_polygon(data = left_join(obcine,
                                 povprecna.druzina%>%
                                   filter(povprecje > 1.5),
                                 by = c("OB_UIME" = "obcina")),
               aes(x = long, y = lat, group = group, 
                   fill = povprecje))








































































































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
      check$equal(nrow(obcine), 306371)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(nrow(druzine), 848)
      check$equal(ncol(druzine), 3)
      if (is.factor(druzine$obcina) && ! is.ordered(druzine$obcina)) {
        check$equal(all(levels(druzine$obcina) == levels(obcine$OB_UIME)), TRUE)
      } else {
        check$error("druzine$obcina ni neurejen faktor!")
      }
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(povprecna.druzina %>% filter(obcina == "Koper") %>%
                    select(povprecje) %>% as.numeric(), 1.472121)
      check$equal(povprecna.druzina %>% filter(obcina == "Gorenja vas-Poljane") %>%
                    select(povprecje) %>% as.numeric(), 1.962891)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    tryCatch({
      
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
    if (length(body) > 0) {
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
