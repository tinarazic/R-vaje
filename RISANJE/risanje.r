# ========================================================================
# Risanje 
#
# Za risanje bomo uporabili knjižnico `ggplot2`. Pri delu s podatki si bomo
# pomagali še s knjižnico `dplyr`.
# 
#     library(ggplot2)
#     library(dplyr)
# 
# Pri risanju s knjižnico `ggplot2` uporabimo ukaz `ggplot`, ki mu podamo
# podatke, iz katerih bomo izpeljali graf. Funkcija vrne nedokončan graf,
# ki ga še ne moremo narisati. Zato mu bo treba "prišteti" še (vsaj en) tip
# grafa, ki ga želimo, in morda še kakšne dodatne nastavitve:
# 
#    - funkcije `geom_point()`, `geom_line()`, `geom_path()` in `geom_step()`
#      določajo grafe točk, črt, poti oziroma stopnic;
#    - funkciji `geom_histogram()` in `geom_bar()` določata histograme oziroma
#      stolpčne diagrame;
#    - s funkcijami `geom_hline()`, `geom_hline()` in  `geom_abline()` lahko
#      rišemo vodoravne in navpične premice oziroma premice s poljubnim
#      naklonom;
#    - s funkcijama `geom_rect()` in `geom_polygon()` lahko rišemo pravokotnike
#      oziroma splošne večkotnike;
#    - s funkcijo `geom_text()` lahko na graf dodamo besedilo;
#    - s funkcijo `coord_polar()` lahko določimo, da bomo uporabljali polarne
#      koordinate;
#    - s funkcijo `stat_function()` lahko dodamo krivuljo na graf;
#    - s funkcijo `aes()` določimo, iz katerih podatkov bomo risali graf (lahko
#      jo podamo kar kot argument h `ggplot`).
# 
# Vsakič, ko narišemo osnovni graf, se stari graf pobriše.
# 
#     # 500-krat si na različne načine izberimo naključno x in y koordinato
#     ggplot(data.frame(x=runif(500), y=runif(500))) + aes(x=x, y=y) + geom_point()
#     ggplot(data.frame(x=runif(500), y=rnorm(500))) + aes(x=x, y=y) + geom_point()
#     ggplot(data.frame(x=rnorm(500), y=rnorm(500))) + aes(x=x, y=y) + geom_point()
#     # Slika naključnega sprehoda v ravnini.
#     ggplot(data.frame(x=cumsum(runif(30, -1, 1)), y=cumsum(runif(30, -1, 1)))) + aes(x=x, y=y) + geom_path()
#     # Graf sinusne funkcije
#     ggplot(data.frame(x=c(-2*pi, 2*pi))) + aes(x) + stat_function(fun=sin)
# 
# Seveda lahko dodamo še več elementov. Model "seštevanja" nam omogoča, da graf postopoma gradimo.
# 
#     # Pripravimo graf s sinusno funkcijo, a brez informacije o koordinati x
#     g <- ggplot(data.frame(x=c(-2*pi, 2*pi))) + stat_function(fun=sin)   #ggplot ne ve katere vrednosti, da uporaboii
#     g + aes(x) # Za koordinato x vzamemo stolpec x v podatkih
#     # Prikažimo še točke samo na izbranih koordinatah
#     h <- g + geom_point(data = data.frame(x = seq(-2, 2, 0.25)*pi)) + aes(x, y = sin(x))
#     # Če želimo iz programa narisati graf, uporabimo funkcijo print
#     print(h)
# 
# S funkcijami `xlab`, `ylab` in `ggtitle` lahko grafu dodamo oznaki osi x in y oziroma naslov.
# 
#     h + xlab("x") + ylab("y") + ggtitle("y = sin(x)")
# 
# S funkcijo `theme` lahko podamo več dodatnih parametrov glede izgleda grafa:
# 
# - Parameter `panel.background` nastavi izgled ozadja grafa - podamo mu funkcijo
#   `element_rect`, tej pa dodatne nastavitve, npr `fill` za barvo ozadja. Barve
#   lahko podamo z imeni (`"white"`, `"red"`, `"green"`, ...), z RGB komponentami;
#   (na primer `"#00F16B"`), ali s pomočjo funkcij `rgb`, `hsv`, `hcl`, ...
# - Parametra `panel.grid.major` in `panel.grid.minor` nastavita izgled mrežnih
#   črt - podamo jima funkcijo `element_line`, tej pa parametre `color` za barvo,
#   `size` za širino oziroma `linetype` za tip črte (`"solid"`, `"dashed"`,
#   `"dotted"`, `"dotdash"` itd.). Če na konec imena parametra dodamo `.x` ali
#   `.y`, nastavitev velja le za navpične oziroma vodoravne črte.
# - Parameter `plot.title` nastavi izgled naslova grafa - podamo mu funkcijo
#   `element_text`, tej pa parametre `color` za barvo, `size` za velikost itd.
# 
# Parametrov je še veliko več. Vse si lahko ogledate pri vgrajeni pomoči za
# ukaz `theme`.
#   
#     g <- g + aes(x) + theme(panel.background = element_rect(fill="white"))    #ozadje bele barve
#     g <- g + geom_point(data = data.frame(x=0, y=0), aes(x=x, y=y), color = "gray", shape = 1, size = 5)   #dodali smo eno točko (0,0); shape = 1 --> KROG..imaš različne številke, ki pomenijo različne oblike
#     g <- g + geom_point(data = data.frame(x=c(-3 * pi / 2, pi / 2), y=c(1, 1)), aes(x=x, y=y), color = "red", shape = 2, size = 4)    # hspae = 2 --> TRIKOTNIK
#     g <- g + geom_point(data = data.frame(x=c(-pi / 2, 3 * pi / 2), y=c(-1, -1)), aes(x=x, y=y), color = "green", shape = 6, size = 4)    #shape = 6 --> obrnjen trikontik
# 
# Narišimo še nekaj grafov iz obstoječih podatkov. Uporabili bomo razpredelnico
# `MathAchieve` iz knjižnice `nlme` s podatki o dosežkih učencev pri matematiki.
# 
#     library(nlme)
#     head(MathAchieve)    #matachieve ni tidy data ker je meanses IZPELJAN PODATEK!!
# 
#     # vsi podatki
#     ggplot(MathAchieve) + aes(x = SES, y = MathAch) + geom_point()
#     # samo šola z oznako 1224
#     library(dplayr)    #da lahko uporabljamo veriženje
#     p <- ggplot(MathAchieve %>% filter(School == 1224))
#     p + aes(x = SES, y = MathAch) + geom_point()
#     # pobarvajmo po spolu
#     p + aes(x = SES, y = MathAch, color = Sex) + geom_point()
#     # oznaka še po manjšini
#     p + aes(x = SES, y = MathAch, color = Sex, shape = Minority) + geom_point()
#     # razdelitev po spolu in manjšini
#     p + aes(x = Sex, fill = Minority) + geom_bar()  
#     # tortni diagram po spolu
#     p + aes(x = factor(1), fill = Sex) + geom_bar(width = 1) +    
#         coord_polar(theta = "y") + xlab("") + ylab("")
# ================================================================@003589=
# 1. podnaloga
# Nariši točkovni graf, ki ima na osi x vrednosti iz stolpca `SES`, na osi y
# vrednosti iz stolpca `MEANSES`, z barvo je prikazan spol, z obliko točke
# (`shape`) pripadnost manjšini, z velikostjo točke (`size`) pa dosežek
# pri matematiki (`MathAch`). Graf naj prikazuje samo tiste podatke iz
# razpredelnice `MathAchieve`, pri katerih je vrednost `MathAch` v zgornjih
# 5 odstotkih (pomagaj si s funkcijo `quantile`).
# 
# Dokončan graf spravi v spremenljivko `graf` ter ga nato izpiši z ukazom
# `print`.
# ========================================================================
library(ggplot2)
library(dplyr)
library(nlme)

top5 <- quantile(MathAchieve$MathAch,0.95)
graf <- ggplot(MathAchieve %>% filter(MathAch >= top5)) + 
  aes(x = SES, y = MEANSES, color = Sex, size = MathAch,
      shape = Minority) + geom_point()
print(graf)

# ggplot(MathAchieve %>% filter(MathAch >= top5)) + 
#    aes(x = SES, y = MEANSES, color = MathAch, 
#          +         shape = Minority) + geom_point()     #odtenek pove številke:)

#za več poglej na spletno stran GGPLOT.ORG





































































































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
      check$equal(nrow(graf$data), 360)
      check$equal(as.character(graf$mapping$x), "SES")
      check$equal(as.character(graf$mapping$y), "MEANSES")
      check$equal(as.character(graf$mapping$colour), "Sex")
      check$equal(as.character(graf$mapping$x), "SES")
      check$equal(as.character(graf$mapping$shape), "Minority")
      check$equal(as.character(graf$mapping$size), "MathAch")
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
