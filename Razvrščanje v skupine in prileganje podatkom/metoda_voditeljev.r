# ========================================================================
# Metoda voditeljev 
#
# Z metodo voditeljev podatke združujemo v skupine tako, da najprej
# naključno izberemo želeno število "centroidov", nato pa vsak podatek
# priredimo skupini, ki ima najbližji centroid. Nato na novo izračunamo
# centroide skupin in postopek ponavljamo, dokler se nam skupine še
# spreminjajo.
# 
# Delali bomo s podatki Mednarodnega denarnega sklada o [makroekonomskih
# obetih](http://ucilnica.fmf.uni-lj.si/mod/resource/view.php?id=8836),
# ki jih bomo narisali na zemljevid sveta.
# 
# Uvozimo datoteko s podatki in zemljevid:
# 
#     library(ggplot2)
#     library(dplyr)
#     library(readr)
#     source("https://raw.githubusercontent.com/jaanos/APPR-2017/master/lib/uvozi.zemljevid.r")
#     svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
#       "ne_50m_admin_0_countries", encoding = "Windows-1252") %>% pretvori.zemljevid()
#     imf <- read_csv2("imf.csv") %>% mutate(GEO = parse_factor(GEO, levels(svet$name_long)))
# 
# Razdelitev na želeno število skupin dobimo s funkcijo `kmeans`, ki ji
# podamo ustrezno normalizirane podatke:
# 
#     imf.norm <- imf %>% select(-GEO) %>% scale()
#     rownames(imf.norm) <- imf$GEO
#     k <- kmeans(imf.norm, 5)
# 
# Objekt `k` sestavlja več atributov. Vektor `k$cluster` tako pove, v
# katero skupino spada določen podatek.
# 
#     head(k$cluster, n = 15)
#     table(k$cluster)
# 
# Matrika `k$centers` vsebuje podatke o centroidih skupin.
# 
#     View(k$centers)
# 
# Atribut `k$tot.withinss` je število, ki meri vsoto kvadratov odstopanj
# znotraj skupin in je kot tak mera za kvaliteto razdelitve. Ker je
# začetni korak pri metodi voditeljev naključen, je tudi izhod naključen.
# 
#     k <- kmeans(imf.norm, 5)
#     k$tot.withinss            # vsakič bomo imeli drugačno oceno
# 
# Da bi dobili čim boljšo razdelitev, lahko funkciji `kmeans` povemo, naj
# naredi več razdelitev in vrne najboljšo.
# 
#     
# 
# Narišimo našo razdelitev na zemljevid sveta.
# 
#     skupine <- data.frame(GEO = imf$GEO, skupina = factor(k$cluster))
#     ggplot() + geom_polygon(data = svet %>% left_join(skupine, by = c("name_long" = "GEO")),
#                             aes(x = long, y = lat, group = group, fill = skupina))
# ================================================================@003595=
# 1. podnaloga
# Napiši funkcijo `zemljevid.sveta(n)`, ki naj nariše zemljevid sveta z
# barvanjem glede na razdelitev držav na `n` skupin z metodo voditeljev.
# Pri tem naj se opravi `1000` razdelitev. Funkcija naj vrne vektor z
# velikostmi posameznih skupin.
# ========================================================================
# Uvoz potrebnih knjižnic
library(ggplot2)
library(dplyr)
source("https://raw.githubusercontent.com/jaanos/APPR-2017/master/lib/uvozi.zemljevid.r")

zemljevid.sveta <- function(n) {
  k <- kmeans(imf.norm,n,nstart = 1000)
  skupine <- data.frame(GEO = imf$GEO, skupina = factor(k$cluster))
  ggplot() + geom_polygon(data = svet %>% left_join(skupine, by = c("name_long" = "GEO")),
                               aes(x = long, y = lat, group = group, fill = skupina)) %>%
    print()
  table(k$cluster)
}
# ================================================================@003596=
# 2. podnaloga
# Napiši funkcijo `centroidi(k)`, ki za dano razdelitev `k` vrne vektor
# imen držav, ki so najbližje centroidom vsake skupine. Za mero bližine
# vzemi vsoto kvadratov razlike normaliziranih podatkov za državo s
# centroidom.
# ========================================================================
centoridi <- function(k) {
  razdalje <- apply(k$centers,1,
                    function(x) apply(imf.norm,1,function(y) sum((x-y)^2)))
  min.razdalje <- apply(razdalje,2,min)
  bool.razdalje <- apply(razdalje,1,function(x) x == min,razdalje)
  imf$GEO[apply(bool.razdalje,1,which)]
}

#imf.tidy <-melt(imf)
#imf.cast <- dcast(imf.tidy,GEO ~ value)






































































































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
      z5 <- zemljevid.sveta(5)
      z10 <- zemljevid.sveta(10)
      check$equal(length(z5), 5)
      check$equal(sum(z5), 157)
      check$equal(length(z10), 10)
      check$equal(sum(z10), 157)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    tryCatch({
      test.k <- kmeans(imf.norm, 10, nstart = 1000)
      test.centroidi <- centroidi(test.k)
      check$equal(length(test.centroidi), 10)
      check$equal(test.k$cluster[test.centroidi], 1:10)
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
