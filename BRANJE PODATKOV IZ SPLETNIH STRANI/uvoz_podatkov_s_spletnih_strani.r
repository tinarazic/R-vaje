# ========================================================================
# Uvoz podatkov s spletnih strani 
#
# Spletne strani so zapisane v obliki HTML. To je opisni jezik, ki temelji
# na hierarhično razvrščenih značkah. Imena značk in njihovi atributi so
# navedeni med znakoma `<` in `>`, sledi pa vsebina značke. To je lahko
# besedilo, lahko pa tudi druge značke. Konec značke označuje značka, ki
# ima pred svojim imenom še znak `/`.
# 
# Podatki na spletnih straneh so najpogosteje zbrani v tabelah. Zanje se uporablja
# značka `<table>`; njene vrstice so predstavljene z značko `<tr>`, celice znotraj
# vrstic pa z značko `<td>`. Včasih naletimo tudi na značko `<tbody>`, ki vsebuje
# vrstice telesa tabele; celice glave tabele pa so včasih označene z značko `<th>`.
# Ostale pogoste značke lahko vidimo v naslednjem primeru dokumenta HTML.
# 
#     <html>
#       <!-- komentar -->
#       <head> <!-- glava dokumenta -->
#         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"> <!-- kodiranje znakov -->
#         <title>Naslov dokumenta</title> <!-- naslov okna ali zavihka -->
#       </head>
#       <body> <!-- telo dokumenta -->
#         <h1>Naslov</h1>
#         <p>Besedilo v odstavku.</p>
#         <p>
#           Nov odstavek. Lahko naredimo tudi prelom <br />
#           vrstice znotraj odstavka. To dosežemo tudi z <br>
#           - to sicer ni v skladu s standardom XML.
#         </p>
#         Besedilo je lahko tudi izven odstavka, kar večinoma ne bo vplivalo na izgled.
#         <table>
#           <tr>
#             <th>Glava</th>
#             <th>tabele</th>
#           </tr>
#           <tr>
#             <td>Prva vrstica</td>
#             <td>Drugi stolpec</td>
#           </tr>
#           <tr>
#             <td></td>
#             <td>Prejšnja celica je prazna</td>
#           </tr>
#           <tr>
#             <td>&nbsp;</td> <!-- če želimo prikazati prazno vrstico, -->
#             <td>&nbsp;</td> <!-- navadno vstavimo "nedeljiv presledek" -->
#           </tr>
#         </table>
#         <img src="slika.png" /> <!-- vstavljanje slik -->
#         <a href="http://www.fmf.uni-lj.si">Povezava</a> na spletno stran FMF.
#       </body>
#     </html>
# 
# Podatki na spletnih straneh so namenjeni človeškim očem, zato pogosto vsebujejo
# komentarje in druge nečistoče, ki nas lahko motijo pri analizi. Tako bomo te
# navadno želeli pobrisati oziroma jih vsaj odmakniti od dejanskih podatkov.
# Za uvoz podatkov s spletne strani bomo uporabili knjižnico `rvest`.
# Pri čiščenju podatkov bomo delali z nizi, kar nam bo olajšala knjižnica `gsubfn`.
# Pomagali si bomo tudi s funkcijami iz knjižnice `readr`. Knjižnico `dplyr`
# bomo uporabili za poizvedovanje po podatkih.
# 
#     library(rvest)
#     library(gsubfn)
#     library(readr)
#     library(dplyr)
# 
# Iz Wikipedije bomo prebrali podatke o slovenskih občinah.
# 
#     link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
#     stran <- html_session(link) %>% read_html() # html session poveže na spletno stran
#     tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
#       .[[1]] %>% html_table(dec = ",") #.[[1]] iz seznama poberemo prvi element ; html_table pretvori  tabelo v razpredelnico ; dec pove kja naj se uporabi za decimalno vejico
# 
# Funkciji `html_nodes` smo s parametrom `xpath` povedali, naj išče tabele,
# ki imajo atribut `class` nastavljen na vrednost `wikitable sortable`.
# Nato smo z `.[[1]]` vzeli prvo tako tabelo, ki smo jo nato prebrali s
# funkcijo `html_table`. Parameter `dec = ","` pomeni, da se bo vejica razumela
# kot decimalna vejica. Z ukazom `summary` lahko nato preverimo, kakšni so tipi
# stolpcev v dobljeni razpredelnici.
# 
#     summary(tabela)
# 
# Vidimo, da so nekateri stolpci s številkami tipa `character`. Razlog je v tem,
# se za uporablja pika za ločilo tisočic, poleg tega pa se ponekod pojavi znak `-`
# za manjkajočo vrednost. Za lažje delo bomo pred zamenjavo zamenjali imena stolpcev.
# 
#     colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
#       "ustanovitev", "pokrajina", "regija", "odcepitev")  # določimo imena stolpcev
#     sl <- locale("sl", decimal_mark = ",", grouping_mark = ".") 
#     for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
#       tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl) #parse number bo znake zravn številk npr * ignorirala
#     }
#     
#     for (col in c("obcina","pokrajina","regija","odcepitev")) {
#     Encoding(tabela[[col]]) <- "UTF-8"
#     }
# 
# Nekatere občine pripadajo več pokrajinam. Različne pokrajine si lahko ogledamo
# z `unique(tabela$pokrajina)`. Želeli bomo narediti novo razpredelnico, ki bo
# podala povezavo med občinami in pokrajinami. Najprej bomo kratice nadomestili
# s polnimi imeni pokrajin. # ekvivaletno unique(tabela[["pokrajina"]])
# 
#     tabela$pokrajina <- gsub("Dolenj\\.", "Dolenjska", tabela$pokrajina)
#     tabela$pokrajina <- gsub("Notr\\.", "Notranjska", tabela$pokrajina)
#     tabela$pokrajina <- gsub("Štaj\\.", "Štajerska", tabela$pokrajina)
# 
# Sedaj bomo s funkcijo `strapplyc` poiskali vse neprazne nize črk v stolpcu
# `pokrajina` ter tako dobili seznam z vektorjem pokrajin za vsako občino.
# 
#     pokrajina <- tabela$pokrajina %>% strapplyc("([[:alpha:]]+)") #alpha zajame vse znake
#     obcina <- lapply(1:nrow(tabela), . %>% { rep(tabela$obcina[.], length(pokrajina[[.]])) })
#     pokrajine <- data.frame(obcina = unlist(obcina), pokrajina = unlist(pokrajina))
# 
# Sedaj lahko stolpec `pokrajina` odstranimo iz glavne razpredelnice.
# Da ne bomo dobivali opozoril in da bodo poizvedbe učinkovitejše,
# bomo stolpec `obcina` pretvorili v faktor.
# 
#     tabela$pokrajina <- NULL
#     tabela$obcina <- factor(tabela$obcina)
# 
# Tako lahko delamo poizvedbe o občinah in pokrajinah.
# 
#     tabela %>% inner_join(pokrajine, by = "obcina") %>% filter(pokrajina == "Notranjska") %>%
#       summarise(prebivalstvo = sum(prebivalci)) # prebivalstvo na Notranjskem
#     pokrajine %>% group_by(obcina) %>% summarise(stevilo = n()) %>% filter(stevilo > 1)
# ================================================================@009898=
# 1. podnaloga
# V stolpcu `odcepitev` so zbrani podatki o občinah, od katerih se je vsaka
# občina odcepila. Če se je zgodila ena sama taka odcepitev, je letnica tega
# dogodka zabeležena v stolpcu `ustanovitev`, sicer pa so letnice navedene
# v oklepajih. Sestavi razpredelnico `odcepitve` s stolpci `obcina`, `odcepitev`
# in `leto`, ki bo vsebovala po eno vrstico za vsako odcepitev.
# Pri tem pazi na naslednje:
# 
# * imena občin lahko vsebujejo črke, presledke, in pomišljaj
# * ponekod je `Ljubljana` skrajšana na `Lj.`
# * za občine, ki se niso odcepile, je v stolpcu `odcepitev` vrednost `-`
#   ali prazen niz (pomagaj si s funkcijo `parse_character`);
#   takih občin naj ne bo v razpredlenici `odcepitve`
# * kjer je naštetih več občin, so te ločene z vejico in presledkom
# * letnice odcepitve so podane v obliki `"('yy)"` - pazi na to,
#   kako razširiš letnico
# * če letnice niso podane, uporabi leto iz stolpca `ustanovitev`
# * ponekod so v oklepajih podani komentarji o predhodnih inkarnacijah
#   občine - te odstrani
# * zvezdice označujejo dodatne komentarje - tudi te odstrani
# 
# Prav bodo prišle tudi sledeče funkcije:
# 
# * `ifelse(pogoji, res, nires)`: sprejme logični vektor in še dva vektorja
#   vrednosti iste dolžine ter vrne vektor, katerega elementi ustrezajo
#   prvemu vektorju vrednosti na mestih, kjer je logični vektor `TRUE`,
#   in drugemu vektorju vrednosti sicer
# * `trimws(niz)`: poreže presledke na začetkih in koncih nizov
# * `unlist(seznam)`: seznam vektorjev stakne v vektor
# ========================================================================









































































































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
      check$equal(nrow(odcepitve), 174)
      check$equal(ncol(odcepitve), 3)
      check$equal(length(unique(odcepitve$obcina)), 155)
      check$equal(suppressWarnings(anti_join(odcepitve, tabela,
                                             by = c("odcepitev" = "obcina")) %>% nrow()), 24)
      check$equal(any(is.na(odcepitve$leto)), FALSE)
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
