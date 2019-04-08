# ========================================================================
# Branje in obdelava podatkov 
#
# Podatke najpogosteje dobimo v razpredelnici kot tekstovno datoteko, kjer
# je vsaka vrstica razpredelnice v eni vrstici datoteke, celice pa so med
# seboj ločene z nekim ločilom. Formatu datoteke, kjer je to ločilo
# vejica, pravimo CSV (comma separated values). Ker pa v Evropi vejico
# uporabljamo za ločevanje celega dela števila od decimalk, za ta format
# namesto vejic uporabljamo tudi podpičje - na računalniku s slovenskimi
# nastavitvami bo tako Excel pri shranjevanju v CSV za ločilo uporabil
# prav podpičje.
# 
# Take datoteke bomo brali s funkcijami `read_*` iz knjižnice `readr`.
# Vse delujejo na enak način, razlikujejo se le v privzetih nastavitvah:
# 
# * `read_csv` prebere datoteko CSV, kjer je ločilo med celicami vejica;
# * `read_csv2` prebere datoteko CSV, kjer je ločilo med celicami podpičje;
# * `read.tsv` prebere datoteko, kjer je ločilo med celicami tabulator;
# * `read.delim` prebere datoteko, ločilo pa moramo podati s parametrom
#   `delim`.
# 
# Vse funkcije kot argument sprejmejo ime vhodne datoteke, vrnejo pa
# razpredelnico s prebranimi podatki. Poleg imena datoteke funkcije
# sprejmejo še ostale neobvezne argumente, kot so:
# 
# * `col_names`: `TRUE`, če naj se prva vrstica razume kot glava, torej
#   vsebuje imena stolpcev, sicer pa `FALSE` ali vektor z imeni stolpcev;
# * `na`: vektor nizov, ki naj se interpretirajo kot manjkajoč podatek (`NA`);
# * `comment`: niz, ki predstavlja začetek komentarja - podatki za tem nizom
#   bodo ignorirani;
# * `n_max`: največje število vrstic, ki naj se prebere (če ni podan, se
#   preberejo vse vrstice);
# * `skip`: število vrstic na začetku datoteke, ki naj se izpustijo;
# * `locale`: nastavitev lokalizacije (decimalna vejica, kodiranje znakov itd.)
#   z uporabo funkcije `locale`.
# 
# S spletne učilnice poberite datoteki
# [imena-moski.csv](http://ucilnica.fmf.uni-lj.si/mod/resource/view.php?id=8316)
# in
# [imena-zenske.csv](http://ucilnica.fmf.uni-lj.si/mod/resource/view.php?id=8317),
# ki vsebujeta podatke o pojavnosti moških oziroma ženskih imen v letih od
# 2008 do 2013
# ([vir](http://pxweb.stat.si/pxweb/Database/Dem_soc/05_prebivalstvo/46_Imena_priimki/06_05X10_imena_priimki/06_05X10_imena_priimki.asp)).
# Pri obeh se za ločilo med celicami uporablja podpičje, prav tako imata
# obe na začetku tri vrstice, ki jih bomo izpustili. Pri obeh so imena
# vrstic v prvem stolpcu. Izpuščeni podatki (zaradi premajhne pojavnosti)
# so označeni z znakom `"-"`. Uporabljena kodna tabela je `Windows-1250`.
# Ker bomo želeli tabeli združiti, bomo posebej podali tudi imena
# stolpcev.
# 
#     library(readr)
#     M <- read_csv2("imena-moski.csv", col_names = c("ime", 2008:2013),
#         skip = 4, na = "-", locale = locale(encoding = "Windows-1250"))
#     Z <- read_csv2("imena-zenske.csv", col_names = c("ime", 2008:2013),
#         skip = 4, na = "-", locale = locale(encoding = "Windows-1250"))
# 
# Če želimo neko funkcijo uporabiti na vrsticah ali stolpcih razpredelnice,
# lahko to storimo s pomočjo funkcije `apply`. Če želimo dobiti število
# moških glede na leto, lahko to dobimo z
# 
#     apply(M[-1], 2, sum, na.rm = TRUE)   # [-1] odstrani, prvi stolpec ; na.rm = TRUE IGNORIRA na
# 
# Poglejmo si, kaj smo tukaj naredili. Funkciji `apply` smo najprej podali
# razpredelnico `M` brez prvega stolpca (indeks -1). Nato smo podali
# številko dimenzije, po kateri bi radi delali - če bi želeli seštevati po
# vrsticah, bi tukaj napisali `1`, dimenzija `2` pa ustreza stolpcem
# razpredelnice. Nato smo podali funkcijo, ki naj se uporabi - v našem primeru
# na vsakem stolpcu razpredelnice. Ker pa funkcija `sum` naleti na manjkajoče
# vrednosti, je te potrebno izločiti (sicer bo vsaka vsota enaka `NA`).
# To storimo tako, da funkciji `sum` podamo parameter `na.rm = TRUE`, ki ga
# pa lahko podamo kar kot parameter funkcije `apply`. Podobno bi lahko
# naredili tako - tokrat za ženske:
# 
#     apply(Z[-1], 2, function(x) {sum(x, na.rm = TRUE)})
# 
# Za lažje delo s podatki želimo te imeti v obliki "tidy data". Osnovna ideja
# te oblike je ta, da imamo za vsako "meritev" po eno vrstico v tabeli.
# Tako bomo navadno imeli enega ali več stolpcev, ki identificirajo meritev,
# poleg tega pa še en stolpec (izjemoma lahko tudi več) s samo vrednostjo
# meritve. Če bi v našem primeru npr. dodajali podatke o pogostosti imen v
# prihodnjih letih, bi torej morali dodati nove vrstice, medtem ko naj bi
# stolpci ostali vedno enaki.
# 
# Za pretvorbo v obliko "tidy data" bomo uporabili funkcijo `melt` iz knjižnice
# `reshape2`. S parametrom `ìd.vars` podamo stolpce, ki (delno) identificirajo
# meritve (ti se bodo ohranili), z `measure.vars` podamo stolpce, ki podajajo
# meritve pri različnih pogojih, z `variable.name` podamo ime novega stolpca
# s pogoji, z `value.name` pa podamo ime novega stolpca z vrednostmi.
# Parameter `na.rm = TRUE` določa, naj se izpustijo vrstice, pri katerih je
# vrednost enaka `NA`.
# 
#     library(reshape2)
#     imena.moski <- melt(M, id.vars = "ime", measure.vars = names(M)[-1],
#                         variable.name = "leto", value.name = "stevilo",
#                         na.rm = TRUE)
#     imena.zenske <- melt(Z, id.vars = "ime", measure.vars = names(Z)[-1],
#                          variable.name = "leto", value.name = "stevilo",
#                          na.rm = TRUE)
# 
# Dobili smo razpredelnici s stolpci `ime`, `leto` in `stevilo`, pri čemer so
# vrednosti v stolpcu `leto` kar enake imenom stolpcev v razpredelnicah `M` in
# `Z`. Te bomo tako pretvorili v števila, dodali pa bomo še stolpec za spol.
# 
#     imena.moski$leto <- parse_integer(imena.moski$leto)
#     imena.zenske$leto <- parse_integer(imena.zenske$leto)
#     imena.moski$spol <- factor("moski", levels = c("moski", "zenske"))
#     imena.zenske$spol <- factor("zenske", levels = c("moski", "zenske"))
# 
# Sedaj lahko združimo podatke iz obeh tabel v eno tako, da enostavno združimo
# vrstice obeh tabel. To storimo s funkcijo `rbind`:
# 
#     imena <- rbind(imena.moski, imena.zenske)
# 
# Za delo s podatki v obliki "tidy data" si lahko pomagamo s funkcijami iz
# knjižnice `dplyr`. Tako lahko filtriramo z uporabo funkcije `filter`:
# 
#     library(dplyr)
#     filter(imena, ime == "Bojan")
# 
# Če ne želimo izpisovati vseh stolpcev, lahko želene izberemo s funkcijo
# `select`. Pomagamo si lahko tudi z operatorjem `%>%`: če pišemo
# `x %>% f(y, z, ...)`, je to enako kot `f(x, y, z, ...)`.
# 
#     # vsa ženska imena, ki so se v letu 2013 pojavila največ petkrat
#     imena %>% filter(leto == 2013, spol == "zenske", stevilo <= 5) %>%
#       select(ime, stevilo)
# 
# Podatke lahko tudi grupiramo s funkcijama `group_by` in `summarise`, npr.
# 
#     # prebivalstvo po letih in spolu
#     imena %>% group_by(leto, spol) %>% summarise(prebivalstvo = sum(stevilo))
# 
# S funkcijo `arrange` lahko razvrstimo vrstice razpredelnice po določenem kriteriju.
# 
#     imena %>% filter(leto == 2013) %>% group_by(ime) %>%
#       summarise(stevilo = sum(stevilo)) %>% arrange(desc(stevilo)) %>%    #desc sortirtaj padajoče po številu
#       head(20) # Kaj vrne ta poizvedba? # dobimo 20 najpogostejših imen neglede na spol
# ================================================================@003579=
# 1. podnaloga
# Napišite funkcijo `povprecje`, ki bo za dani vektor imen vrnila
# razpredelnico s stolpci `ime`, `spol` in `povprecje`, ki naj vsebuje
# povprečne pojavnosti imen skozi leta za moške in ženske posebej.
# Pomagajte si z operatorjem `%in%` za preverjanje vsebovanosti v vektorju.
# ========================================================================
library(dplyr)
povprecje <- function(x){
  imena %>% filter(ime %in% x) %>% group_by(ime,spol) %>%
    summarise(povprecje = mean(stevilo,na.rm = TRUE))  #mean bi na štel kot ničle in ne bi bilo pravo povprčje ker bi bilo deljeno z večjim številom
}
# ================================================================@009839=
# 2. podnaloga
# Napišite funkcijo `prebivalstvo`, ki na podlagi podatkov v razpredelnici
# `imena` za dani vektor let izračuna skupno prebivalstvo Slovenije
# (ne glede na spol) za podana leta (privzeta vrednost naj bodo vsa leta,
# za katera imamo podatke, torej od 2008 do 2013).
# Funkcija naj vrne razpredelnico s stolpcema `leto` in `prebivalstvo`.
# ========================================================================
prebivalstvo <- function(leta = 2008:2013){
  imena %>% filter(leto %in% leta) %>% group_by(leto) %>%
    summarise(prebivalstvo = sum(stevilo))
}
# ================================================================@003581=
# 3. podnaloga
# Ustvari razpredelnico `MZ` z enim samim stolpcem `ime`, ki vsebuje tista
# imena iz razpredelnice `imena`, ki se pojavijo tako za moške kot za ženske.
# Pomagaj si s funkcijo `n_distinct`, ki prešteje število različnih vrednosti
# v stolpcu.
# ================================================================
MZ <- imena %>% group_by(ime) %>% summarise(spoli = n_distinct(spol)) %>% 
  filter(spoli == 2) %>% select(ime)








































































































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
      check$equal(povprecje("Slovenko")$povprecje, 11.83333, precision=3)
      check$equal(povprecje("Alojzija Stanislava")$povprecje, 5)
      check$equal(sum(povprecje("Vanja")$povprecje), 2193.833, precision=3)
      check$equal("zenske" %in% povprecje("Jane")$spol, TRUE)
      check$equal("moski" %in% povprecje("Tilka")$spol, FALSE)
      check$equal(sum(povprecje(c("Tine", "Tone", "Jure", "Meta"))$povprecje), 9382.5, precision=3)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(ncol(prebivalstvo()), 2)
      check$equal(nrow(prebivalstvo()), 6)
      check$equal(prebivalstvo("2013")$prebivalstvo, 2001129)
      check$equal(prebivalstvo("2008")$prebivalstvo, 1975633)
      check$equal(sum(prebivalstvo(c(2009, 2011, 2013))$prebivalstvo), 5976905)
    },
    error = function(e) {
      check$error("Testi v izrazu %s sprožijo izjemo %s", deparse(e$call), e$message)
    })
    body[[length(body) + 1]] <- check$parts[[check$part.counter]]
    indices <- c(indices, check$part.counter)
  }
  
  if (check$part()) {
    tryCatch({
      check$equal(ncol(MZ), 1)
      check$equal(nrow(MZ), 99)
      check$equal("Nastja" %in% MZ$ime, TRUE)
      check$equal("Denis" %in% MZ$ime, TRUE)
      check$equal("Tilka" %in% MZ$ime, FALSE)
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
