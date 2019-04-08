##############################################################################
# Delo v konzoli
##############################################################################

#### Kakor v Pythonu lahko tudi v R-ju ukaze vnašamo direktno v konzolo.
#### Znak > nam pove, da konzola pričakuje nov vnos.
# 1. Izračunajte naslednja števila: 12345679 * 9, 7 * (1 + 2 + 3).

#### Če vnos ni zaključen, konzola kaže znak +.
# 2. Napišite 3 + (4 ter pritisnite Enter. Kaj morate storiti, da bo R srečen?
#    S katero tipko lahko prekinete vnos?

#### Po zgodovini vnosov brskamo s puščicama gor in dol.
# 3. Sprehodite se po zgodovini ter v njej poiščite svoje najljubše ukaze.

#### Ker se konzola včasih sesuje in ker je iskanje po zgodovini nepregledno,
#### je našo kodo bolje pisati v datoteko, kot je ta. Iz nje lahko ukaze z
#### ukazom Run ali kombinacijo tipk Ctrl+Enter izvedemo v konzoli.
# 4. Poskusite izvesti spodnje ukaze
9 * 98765432
(1 + 2 + 3) * (3 + 4)
sin(pi / 4)

#### Za pomoč o posamezni funkciji uporabite ukaz ?ime.funkcije
# 5. Kaj počne funkcija choose?

##############################################################################
# Osnovna aritmetika
##############################################################################

#### Vrednosti izrazov lahko shranimo v spremenljivke.
# 6. Kaj je po spodnjem izvajanju shranjeno v spremenljivki z?
#    Razmislite o odgovoru preden ga izračunate.
x <- 5
y <- 6
x <- x + y
z <- x + y

#### V R je vgrajenih cel kup matematičnih funkcij: trigonometrijske,
#### kombinatorične, zaokroževalne, verjetnostne in še in še. Spoznavali jih
#### bomo spotoma.
a <- sqrt(2) / 2
fi <- asin(a)
4 * fi
sin(fi)^2 + cos(fi)^2
sin(fi)^2 + cos(fi)^2 - 1

# 7. V čem se ukaz atan2(y, x) razlikuje od ukaza atan(y / x)?

# 8. Kaj delajo zaokroževalne funkcije round, ceiling, floor, trunc, signif?
round(13.2)
round(13.7)
ceiling(13.2)
ceiling(13.7)
floor(13.2)
floor(13.7)

# 9. Kako se zaokrožujejo polovice?
round(12.5)
round(13.5)
round(14.5)
round(15.5)

# 10. Kako določimo število decimalk pri zaokrožitvi?
# Zaokrožite število 1 / pi na 3 decimalke.

##############################################################################
# Nerealna števila
##############################################################################

#### R pozna tudi kompleksna števila. Pišemo jih v obliki x+yi, kjer pred i ni
#### presledka, temveč le število.
1 + 3i
-3 - 4i
2i
sqrt(3i)
exp(pi * 1i)

#### Števila pretvorimo v kompleksna z ukazom as.complex ali s tem, da jih
#### uporabimo v istem izrazu kot kompleksna
as.complex(2)
2 + 0i
2 * 1i
3 + (2 + 3i) - (4 + 3i)
Re(Conj((2 - 3i) * (7 + 5i)))
Mod((2 - 3i) * (7 + 5i)) # Kaj dela funkcija Mod?

#### R pozna tudi vrednost Inf, ki predstavlja neskončnost.
3 + Inf
1 / 0
-3 * Inf
Inf + Inf
2^10000 # Inf dobimo tudi v primeru prevelikih števil

#### Za števila, ki niso definirana, uporabimo vrednost NaN.
Inf - Inf
0 * Inf
0 / 0
(NaN + 3) - 20 * 6 #  Število NaN "okuži" celoten izraz, v katerem nastopa
NaN * 0 # tudi če množimo z 0, se ga ne znebimo.

#### Obstaja tudi vrednost NA, ki predstavlja manjkajoče podatke in jo bomo
#### spoznali kasneje.

# 11. Kaj vrne sledeči ukaz? Kako bi izračunali kompleksni koren iz -3?
sqrt(-3)

##############################################################################
# Rešitve
##############################################################################

# 2.  Vnosu je treba dodati še zaklepaj ali pa pritisniti Escape.
# 5.  choose(n, r) izračuna binomski koeficient "n nad r"
# 6.  V spremenljivki z je shranjena vrednost 17
# 7.  Ukaz atan2(y, x) izračuna kot smernega vektorja točke (x, y).
#     V primeru, ko je točka v 1. ali 2. kvadrantu, je rezultat enak
#     rezultatu ukaza atan(y / x). Če pa je točka v 3. ali 4. kvadrantu,
#     vrne atan2(y, x) rezultat, povečan za pi.
# 8.  Round zaokroža na najbližje število, prav tako signif, le da prvi na
#     dano število mest za decimalno vejico, drugi pa število mest na začetku
#     števila. Funkcija ceiling zaokroža navzgor, floor navzdol, trunc pa
#     odbije decimalke. Zadnji dve se razlikujeta pri negativnih številih.
# 9.  Polovice se vedno zaokrožujejo na sodo število.
# 10. round(1 / pi, digits = 3)
# 11. Ukaz vrne NaN. Če bi želeli kompleksni koren iz -3, moramo -3 pretvoriti
#     v kompleksno število z sqrt(as.complex(-3))


