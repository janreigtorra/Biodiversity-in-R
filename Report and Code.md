Legacy forest structure increases bird diversity and abundance in aging
young forests
================
4/24/2021

# Introducció i aspectes generals

L’**objectiu** d’aquests estudi es estudiar la relació que hi ha entre
l’estructura forestal regenerant els boscos i la composició de la
comunitat d’aus reproductores. Per tant, consisteix en analitzar si la
comunitat d’ocells canvia amb la successió del bosc.

S’observa que els canvis de l’edat i l’estructura dels boscos (ja sigui
per l’abandonament agrícola o la disminució de la collita de fusta) han
alterat les poblacions d’ocells. I que a mesura que augmenta la densitat
dels boscos augmenta la mortalitat dels arbres més febles. Això deriva
en que les comunitats d’ocells també disminueixin i es simplifiquin.
D’aquesta manera **podem pensar que la regeneració dels boscos** (ja
sigui recol·lectant per tal d’augmentar la diversitat de plantes)
**portaria a una major diversitat d’ocells**.

L’estudi del que s’han obtingut aquestes dades es va realitzar al bosc
de Yale-Myers situat al nord-est de Connecticut, Estats Units.

A continuació explicarem a grans trets com es va realitzar el **disseny
experimental**:

-   Pel que fa els **ocells**, es van dur a terme prospeccions d’aus a
    36 *stands* en tot el bosc. Cada punt es visitava 4 cops al dia
    (durant 3 mesos) i es comptaven tots els ocells observats o
    escoltats en un radi de 50 metres durant 12 mintuts.

-   Pel que fa la **vegetació**, a nivell general es van classificar i
    registrar les plantes en diferents mesures. Cada *stand* de
    recollida d’ocells estava classificat en funció de l’etapa del
    desenvolupament de les plantes que hi havia aquella zona.

    Les quatre etapes són: EI (stand d’iniciació primerenca), LI (stand
    d’iniciació tardana), ESE (stands d’exclusió de tija primerenca) i M
    (stands no gestionts).

Un tipus d’arbre especialment sensibles a la sobre densitat són els
arbustos, aquest tipus d’arbres, a més, són crítics i necessaris per a
la supervivència de moltes espècies d’ocells. És per això que volem
comprovar, entre altres coses, **si als stands no gestionats,** aquells
més susceptibles a la sobre densitat vegetal, **les espècies d’ocells
que aniden en arbustos són menys abundants.**

# Presentació de la base de dades i primers resultats

La base de dades que utilitzarem en aquest estudi s’ha construit a
partir de la base de dades que s’utilitza en el article de referència.
La base de dades que hem constuit té un total de **36 files o tuples i
71 columnes o atributs**.

Aquesta conté la informació del nombre d’aus observades per espècie a
cada stand. Cada fila representa un *stand* del bosc i cada columna una
tipologia d’espècie d’au.

Visualització de les primeres tuples de la matriu de dades:

    ## # A tibble: 6 × 71
    ##   Stand  AMCR  AMGO  AMKE  AMRE  AMRO  BADO  BAOR  BAWW  BBCU  BCCH  BHCO  BHVI
    ##   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 BLHA     NA    NA    NA     2    NA    NA     2     3    NA    NA    NA     4
    ## 2 BU2      NA    NA    NA    NA    NA    NA     1     2    NA     2    NA     1
    ## 3 BU5      NA     3    NA    NA    NA    NA    NA     2    NA     4    NA     3
    ## 4 CARO      1    NA    NA    NA    NA    NA     7    NA    NA    NA    NA    NA
    ## 5 CLFO     NA    NA    NA    NA    NA    NA    NA     5    NA     3    NA    NA
    ## 6 CMU1      1    NA    NA    NA    NA    NA    NA     3    NA     2    NA     2
    ## # … with 58 more variables: BLBW <dbl>, BLJA <dbl>, BLVU <dbl>, BRCR <dbl>,
    ## #   BTBW <dbl>, BTNW <dbl>, BWHA <dbl>, BWWA <dbl>, CANG <dbl>, CAWA <dbl>,
    ## #   CEDW <dbl>, CHSP <dbl>, COYE <dbl>, CSWA <dbl>, DOWO <dbl>, EAKI <dbl>,
    ## #   EAPH <dbl>, EATO <dbl>, EAWP <dbl>, GCFL <dbl>, GCKI <dbl>, GRCA <dbl>,
    ## #   HAWO <dbl>, HETH <dbl>, INBU <dbl>, LEFL <dbl>, MAWA <dbl>, MODO <dbl>,
    ## #   NOCA <dbl>, NOFL <dbl>, NOHA <dbl>, NOMO <dbl>, NOWA <dbl>, OVEN <dbl>,
    ## #   PIWA <dbl>, PIWO <dbl>, PRAW <dbl>, RBGR <dbl>, RBNU <dbl>, RBWO <dbl>, …

Descriptiva de la base de dades i primers resultats:

``` r
X[is.na(X)] <- 0
S <- ncol(X[,2:ncol(X)]) # Número de especies 

y <- colSums(X[,2:ncol(X)]) # Abundancia de cada especie 
N <- sum(y) # Abundancia total

av <- N/S
```

S’han trobat **70 especies**. L’**abundancia total** (total d’individus
de totes les espècies) és de **2329 exemplars** i hi ha en promig
**33.2714 individus per espècie**.

Si ordenem les espècies segons la seva abundancia observem que l’especie
més abundant és EATO (*Eastern Towhee*) amb **164 exemplars**.

# Classificacions

## Presentació de les fases de desenvolupament de les plantes

Com s’ha comentat a la introducció s’han dividit els *stands* en funció
de les fases de desenvolupament de les plantes. Les quatre etapes de
desenvolupament són:

-   EI (stand d’iniciació primerenca),

-   LI (stand d’iniciació tardana),

-   ESE (stands d’exclusió de tija primerenca) i,

-   M (stands no gestionts).

Introduïm aquesta informació a la matriu de dades amb l’objectiu
d’obtenir un vector amb les dades relatives a l’abundància de cada
espècie per cadascuna de les fases de creixement.

``` r
sumy <- read.csv("Summaries.csv", header = TRUE, sep = ";")

stand_etapa <- sumy[, c(3,1)] 
names(stand_etapa)[1] <- "Etapa Des"

dades2 <- cbind (stand_etapa, X)

y.EI <- dades2[dades2$`Etapa Des` == "EI",]
y.LI <- dades2[dades2$`Etapa Des` == "LI",]
y.ESE <- dades2[dades2$`Etapa Des` == "ESE",]
y.M <- dades2[dades2$`Etapa Des` == "M",]
```

**a. EI: Stands d’iniciació primerenca.** En aquests *stands* el dosser
del bosc roman obert en gran mesura, mentre que la coberta del terreny,
incloses les plàntules d’arbres, es regenera: augmenta el nombre de
planters, així com la seva alçada i el seu diàmetre mitjà a l’alçada del
pit.

``` r
y.EI <- colSums(y.EI[4:ncol(y.EI)])
y.EI <- y.EI[!y.EI==0]
S.EI <- length (y.EI) # Número d'espècies 
N.EI <- sum(y.EI)     # Abundància total
yr.EI <- sort(y.EI/N.EI, decreasing = T)
```

Als stands d’iniciació primenenca s’han observat un total de **545
individus** d’un total de **57 espècies** **diferents.** Per tant,
l’**abundància mitjana és 9.561.**

A més, l**’espècie més abundant és l’EATO (*Eastern Towhee)*** que és
una espècie recolectora en el terra pertanyent al gremi de les aus que
aniden en arbusts*.*

**b. LI: Stands d’iniciació tardana.** En aquests *stands* el dosser ha
començat a tancar-se i comença a excloure algunes herbes i arbustos.

``` r
y.LI <- colSums(y.LI[4:ncol(y.LI)])
y.LI <- y.LI[!y.LI==0]
S.LI <- length (y.LI) # Número d'espècies 
N.LI <- sum(y.LI)     # Abundància total
yr.LI <- sort(y.LI/N.LI, decreasing = T)
```

Als stands d’iniciació tardana s’han observat un total de **712
individus** d’un total de **61 espècies diferents**. Per tant,
l’**abundància mitjana és 11.672.**

A més, l’**espècie més abundant és l’CSWA** (*Chestnut-sided Warbler)*
que és una espècie recolectora de follatge pertanyent al gremi de les
aus que aniden als arbusts*.*

**c. ESE: Stands d’exclusió de tija primerenca.** En aquests *stands* la
capçada del peu regenerador s’ha tancat completament, la densitat de la
tija disminueix a mesura que els planters competeixen per la llum solar
i augmenten l’alçada de les tiges restants.

``` r
y.ESE <- colSums(y.ESE[4:ncol(y.ESE)])
y.ESE <- y.ESE[!y.ESE==0]
S.ESE <- length (y.ESE) # Número d'espècies 
N.ESE <- sum(y.ESE)     # Abundància total
yr.ESE <- sort(y.ESE/N.ESE, decreasing = T)
```

Als stands d’exclusió de tija primerenca s’han observat un total de
**639 individus** d’un total de **55 espècies diferents**. Per tant,
l’**abundància mitjana és 11.618**.

A més, l’**espècie més abundant és VEER** (*Veery)* que és una espècie
recolectora en la terra que pertany al gremi de les aus que aniden al
terra dels boscos.

**d. M: Stands no gestionats.** En aquests *stands* els boscos no estàn
gestionats i, per tant, és un bosc de creixament centanari.

``` r
y.M <- colSums(y.M[4:ncol(y.M)])
y.M <- y.M[!y.M==0]
S.M <- length (y.M) # Número d'espècies 
N.M <- sum(y.M)     # Abundància total
yr.M <- sort(y.M/N.M, decreasing = T)
```

Als stands no gestionats s’han observat un total de **433 individus**
d’un total de **49 espècies diferents**. Per tant, l’**abundància
mitjana és 8.837.**

A més, l’**espècie més abundant és OVEN** (*Ovenbird)* que és una
espècie recolectora en la terra que pertany al gremi de les aus que
aniden al terra dels boscos.

## Presentació dels gremis

L’analisi també classifica les espècies d’ocells en funció de gremis.
Trobem una doble classificació (dos gremis): el **gremi nidícola** (que
classifica les aus en funció del lloc on possen el seu niu) i
l’**alimentari** (que classifica les espècies d’aus en funció de la seva
alimentació).

``` r
InfoBIRDS <- read_excel("InfoBIRDS.xlsx")
InfoBIRDS <- InfoBIRDS[c(1, 4:5)]
```

### Gremi nidícola

Es classifiquen les diferents espècies d’aus en funció del lloc on fan
el niu en els vuit grups o categories següents:

| Cavitat     | Penya-segat              | Bosc de terra | Pantà de terra |
|-------------|--------------------------|---------------|----------------|
| Terra obert | Voladissos (*overhangs*) | Arbust        | Arbre          |

Primer de tot, observem que les espècies que aniden als arbres estan
classificades en dues categories: “Tree” i “tree”. Entenem que això es
un error ortogràfic i les ajuntem en una única categoria, “Tree”

``` r
InfoBIRDS$`nesting guild` [InfoBIRDS$`nesting guild` == "tree"] <- "Tree"
```

A continuació peresentem en una taula i un diagrama de barres
l’abundància (en número de espècies) de cada gremi nidícola sobre el
conjunt de la comunitat d’aus.

``` r
sort(table(InfoBIRDS$`nesting guild`),decreasing  = T)
```

    ## 
    ##            Tree           shrub          cavity Ground - forest  ground - marsh 
    ##              30              14              12               8               2 
    ##   ground - open           cliff       overhangs 
    ##               2               1               1

``` r
barplot(sort(table(InfoBIRDS$`nesting guild`),decreasing  = T), col = "palegreen2", 
        main = "Número d'espècies per gremi nidícola", cex.names = 0.7, las = 2)
```

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

Tenim un gran nombre d’espècies que aniden als arbres, seguit de lluny
del nombre de espècies que aniden en arbustos, cavitats o al terra. A la
resta de categories de anidació, el nombre d’espècies és molt baix.

Pel que fa l’abundància d’espècies de cada grup, en nombre de exemplars:

``` r
X2 <- data.frame(d = names(colSums(X[,2:71])), abundancia = as.vector(colSums(X[,2:71])))

names(X2) <- c("Bird species abbeviations", "abundancia")
dades3 <- merge(InfoBIRDS, X2)

abun_nest <- tapply(dades3$abundancia, dades3$`nesting guild`, sum)
sort(abun_nest, decreasing =T)
```

    ##            Tree           shrub Ground - forest          cavity   ground - open 
    ##             821             799             396             272              24 
    ##       overhangs  ground - marsh           cliff 
    ##               9               7               1

``` r
barplot(sort(abun_nest, decreasing =T), col = "skyblue", 
        main = "Abundància d'aus per gremi nidícola", cex.names = 0.7, las = 2)
```

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

Si representem les categoríes d’anidació en funció del nombre
d’exemplars d’au i no d’espècies, podem observar que hem observat moltes
aus que fan al niu als arbres i als arbustos, això ens indica que les
espècies que aniden als arbustos són més abundants que les que aniden
als arbres, en moltes menys espècies hem visualitzat un nombre
d’exemplars similars. Una mica més lluny, en termes d’abundància, tenim
les aus que aniden al terra o en alguna cavitat, l’abundància relativa
de la resta de categories continua sent insignificant.

### Gremi alimentari

Les diferents espècies d’aus es classifiquen segons la seva alimentació
en els vuit grups o categories següents:

| Recolectores de fullatge | Recolectores de terra        | Cercadores d’escorça | Flycatching |
|--------------------------|------------------------------|----------------------|-------------|
| Elevadores (*soaring*)   | Busseig aeri (*aerial dive*) | Espiga de fullatge   | *Havering*  |

Novament presentarem una taula i un diagrama de barres de l’abundància
(en funció del nombre d’espècies i del nombre d’exemplars observats) de
cada de cada gremi alimentari sobre el conjunt de la comunitat.

``` r
sort(table(InfoBIRDS$`foraging guild`),decreasing  = T)
```

    ## 
    ## foliage gleaner  ground forager    bark forager     flycatching         soaring 
    ##              27              21               9               5               4 
    ##     aerial dive  foliiage glean        havering 
    ##               2               1               1

``` r
barplot(sort(table(InfoBIRDS$`foraging guild`),decreasing  = T), col = "palegreen2", 
        main = "Número d'espècies per gremi alimentari", cex.names = 0.7, las = 2)
```

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-13-1.png" title="center" alt="center"  />

Tenim un gran nombre d’espècies que s’alimenten de fulles i que
recolecten al terra, el nombre d’espècies que pertany a la resta de
categories és molt inferior.

Pel que fa l’abundància d’espècies de cada grup:

``` r
X2 <- data.frame(d = names(colSums(X[,2:71])), abundancia = as.vector(colSums(X[,2:71])))

names(X2) <- c("Bird species abbeviations", "abundancia")
dades3 <- merge(InfoBIRDS, X2)

abun_nest <- tapply(dades3$abundancia, dades3$`foraging guild`, sum)
sort(abun_nest, decreasing =T)
```

    ## foliage gleaner  ground forager    bark forager     flycatching  foliiage glean 
    ##            1064             883             216             103              51 
    ##         soaring     aerial dive        havering 
    ##               7               4               1

``` r
barplot(sort(abun_nest, decreasing =T), col = "skyblue", 
        main = "Abundància d'aus per gremi alimentari", cex.names = 0.7, las = 2)
```

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

En aquest cas observem que la distribució del nombre d’espècies entre
els diferents grups alimentaris és molt semblant a la distribució de les
diferents aus observades entre els diferents grups alimentaris. Tenim un
gran nombre d’aus corresponent al grup d’espècies que s’alimenten de
fulles, seguit de les aus recolectores de terra, que en aquest cas tenen
una abundància lleugerament major.

### Gremis en funció de l’etapa del desenvolupament

**Stands d’iniciació primerenca (EI)**

``` r
# EI
y.EI <- dades2[dades2$`Etapa Des` == "EI",]
y.EI <- colSums(y.EI[4:ncol(y.EI)])
y.EI2 <- cbind(as.data.frame(y.EI), InfoBIRDS)
N.EI <- sum(y.EI2$y.EI)     # Abundància total
y.EI2$yr <- y.EI2$y.EI/N.EI

par(mfrow=c(1,2))
y.EIn <- sort(tapply(y.EI2$yr, y.EI2$`nesting guild`, FUN=sum), decreasing=T)

barplot(height=y.EIn, names=row.names(y.EIn), ylim= c(0,0.5), las=2,
        ylab= "Abundancia", main= "Abundàncies segons gremi nidícola", 
        col= "lavender",  cex.main=0.75, cex.lab=0.8)

y.EIa <- sort(tapply(y.EI2$yr, y.EI2$`foraging guild`, FUN=sum), decreasing=T)

barplot(height=y.EIa, names=row.names(y.EIa), ylim= c(0,0.5), las=2,
        ylab= "Abundancia", main= "Abundàncies segons gremi alimentari", 
        col= "lavender",  cex.main=0.75, cex.lab=0.8)
```

![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Observem que hi ha dos gremis nidícoles i dos gremis alimentaris molt
abundants en els stands d’iniciació primerenca. El gremi d’espècies més
abundants són aquelles que **aniden als arbustos**, seguides de les que
ho fan als **arbres**. Mentre que el gremi de les espècies més abundants
segons l’alimentació són les **recolectores de follatge** seuides de les
**recolectores en el terra**.

| Gremi                    | S: Número de espècies | N: abundància total |
|--------------------------|-----------------------|---------------------|
| Aniden als arbustos      | S= 11                 | N= 240              |
| Aniden als arbres        | S= 25                 | N= 187              |
| Recolectores de follatge | S= 24                 | N= 270              |
| Recolectores de terra    | S= 17                 | N= 181              |

**Stands d’iniciació tardana LI**

``` r
# LI
y.LI <- dades2[dades2$`Etapa Des` == "LI",]
y.LI <- colSums(y.LI[4:ncol(y.LI)])
y.LI2 <- cbind(as.data.frame(y.LI), InfoBIRDS)
y.LI2 <- y.LI2[,c(1, 3:4)]
N.LI <- sum(y.LI2$y.LI)     # Abundància total
y.LI2$yr <- y.LI2$y.LI/N.LI

par(mfrow=c(1,2))

y.LIn <- sort(tapply(y.LI2$yr, y.LI2$`nesting guild`, FUN=sum), decreasing=T)

barplot(height=y.LIn, names=row.names(y.LIn), ylim= c(0,0.5), las=2,
        ylab= "Abundancia", main= "Abundàncies segons gremi nidícola", 
        col= "lavender", cex.main=0.75, cex.lab=0.8)

y.LIa <- sort(tapply(y.LI2$yr, y.LI2$`foraging guild`, FUN=sum), decreasing=T)

barplot(height=y.LIa, names=row.names(y.LIa), ylim= c(0,0.5), las=2,
        ylab= "Abundancia", main= "Abundàncies segons gremi alimentari", 
        col= "lavender", cex.main=0.75, cex.lab=0.8)
```

![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Observem que hi ha dos gremis alimentaris i dos gremis nidícoles molt
abundants en els stands d’iniciació tardana. El gremi d’ocells que
**recolecten fulles** i les que **recolecten al terra** són els més
abundans en aquest tipus d’stands tenint en compte la seva alimentació.
Mentre que si observem els gremis nidícoles les més abundants són les
espècies que **aniden als arbustos** seguides de les espècies que
**aniden als arbres**.

Aquests resultats són molt semblants als que hem obtingut a la fase de
desenvolupament EI (inicialització primerenca).

| Gremi                    | S: Número de espècies | N: abundància total |
|--------------------------|-----------------------|---------------------|
| Aniden als arbustos      | S= 13                 | N= 301              |
| Aniden als arbres        | S= 25                 | N= 221              |
| Recolectores de follatge | S= 26                 | N= 332              |
| Recolectores de terra    | S= 19                 | N= 261              |

**Stands d’exclusió de tija primerenca ESE**

``` r
# ESE
y.ESE <- dades2[dades2$`Etapa Des` == "ESE",]
y.ESE <- colSums(y.ESE[4:ncol(y.ESE)])
y.ESE2 <- cbind(as.data.frame(y.ESE), InfoBIRDS)
y.ESE2 <- y.ESE2[,c(1, 3:4)]
N.ESE <- sum(y.ESE2$y.ESE)     # Abundància total
y.ESE2$yr <- y.ESE2$y.ESE/N.ESE

par(mfrow=c(1,2))

y.ESEn <- sort(tapply(y.ESE2$yr, y.ESE2$`nesting guild`, FUN=sum), decreasing=T)

barplot(height=y.ESEn, names=row.names(y.ESEn), ylim= c(0,0.5), las=2,
        ylab= "Abundancia", main= "Abundàncies segons gremi nidícola", 
        col= "lavender",  cex.main=0.75, cex.lab=0.8)

y.ESEa <- sort(tapply(y.ESE2$yr, y.ESE2$`foraging guild`, FUN=sum), decreasing=T)

barplot(height=y.ESEa, names=row.names(y.ESEa), ylim= c(0,0.5), las=2,
        ylab= "Abundancia", main= "Abundàncies segons gremi alimentari", 
        col= "lavender",  cex.main=0.75, cex.lab=0.8)
```

![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Observem que hi ha dos gremis alimentaris molt abundants, aquest són: el
gremi del ocells que **recolecten fulles** i les que **recolecten al
terra**. Mentre que si observem els gremis nidícoles podríem considerar
que hi ha 3 abundants: les especies que fan el seu niu als **arbres,**
els que els fan als **arbustos** i els que els fan al **terra del
bosc,** tot i que el primer és molt més abundant que els altres dos.

| Gremi                    | S: Número de espècies | N: abundància total |
|--------------------------|-----------------------|---------------------|
| Aniden als arbres        | S= 25                 | N= 264              |
| Aniden als arbustos      | S= 12                 | N= 155              |
| Aniden al terra del bosc | S= 7                  | N= 141              |
| Recolecten fulles        | S= 23                 | N= 286              |
| Recolecten al terra      | S= 18                 | N= 261              |

**Stands de bosc no gestionat M**

``` r
# M
y.M <- dades2[dades2$`Etapa Des` == "M",]
y.M <- colSums(y.M[4:ncol(y.M)])
y.M2 <- cbind(as.data.frame(y.M), InfoBIRDS)
y.M2 <- y.M2[,c(1, 3:4)]
N.M <- sum(y.M2$y.M)     # Abundància total
y.M2$yr <- y.M2$y.M/N.M

par(mfrow=c(1,2))

y.Mn <- sort(tapply(y.M2$yr, y.M2$`nesting guild`, FUN=sum), decreasing=T)

barplot(height=y.Mn, names=row.names(y.Mn), ylim= c(0,0.5), las=2,
        ylab= "Abundancia", main= "Abundàncies segons gremi nidícola", 
        col= "lavender",  cex.main=0.75, cex.lab=0.8)

y.Ma <- sort(tapply(y.M2$yr, y.M2$`foraging guild`, FUN=sum), decreasing=T)

barplot(height=y.Ma, names=row.names(y.Ma), ylim= c(0,0.5), las=3,
        ylab= "Abundancia", main= "Abundàncies segons gremi alimentari", 
        col= "lavender",  cex.main=0.75, cex.lab=0.8)
```

![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Novament observem que hi ha dos gremis alimentaris molt abundants, però
ara les espècies **recolectores de la terra** són lleugerament més
abundants que les espècies **recolectores de fullatge**. Si observem els
gremis nidícoles podríem considerar que hi ha 3 gremis abundants: les
especies que fan el seu niu als **arbres,** els que els fan al **terra
del bosc** i per últim, els que els fan als **arbusts**.

| Gremi                    | S: Número de espècies | N: abundància total |
|--------------------------|-----------------------|---------------------|
| Aniden als arbres        | S= 25                 | N= 264              |
| Aniden al terra del bosc | S= 7                  | N= 155              |
| Aniden als arbustos      | S= 12                 | N= 141              |
| Recolecten al terra      | S= 18                 | N= 286              |
| Recolecten a les fulles  | S= 23                 | N= 261              |

En aquest apartat hem observat que **a les regions de bosc regenerades
hi ha una major abundància de les espècies que aniden en arbustos, que
aniden en arbres i recolectores de fullatge**, i que aquesta
**abundància disminueix amb les fases de desenvolupament de la
vegetació** a la vegada que **augmenta l’abundància d’ocells nidificants
al terra**.

Això dona un punt a favor de la teoria que diu que a mesura que augmenta
la densitat de vegetació, la vegetació més feble (arbust) és redueix i
conseqüentment es redueixen els exemplars de les espècies d’ocells que
aniden en aquest tipus de vegetació.

# Anàlisi descriptiu

## Diagrama Rank-abundància

Calculem **l’abundancia relativa de les especies d’aus** i representem
al diagrama de rank-abundància de tota la comunitat (tots els *stands*
junts).

``` r
rel.y <- index/N # Abundancia relativa de les especies

plot(1:S, rel.y, "o", pch= 20, xlab= "especie", ylab="f(x)", 
     main= "Rank-abundance plot", col = "royalblue1")
```

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

A la vista del gràfic, no s’observa que hi hagi una única espècie molt
més abundant que la resta, la especie més abundant, com hem dit abans,
és la EATO. La abundància relativa d’aquesta espècie correspon a la
primera observació representada en el gràfic.

S’observa que hi ha moltes espècies amb freqüències realtives molt
baixes, quasi arribant al 0, això és degut a que s’han trobat molt pocs
exemplars d’aquelles espècies i tenim un nombre molt elevat d’espècies.

Si representem l’abundància relativa en escala logarítmica, la pendent
de la funció varia i la freqüència relativa pren valors negatius. Però
fer-ho ens permetrà observar si la caiguda és lineal, fet que ens
permetría ajustar les dades amb una recta.

``` r
plot(1:S, log(rel.y), "o", pch=20, xlab= "especie", ylab="log(f(x))", 
     main= "rank-abundance plot logaritmic", col = "royalblue1")
```

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

Com més suau és la caiguda més diversa és la comunitat, per tant, podem
observar que la comunitat és força diversa ja que la caiguda és bastant
suau.

També s’observa que podrem l’abundància podrá ajustar-se correctament
amb una recta, però això ho veurem més endevant.

### Estratificat per Etapa del desenvolupament

En aquest apartat representarem el diagrama de rank-abundància
estratificat en funció de l’etapa de desenvolupament de les plantes:

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-25-1.png" style="display: block; margin: auto;" />

A simple vista no s’observen moltes diferències. És millor utilitzar
l’escala logarítmica.

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-26-1.png" style="display: block; margin: auto;" />

A la vista del diagrama de rank-abundància no s’observen moltes
diferències entre les etapes del desenvolupament i, per tant, a priori
**no podem afirmar que hi hagi menys diversitat en els boscos no
gestionats**.

## Índex de Shannon

Aquest ens permetrà tenir una mesura objectiva sobre la diversitat.
L’índex de Shannon pren la següent expressió:

$$H' = - \sum_{i=1}^{S} p_i \log{p_i}$$

Sabem que el índex de Shannon normalment es troba entre 1.5 i 3.5. El
valor màxim del índex de Shannon és $H_\text{`}=ln(S)$ i l’obtindíem
quan totes les espècies de la nostra mostra fossin igual d’abundants, és
a dir, tots els individus (ocells) es repartissin a parts iguals entre
totes les espècies. Mentre que el valor mínim del índex de Shannon és 0
i l’observaríem si tots els individus (ocells) fossin d’una mateixa
espècie. És a dir, **a major índex de Shannon major diversitat
d’espècies**.

``` r
library(vegan)
Hp <- diversity(y, index = "shannon")

Hp_max <- log(S)
```

**El Índex de Shannon calculat per aquesta mostra és de 3.6364278** molt
proper al valor màxim, per tant, podem dir que hi ha una gran diversitat
d’espècies.

A continuació calcularem l’interval de confiança per aquest índex:

$V(H') \approx \frac{\sum_{i=1}^{S}\theta _i[ln(\theta_i)]^2-(\sum_{i=1}^{S}\theta_i ln(\theta_i))^2}{N}+\frac{S-1}{2N^2}$

``` r
deno1 <- sum(rel.y*(log(rel.y))^2)
deno2 <- sum((rel.y*log(rel.y)))^2

var.Hp <- ((deno1-deno2)/N)+((S-1)/(2*N^2))
```

Assumint normalitat de les dades podem calcular l’**interval de
confiança** com:

$$IC = \left [ H'\pm z_{\alpha /2} \sqrt{V(H')} \right ]$$

``` r
IC.inf <- Hp - sqrt(var.Hp)*qnorm(0.025, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
IC.sup <- Hp + sqrt(var.Hp)*qnorm(0.025, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
IC <- c(IC.inf, IC.sup)
```

L’interval de confiança del 95% per al vertader valor del índex de
Shannon és **IC= \[3.5991319, 3.6737238\]. El valor estimat de H’ té un
biaix negatiu pel que podem dir que tendeix a subestimar el vertades
valor poblacional.**

``` r
biaix <- -(S-1)/N^2
```

El biaix és -1.2720674^{-5}.

A més a més, podem calcular la mesura d’uniformitat d’espècies de
Shannon que prén la següent expressió:

$$J'=\frac{H'}{H_{max}}$$

``` r
(J = Hp/Hp_max)
```

    ## [1] 0.8559331

Podem concloure que hi ha **uniformitat d’espècies**.

### Estratificat per etapa del desenvolupament

Si fem aquests càlculs per les diferents fases de desenvolupament de la
vegetació obtindriem els resultats següents:

| Stand | Índex de Shannon | Uniformitat d’espècies | IC(95%) Índex de Shannon |
|-------|------------------|------------------------|--------------------------|
| EI    | H’= 3.533        | J’= 0.874              | IC= \[3.453 , 3.614 \]   |
| LI    | H’= 3.518        | J’= 0.856              | IC= \[3.446 , 3.589 \]   |
| ESE   | H’= 3.512        | J’= 0.876              | IC= \[3.442 , 3.583 \]   |
| M     | H’= 3.41         | J’= 0.876              | IC= \[3.324 , 3.497 \]   |

A simple vista no sembla que l’index de Shannon sigui molt diferents
entre una fase de desenvolupament i un altre. Podem veure que els
intervals de confiança entre les fases están superposats.

## Índex de Simpson

``` r
(D1 <- diversity(y,index="simpson"))  # 1-D
```

    ## [1] 0.9646308

``` r
(D2 <- diversity(y,index="invsimpson"))   # 1/D
```

    ## [1] 28.2732

``` r
(D = 1- D1) 
```

    ## [1] 0.03536919

La probabilitat de que a l’escollir dues aus a l’atzar (en un stand
tipus) i que siguin de la mateixa espècie és del 3.54%.

### Estratificat per Etapa del desenvolupament

Si novament fem el mateix estratificant per etapes de desenvolupament:

| Stand | Índex de Simpson | Uniformitat d’espècies |
|-------|------------------|------------------------|
| EI    | 1/D’= 0.959      | J’= 0.017              |
| LI    | 1/D’= 0.958      | J’= 0.016              |
| ESE   | 1/D’= 0.96       | J’= 0.017              |
| M     | 1/D’= 0.955      | J’= 0.019              |

## Diversity t-test entre EDP

El diversity t-test ens permet comparar entre les mesures de diversitat
calculades per als stand de les diferents fases de desenvolupament.

``` r
diversity.test <- function (H1, H2, var.H1, var.H2, N1, N2) {

T.test <- abs(H1-H2)/sqrt(var.H1+var.H2)
v <- (var.H1 + var.H2)^2 / ( var.H1^2/N1 + var.H2^2/ N2) 
pvalor <- pt(T.test, df= v, lower.tail = FALSE)

return(c(T.test, pvalor))
}

alpha.bonferroni <- 0.05/4

diversity.test (H.EI, H.LI, var.HEI, var.HLI, N.EI, N.LI)[2] < alpha.bonferroni
diversity.test (H.EI, H.ESE, var.HEI, var.HESE, N.EI, N.ESE)[2] < alpha.bonferroni
diversity.test (H.EI, H.M, var.HM, var.HM, N.EI, N.M)[2]<alpha.bonferroni

diversity.test (H.LI, H.ESE, var.HLI, var.HESE, N.LI, N.ESE)[2]<alpha.bonferroni
diversity.test (H.LI, H.M, var.HLI, var.HM, N.LI, N.M)[2]<alpha.bonferroni

diversity.test (H.ESE, H.M, var.HESE, var.HM, N.ESE, N.M)[2]<alpha.bonferroni
```

Fent el t-test de diversitat sota la hipòtesis nula d’igualtat de
diversitat entre les diferents etapes i ajustant per bonferroni podem
concloure que **no és detecten diferències significatives en la
diversitat entre cap de les fases de desenvolupament de les plantes.**
Hem de tenir en compte que aquest mètode d’ajust és molt conservador.

A l’article es feia això mateix ajustant per Tuckey, d’aquesta manera és
diferenciaven dos grups d’estands segons el índex de Shannon, un format
per els stands EI i LD i l’altre corresponent al stand M. L’stand ESE
està a cavall entre aquest dos grups.

``` r
D.EI <- diversity(yr.EI, index="simpson")
D.LI <- diversity(yr.LI, index="simpson")
D.ESE <- diversity(yr.ESE, index="simpson")
D.M <- diversity(y.M, index="simpson")
c(D.EI, D.LI, D.ESE, D.M)
```

    ## [1] 0.9586735 0.9582123 0.9597400 0.9548400

# Modelització

## Geometric series per EDP

Una manera d’estudiar la diversitat d’espècies és mitjançant el paramtre
k de la serie geomètrica. Com major sigui k , hi haurà una distribució
menys igualitaria entre les espècies i per tant, menys diversitat. Per
el contrari, com menor sigui k, la distribució entre les espècies serà
més igualitaria i per tant hi haurà una major diversitat.

El paramtre k pot ser estimat mitjançant una regressió lineal.

``` r
v.x <- 1:S
lny <- as.vector(log(rel.y))

summary(lm(lny ~ v.x))
```

    ## 
    ## Call:
    ## lm(formula = lny ~ v.x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.61460 -0.06586  0.02525  0.13354  0.28200 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -2.591394   0.045512  -56.94   <2e-16 ***
    ## v.x         -0.069957   0.001114  -62.79   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1884 on 68 degrees of freedom
    ## Multiple R-squared:  0.983,  Adjusted R-squared:  0.9828 
    ## F-statistic:  3942 on 1 and 68 DF,  p-value: < 2.2e-16

La recta de regressió és:

$$
log(abundancia) = -2.591394-0.069957t 
$$

Datat que el p-valor associat a la variable t(\<2e-16) és inferior al
nivell de significació del 5%, podem concloure que el model és
significatiu.

La pendent de la recta estimada és de -0.069957 i el coeficient de
determinació (Adjusted R-Squared) és de 0.9828 podem afirmar que hi ha
una bona bondad d’ajust.

``` r
plot(1:S, log(rel.y), "o", pch=20, xlab= "especie", ylab="log(f(x))", 
     main= "Rank-abundance plot logaritmic", col = "royalblue1")
abline (a = -2.591394, b = -0.069957, col = "salmon", lwd = 2)
```

<img src="INFORME-FINAL_files/figure-gfm/unnamed-chunk-38-1.png" style="display: block; margin: auto;" />

Podem observar que:

$$ln(1-k)=-0.069957$$

Així doncs, la k és pot calcular com:
$$k=1-e^{-0.069957}=0.067566$$Finalment, podem concloure que el valor de
k és molt petit, i que per tant, hi haura una gran diversitat
d’espècies.

Gràficament, podem observar que el patró de caigua pot considerar-se
lineal.

A continuació, podem comprovar si el model de la serie goemetrica
s’ajusta bé a les dades, és per això que passem a gradicar diferents
aspectes a tenir en compte com ara la normalitat i la heterocedasticitat
del model:

``` r
plot(lm(lny ~ v.x))
```

![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-39-3.png)<!-- -->![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-39-4.png)<!-- -->

Si eliminem les observacions situades més a l’esquerra, que tenen un
comportament extrany, podem observar que hi ha homogeneïtat dels
residus. És a dir, que els residus estan homogeniament distribuïts.

Ala vista de l’QQ-plot és difícil acceptar el supòsit de normalitat. Els
valors centrals sí que s’ajusten correctament, però no podem dir el
mateix dels valors situats a les cues.

Per tant, la conclusió a la que arribem es que el model de la sèrie
geomètrica pensem que té una bona bondat d’ajust. Però per poder
extreure conclusions hauríem de fer el test analítics de normalitat i
homogeneïtat de variàncies.

## Broken stick model per EDP

``` r
 out <- rad.null(y)
yhat <- fitted(out)
```

``` r
plot(out, col = "royalblue1", lwd= 2, main = "Broken stick model")
```

![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

Podem observar que el model del bastó trencat s’ajusta correctament a
les dades. Una millor representació gràfica d’aquest model es
expressar-lo en escala logarítmica.

``` r
plot(1:S, log(rel.y), "o", pch=20, xlab= "especie", ylab="log(f(x))", 
     main= "Rank-abundance plot logaritmic", col = "royalblue1")
lines(1:S, log(as.vector(yhat)/N), col = "salmon", lwd = 2)
```

![](INFORME-FINAL_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

Finalment, arribem a les mateixes conclusions que amb gràfic anterior.
**El model del bastó trencat s’ajusta correctament a les dades.**

Si comparem el model del bastó trencat amb el model de la sèrie
geometrica podem concloure que el model que s’ajusta millor és el model
de la sèrie geomètrica ja que les dades es troben més properes en el
espai a la recta que no pas a la corba del bastó trencat.

# Conclusions

L’objectiu d’aquest estudi, com ja hem comentat era analitzar per una
banda, com **influeix la vegetació regeneradora en la composició i en
l’abundància de la comunitats d’aus** i per altra banda si aquesta
composició i l’abundància és diferència d’alguna manera amb la que hi ha
en els stands de vegetació heretaga (de menor intervenció humana).

El primer que hem conclòs amb les diferents mesures de diversitat i el
t-test adjutat per bonferroni és que **no podem considerar que
existeixin diferències significatives entre la diversitat entre cap de
les fases de desenvolupament de les plantes**.

Nosaltres hem considerat un ajust molt conservador, l’article fa servir
l’ajust de Tuckey i detecta diferències entre els stand s’inicialització
primerenca i tardana amb respecta dels stands no gestionats. El que diu
l’article és que la diversitat és major en els estands regenerats que en
els estands no gestionats.

Pel que fa a la composició de les espècies a cadascuna de les fases de
desenvolupament hem conclòs que **a les regions de bosc regenerades hi
ha una major abundància de les espècies que aniden als arbustos i als
arbres, pel que fa als gremis nidícoles, i una major abundància de les
espècies que s’alimenten de fulles**. Però que, **a mesura que
disminueix el grau de gestió**, és a dir, el grau de regeneració,
**disminueix l’abundància d’aquests tipus de espècies alhora que
augmenta la abundància d’ocells nidificants al terra**.

Això dona un punt a favor de la teoria que diu que a mesura que augmenta
la densitat de vegetació, la vegetació més feble, especialment els
arbustos,es redueix i conseqüentment es redueixen els exemplars de
espècies d’ocells que aniden o s’alimenten d’aquest tipus de vegetació.

Per últim hem considerat que el **model de sèrie geomètrica** era el que
ajustava millor la abundància de les espècies en escala logarítmica.
