TACT_fi<-
  c(language="fi",
    low="matala",
    medium="keskimääräinen",
    high="korkea",
    correlation="Korrelaatio",
    match_1="% matalista, keskimääräisistä ja korkeista arvoista osuvat\nsamoihin kategorioihin molemmilla akseleilla",
    match_2="Satunnaisella arvauksella luku olisi 33.3%")

TACT_en<-
  c(language="en",
    low="low",
    medium="medium",
    high="high",
    correlation="Correlation",
    match_1="% of low, medium and high values match",
    match_2="With no correlation, this percentage would be 33.3%")

TACT_ge <- c(
  language = "ge",
  low = "niedrig",
  medium = "mittel",
  high = "hoch",
  correlation = "Korrelation",
  match_1 = "% der niedrigen, mittleren und hohen Werte stimmen überein",
  match_2 = "Mit einer Zufallsguessing wäre dieser Prozentsatz 33.3%."
)

TACT_es <- c(
  language = "es",
  low = "madal",
  medium = "keskmine",
  high = "kõrge",
  correlation = "Korrelatsioon",
  match_1 = "% madalatest, keskmistest ja kõrgetest väärtustest kattuvad",
  match_2 = "Juhusliku arvamisega oleks see protsent 33.3%."
)


TACT_languages<-
  data.frame(rbind(TACT_fi,TACT_en,TACT_ge,TACT_es))


