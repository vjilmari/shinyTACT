TACT_fi<-
  c(language="fi",
    low="matala",
    medium="keskimääräinen",
    high="korkea",
    correlation="Korrelaatio",
    match_1="% matalista, keskimääräisistä ja korkeista arvoista osuvat\nsamoihin kategorioihin molemmilla akseleilla.\n",
    match_2="Satunnaisella arvauksella luku olisi 33.3%")

TACT_en<-
  c(language="en",
    low="low",
    medium="medium",
    high="high",
    correlation="Correlation",
    match_1="% of low, medium and high values match.\n",
    match_2="With random-guess this percentage would be 33.3%")

TACT_languages<-
  data.frame(rbind(TACT_fi,TACT_en))


