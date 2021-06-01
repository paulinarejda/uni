library(olsrr)

# Problem badawczy. Zmienne -----------------------------------------------

  # Predykcja wagi ryb na podstawie ich dokładnych wymiarów 

  # Zbiór danych jest zawiwra pomiary różnych gatunków ryb dostępnych
  # w sprzedaży na rynku rybnym.
  # Zmienne określają wagę ryb i ich dokładne pomiary (w cm), w tym:
    # length1 - długość w pionie
    # length2 - długość diagonalna 
    # length3 - długość poprzeczna

library(olsrr)
options(scipen = 999)


# Model regresji ----------------------------------------------------------


# MODEL 1
  # bierzemy wszystkie zmienne

model1 <- lm(data2$Weight ~ ., 
             data = data2)
summary(model1)


  # R^2 rowne 0,88 mowi o tym, ze jest dobrze dopasowany do danych (zmienna 
  # objasniana jest wyjasniana przez objaśniajace w 88%) 

  # statystyka F = 236.2, p-value bliskie zeru ->  odrzucamy H0, przynajmniej
  # jedna zmienna wpływa istotnie na wagę ryby
  # istotna zmienna - height - p-value < 0.05
  # stała jest istotna, p-value bliskie 0
  # (mamy jeszcze zm. length3, gdzie p-value = 0.09, czyli mniej niz 0.1


# MODEL 2
  # bierzemy do modelu jedynie w.w. istotną zmienną
model2 <- lm(data = data2, 
             Weight ~ Height)
summary(model2)


# MODEL 3
  # bierzemy istotną zmienną i zmienną, dla której p-value < 0.1

model3 <- lm(data = data2, 
             Weight ~ Height + Length3)
summary(model3)

#W obu modelach wszystkie zmienne są istotne. 
#Jeżeli  długość poprzeczna się o centymetr, to waga zmieni się o 25.24 gramów.
#Jeżeli wysokość zwiększy się o centymetr, to waga zwiększy się o 12.41 gramów.


# Wspolliniowosc ----------------------------------------------------------

# tolerancja i VIF (Variance Inflation Factor)
ols_vif_tol(model3)

#Wartości tego parametru wyższe niż 10 wskazują na to, iż występuje wysoka współliniowość. 
#Czasem przyjmuje się nawet bardziej konserwatywny limit dolny – 2.5.
#W tym wypadku VIF wynosi 1.98, a więc nie występuje zjawisko współliniowości.


# Ocena modelu ------------------------------------------------------------------

# Residuals vs Fitted 
# wartosc reszt zale?y od y^ - jest to niepozadane zjawisko, 
# postac modelu nie jest dobra 
plot(model3, 1)

# QQplot
# punkty układają się wzdluz linii, świadczy to o normalnosci rozkladu
plot(model3, which=2)

# Scale-Location
plot(model3, which=3)
  # brak heteroskedastycznosci

# - Obserwacje wpływowe --------------------------------------------------

# ODLEGŁOŚĆ COOKA

plot(model3, which=4)
cooks.distance(model3)

#trzy obserwacje 144, 145 oraz 143 osiągnęły o wiele wyższe wartości 
#odległości Cooka niż pozostałe.

# wartosci dzwigni
hatvalues(model3) 
#nie ma obserwacji o wysokiej dźwigni. 
#Większość obserwacji nie przekracza wartości 0.05, 
#a tylko niektóre przekraczają 0.1.

# wykres
plot(hatvalues(model3), type= 'h') 
#W modelu tym nie znalazły się więc obserwacje odstające.








