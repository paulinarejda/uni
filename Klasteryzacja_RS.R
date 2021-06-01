
# INSTALACJA PAKIETÓW -----------------------------------------------------


install.packages("Hmisc")
install.packages("imputeTS")
install.packages("naniar")
install.packages("VIM")
install.packages("DMwR")
set.seed(123)

install.packages("factoextra")
install.packages("cluster")
install.packages("dbscan")
install.packages("NbClust")
install.packages("mclust")

library("Hmisc")
library("imputeTS")
library("naniar")
library("VIM")
library("DMwR")
library("factoextra")
library("cluster")
library("dbscan")
library("NbClust")
library("mclust")


# PROBLEM BADAWCZY. ZMIENNE -----------------------------------

  # Grupowanie win na podstawie ich analizy chemicznej

# Wykorzystane dane są wynikiem analizy chemicznej win uprawianych w tym samym
# regionie Włoch, ale pochodzących z różnych odmian. 
# Zmienne określają ilości 13 składników znajdujących się w każdym rodzaju win.


# BRAKI -------------------------------------------------------------------

vis_miss(data1)

#nie ma brakow

# I. ANALIZA SKUPIEŃ ------------------------------------------------------


# standaryzacja
stand <- scale(data1)


# Metoda k-średnich -------------------------------------------------------


# liczba skupien - wskaźnik sylwetkowy
NbClust(data=stand, method = "kmeans", index = "silhouette")

res_k_means_sil <- NbClust(data=stand, 
                      method = "kmeans", 
                      index = "silhouette")

plot(2:15, res_k_means_sil$All.index,
     xlab = 'Liczba skupien',
     ylab = 'Indeks sylwetkowy',
     type = 'o',
     ylim = c(0,max(res_k_means_sil$All.index)))

# 3 to optymalna liczba skupień

# szczegolowe informacje o wsk. sylwetkowym dla k=3
sil <- fviz_silhouette(eclust(stand, 
                              FUNcluster ="kmeans", 
                              k = 3))
sil
#przedstawione są tu wskaźniki sylwetkowe dla każdej obserwacji
sil[1]

#Średni wskaźnik sylwetkowy to 0.28. 
#Dla skupienia pierwszego wynosi on 0.35, dla drugiego 0.34,
#a dla trzeciego 0.18. 


# PRZYNALEŻNOŚĆ DO SKUPIEŃ 
k_means <- kmeans(stand, 3)

# przynaleznosc do skupien
k_means$cluster

# srodki ciezkosci skupien
k_means$centers
barplot(t(k_means$centers),
        beside = TRUE,
        col = rainbow(10),
        ylim = c(-2, 2),
        xlab = 'Srodki ciezkosci skupien')


# Metoda PAM --------------------------------------------------------------


res_pam <- pam(stand, k=3, nstart=30)

# podsumowanie
res_pam

# numery obiektow bedacych medoidami
res_pam$id.med

# szczegolowe informacje o skupieniach
res_pam$clusinfoRES

# informacje o indeksie sylwetkowym
res_pam$silinfo

sil_pam <- silhouette(res_pam$cluster, 
                      dist(stand))

plot(sil_pam)

#Medoidami zostały następujące obiekty: 36, 107 oraz 149. 
#Średni wskaźnik sylwetkowy to 0.27. Dla skupienia pierwszego wynosi on 0.25, 
#dla drugiego 0.22, a dla trzeciego 0.34


# Fuzzy clustering --------------------------------------------------------

res_fuz <- fanny(stand, k=2) 

# podsumowanie
res_fuz

# stopien przynaleznosci
res_fuz$membership

# miara Dunna
res_fuz$coeff

# informacje o indeksie sylwetkowym
res_fuz$silinfo

sil_fuz <- silhouette(res_fuz$cluster, dist(stand))
plot(sil_fuz)



#Optymalna liczba skupień według fuzzy-clustering to 2. 
#Indeks Dunna wyniósł 0.11, słaba jakośc grupowania. 
#Średni wskaźnik sylwetkowy jest równy 0.26. 
#Dla skupienia pierwszego wyniósł on 0.28, a dla skupienia drugiego 0.24.








