library(ggplot2)
library(cowplot)


#-------------------------- Import potrzebnych danych

kurs_2019 <- read.csv(file = 'C:/Users/Piotr/Desktop/Programowanie/R/Analiza danych/Allegro/Dane/NBP/archiwum_tab_a_2019.csv', sep=';')
kurs_2020 <- read.csv(file = 'C:/Users/Piotr/Desktop/Programowanie/R/Analiza danych/Allegro/Dane/NBP/archiwum_tab_a_2020.csv', sep=';')

#-------------------------- Przygotowanie potrzebych danych

dolar_2019 <- data.frame(kurs_2019$X1USD)
dolar_2020 <- data.frame(kurs_2020$X1USD)

euro_2019 <- data.frame(kurs_2019$X1EUR)
euro_2020 <- data.frame(kurs_2020$X1EUR)

juan_2019 <- data.frame(kurs_2019$X1CNY)
juan_2020 <- data.frame(kurs_2020$X1CNY)

daty_2019 <- data.frame(kurs_2019$ï.¿daty)
daty_2020 <- data.frame(kurs_2020$ï.¿data)

dolary_i_daty_2019 <- data.frame(kurs_2019$X1USD, kurs_2019$ï.¿daty)
dolary_i_daty_2020 <- data.frame(kurs_2020$X1USD, kurs_2020$ï.¿data)

#-------------------------- Analzia zebranych danych

# Srednia

dolar_2019_sr <- sapply(dolar_2019, mean)
dolar_2020_sr <- sapply(dolar_2020, mean)

# Wartosc minimalna

dolar_2019_min <- sapply(dolar_2019, min)
dolar_2020_min <- sapply(dolar_2020, min)

# Wartosc maksymalna

dolar_2019_max <- sapply(dolar_2019, max)
dolar_2020_max <- sapply(dolar_2020, max)

# Dominanta

dom_f <- function(x) {
  un <- unique(x)
  un[which.max(tabulate(match(x, un)))]
}

dolar_2019_dom <- sapply(dolar_2019, dom_f)
dolar_2020_dom <- sapply(dolar_2020, dom_f)

# Odchylenie standardowe

dolar_2019_odch <- sapply(dolar_2019, sd)
dolar_2020_odch <- sapply(dolar_2020, sd)

# Korealcja

dolar_do_euro_2019 <- cor(dolar_2019, euro_2019)
dolar_do_juan_2019 <- cor(dolar_2019, juan_2019)

dolar_do_euro_2020 <- cor(dolar_2020, euro_2020)
dolar_do_juan_2020 <- cor(dolar_2020, juan_2020)

#-------------------------- Wizualizacja danych

# Kurs dolara w czasie

dolar_wykres_2019 <- ggplot(dolary_i_daty_2019, aes(kurs_2019.ï.¿daty, kurs_2019.X1USD)) + labs(y="Kurs dolara", x = "Czas") + geom_point() + geom_smooth(se = FALSE) + ggtitle("Kurs dolara w czasie w 2019") + theme(plot.title = element_text(hjust = 0.5))
dolar_wykres_2020 <- ggplot(dolary_i_daty_2020, aes(kurs_2020.ï.¿data, kurs_2020.X1USD)) + labs(y="Kurs dolara", x = "Czas") + geom_point() + geom_smooth(se = FALSE) + ggtitle("Kurs dolara w czasie w 2020") + theme(plot.title = element_text(hjust = 0.5))

# Rozklad normalny(Gaussa)

dolar_norm_2019 <- ggplot(data = dolar_2019, aes(kurs_2019.X1USD)) +
                          stat_function(fun = dnorm, n = 101, args = list(mean = dolar_2019_sr, sd = dolar_2019_odch)) + ylab("") +
                          scale_y_continuous(breaks = NULL) + labs(x = "Kurs dolara") + xlim(3.6, 4.1) + ggtitle("Rozk³ad normalny kursu dolara w 2019") + theme(plot.title = element_text(hjust = 0.5))

dolar_norm_2020 <- ggplot(data = dolar_2020, aes(kurs_2020.X1USD)) +
                          stat_function(fun = dnorm, n = 101, args = list(mean = dolar_2020_sr, sd = dolar_2020_odch)) + ylab("") +
                          scale_y_continuous(breaks = NULL) + labs(x = "Kurs dolara") + xlim(3.5, 4.3) + ggtitle("Rozk³ad normalny kursu dolara w 2020") + theme(plot.title = element_text(hjust = 0.5))
                           











