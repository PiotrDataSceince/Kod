import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats




#------------------ Import Danych

df1 = pd.read_csv(r'C:\Users\Piotr\Desktop\Programowanie\Python\Analizy Danych\Dane\GUS\ceny_zyw_polska_2006-2019.csv',                        # Ceny wybranych produktów 2006 - 2019, w pln
                        sep=';')

#------------------ Przygotowanie danych do analizy

# Wszystkie dane o wyroznionych towarach

ryz = df1.loc[df1['Rodzaje towarów i usług'] == "ryż_za_1kg"]
chleb = df1.loc[df1['Rodzaje towarów i usług'] == "chleb pszenno-żytni - za 0.5kg"]
ziemniaki = df1.loc[df1['Rodzaje towarów i usług'] == "ziemniaki - za 1 kg"]
jajka = df1.loc[df1['Rodzaje towarów i usług'] == "jaja kurze świeże - za 1szt"]

# Ceny w poszczegolnych miesiacach

# Ryz

ryz_st = ryz.loc[ryz['Miesiące'] == 'styczeń']
ryz_lt = ryz.loc[ryz['Miesiące'] == 'luty']
ryz_mr = ryz.loc[ryz['Miesiące'] == 'marzec']
ryz_kw = ryz.loc[ryz['Miesiące'] == 'kwiecień']
ryz_mj = ryz.loc[ryz['Miesiące'] == 'maj']
ryz_cz = ryz.loc[ryz['Miesiące'] == 'czerwiec']
ryz_lp = ryz.loc[ryz['Miesiące'] == 'lipiec']
ryz_sp = ryz.loc[ryz['Miesiące'] == 'sierpień']
ryz_wr = ryz.loc[ryz['Miesiące'] == 'wrzesień']
ryz_pz = ryz.loc[ryz['Miesiące'] == 'październik']
ryz_ls = ryz.loc[ryz['Miesiące'] == 'listopad']
ryz_gr = ryz.loc[ryz['Miesiące'] == 'grudzień']

# Chleb

chleb_st = chleb.loc[chleb['Miesiące'] == 'styczeń']
chleb_lt = chleb.loc[chleb['Miesiące'] == 'luty']
chleb_mr = chleb.loc[chleb['Miesiące'] == 'marzec']
chleb_kw = chleb.loc[chleb['Miesiące'] == 'kwiecień']
chleb_mj = chleb.loc[chleb['Miesiące'] == 'maj']
chleb_cz = chleb.loc[chleb['Miesiące'] == 'czerwiec']
chleb_lp = chleb.loc[chleb['Miesiące'] == 'lipiec']
chleb_sp = chleb.loc[chleb['Miesiące'] == 'sierpień']
chleb_wr = chleb.loc[chleb['Miesiące'] == 'wrzesień']
chleb_pz = chleb.loc[chleb['Miesiące'] == 'październik']
chleb_ls = chleb.loc[chleb['Miesiące'] == 'listopad']
chleb_gr = chleb.loc[chleb['Miesiące'] == 'grudzień']

# Ziemniaki

ziemniaki_st = ziemniaki.loc[ziemniaki['Miesiące'] == 'styczeń']
ziemniaki_lt = ziemniaki.loc[ziemniaki['Miesiące'] == 'luty']
ziemniaki_mr = ziemniaki.loc[ziemniaki['Miesiące'] == 'marzec']
ziemniaki_kw = ziemniaki.loc[ziemniaki['Miesiące'] == 'kwiecień']
ziemniaki_mj = ziemniaki.loc[ziemniaki['Miesiące'] == 'maj']
ziemniaki_cz = ziemniaki.loc[ziemniaki['Miesiące'] == 'czerwiec']
ziemniaki_lp = ziemniaki.loc[ziemniaki['Miesiące'] == 'lipiec']
ziemniaki_sp = ziemniaki.loc[ziemniaki['Miesiące'] == 'sierpień']
ziemniaki_wr = ziemniaki.loc[ziemniaki['Miesiące'] == 'wrzesień']
ziemniaki_pz = ziemniaki.loc[ziemniaki['Miesiące'] == 'październik']
ziemniaki_ls = ziemniaki.loc[ziemniaki['Miesiące'] == 'listopad']
ziemniaki_gr = ziemniaki.loc[ziemniaki['Miesiące'] == 'grudzień']

# Jajka

jajka_st = jajka.loc[jajka['Miesiące'] == 'styczeń']
jajka_lt = jajka.loc[jajka['Miesiące'] == 'luty']
jajka_mr = jajka.loc[jajka['Miesiące'] == 'marzec']
jajka_kw = jajka.loc[jajka['Miesiące'] == 'kwiecień']
jajka_mj = jajka.loc[jajka['Miesiące'] == 'maj']
jajka_cz = jajka.loc[jajka['Miesiące'] == 'czerwiec']
jajka_lp = jajka.loc[jajka['Miesiące'] == 'lipiec']
jajka_sp = jajka.loc[jajka['Miesiące'] == 'sierpień']
jajka_wr = jajka.loc[jajka['Miesiące'] == 'wrzesień']
jajka_pz = jajka.loc[jajka['Miesiące'] == 'październik']
jajka_ls = jajka.loc[jajka['Miesiące'] == 'listopad']
jajka_gr = jajka.loc[jajka['Miesiące'] == 'grudzień']



#------------------ Analiza danych

# Srednia 

ryz_sr = ryz["Wartosc"].mean()
chleb_sr = chleb["Wartosc"].mean()
ziemniaki_sr = ziemniaki["Wartosc"].mean()
jajka_sr = jajka["Wartosc"].mean()

# Odchylenie standardowe 

ryz_odch = ryz["Wartosc"].std()
chleb_odch = chleb["Wartosc"].std()
ziemniaki_odch = ziemniaki["Wartosc"].std()
jajka_odch = jajka["Wartosc"].std()

# Wariancja

ryz_war = ryz["Wartosc"].var()
chleb_war = chleb["Wartosc"].var()
ziemniaki_war = ziemniaki["Wartosc"].var()
jajka_war = jajka["Wartosc"].var()


# Maksymalna wartosc

ryz_max = ryz["Wartosc"].max()
chleb_max = chleb["Wartosc"].max()
ziemniaki_max = ziemniaki["Wartosc"].max()
jajka_max = jajka["Wartosc"].max()
    
# Minimalna wartosc

ryz_min = ryz["Wartosc"].min()
chleb_min = chleb["Wartosc"].min()
ziemniaki_min = ziemniaki["Wartosc"].min()
jajka_min = jajka["Wartosc"].min()

# Mediana

ryz_med = ryz["Wartosc"].median()
chleb_med = chleb["Wartosc"].median()
ziemniaki_med = ziemniaki["Wartosc"].median()
jajka_med = jajka["Wartosc"].median()

# Dominanta

ryz_dom = ryz["Wartosc"].mode()
chleb_dom = chleb["Wartosc"].mode()
ziemniaki_dom = ziemniaki["Wartosc"].mode()
jajka_dom = jajka["Wartosc"].mode()

# Typowy przedzial zmiennosci(klasyczny)

ryz_typ = (ryz_sr - ryz_odch, ryz_sr + ryz_odch)
chleb_typ = (chleb_sr - chleb_odch, chleb_sr + chleb_odch)
ziemniaki_typ = (ziemniaki_sr - ziemniaki_odch, ziemniaki_sr + ziemniaki_odch)
jajka_typ = (jajka_sr - jajka_odch, jajka_sr + jajka_odch)

# Kwantyle

ryz_quan = ryz["Wartosc"].quantile([.125, .25, .5, .75])
chleb_quan = chleb["Wartosc"].quantile([.125, .25, .5, .75])
ziemniaki_quan = ziemniaki["Wartosc"].quantile([.125, .25, .5, .75])
jajka_quan = jajka["Wartosc"].quantile([.125, .25, .5, .75])

# Wspolczynnik zmiennosci

ryz_zmn = ryz_odch/ryz_sr
chleb_zmn = chleb_odch/chleb_sr
ziemniaki_zmn = ziemniaki_odch/ziemniaki_sr
jajka_zmn = jajka_odch/jajka_sr

# Wspolczynnik asymetrii

ryz_as = (ryz_sr - ryz_dom) / ryz_odch
chleb_as = (chleb_sr - chleb_dom) / chleb_odch
ziemniaki_as = (ziemniaki_sr - ziemniaki_dom) / ziemniaki_odch
jajka_as = (jajka_sr - jajka_dom) / jajka_odch

#------------------ Wizualizacja danych

dane_x=['ryż', 'chleb', 'ziemniaki', 'jajka']

# Histogram ze srednia cena towarow

dane_y_sr=[ryz_sr, chleb_sr, ziemniaki_sr, jajka_sr]
plt.title('Średnia cena towarów')
plt.bar(dane_x, height=dane_y_sr ,width=0.8)
plt.savefig("sr ceny.png", dpi=150)

# Histogram z cena maksymanlna

dane_y_max=[ryz_max, chleb_max, ziemniaki_max, jajka_max]
plt.title('Maksymalna cena towarów')
plt.bar(dane_x, height=dane_y_max ,width=0.8)
plt.savefig("max ceny.png", dpi=150)

# Histogram z cena minimalna

dane_y_min=[ryz_min, chleb_min, ziemniaki_min, jajka_min]
plt.title('Minimalna cena towarów')
plt.bar(dane_x, height=dane_y_min, width=0.8)
plt.savefig("min cena.png", dpi=150)

# Histogram z dominanta

dane_y_dom=[ryz_dom.tolist()[0], chleb_dom.tolist()[0], ziemniaki_dom.tolist()[0], jajka_dom.tolist()[0]]
plt.title('Dominanta ceny towarów')
plt.bar(dane_x, height=dane_y_dom, width=0.8)
plt.savefig("dom cena.png", dpi=150)

# Rozklad normalny(Gaussa)

# Ryz

x_ryz_gauss = np.linspace(ryz_min , ryz_max + 0.5, 100)
y_ryz_gauss = scipy.stats.norm.pdf(x_ryz_gauss,ryz_sr,ryz_odch)
plt.plot(x_ryz_gauss,y_ryz_gauss, color='coral')
plt.grid()
plt.title('Rozkład normalny cen ryżu w latach 2006-2019',fontsize=10)
plt.savefig("rozklad ryz.png", dpi=150)


# Chleb

x_chleb_gauss = np.linspace(chleb_min , chleb_max + 0.1, 100)
y_chleb_gauss = scipy.stats.norm.pdf(x_chleb_gauss,chleb_sr,chleb_odch)
plt.plot(x_chleb_gauss,y_chleb_gauss, color='coral')
plt.grid()
plt.title('Rozkład normalny cen chleba w latach 2006-2019',fontsize=10)
plt.savefig("rozklad chleb.png", dpi=150)

# Ziemniaki

x_ziemniaki_gauss = np.linspace(ziemniaki_min-1 , ziemniaki_max , 100)
y_ziemniaki_gauss = scipy.stats.norm.pdf(x_ziemniaki_gauss,ziemniaki_sr,ziemniaki_odch)
plt.plot(x_ziemniaki_gauss,y_ziemniaki_gauss, color='coral')
plt.grid()
plt.title('Rozkład normalny cen ziemniakiów w latach 2006-2019',fontsize=10)
plt.savefig("rozklad ziemniaki.png", dpi=150)


# Jajka

x_jajka_gauss = np.linspace(jajka_min , jajka_max + 0.5, 100)
y_jajka_gauss = scipy.stats.norm.pdf(x_jajka_gauss,jajka_sr,jajka_odch)
plt.plot(x_jajka_gauss,y_jajka_gauss, color='coral')
plt.grid()
plt.title('Rozklad normalny cen jajkau w latach 2006-2019',fontsize=10)
plt.savefig("rozklad jajka.png", dpi=150)


