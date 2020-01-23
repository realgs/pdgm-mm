package Gra;

public class Mapa {
    private Pole[] pole;

    public Mapa() {
        this.pole = new Pole[14];
        int i = 0;
        for (; i < 6; i++) {
            pole[i] = new Pole(6, false, 1);
        }
        pole[i++] = new Pole(0, true, 1);
        for (; i < 13; i++) {
            pole[i] = new Pole(6, false, 2);
        }
        pole[i] = new Pole(0, true, 2);
    }

    /**
     * @param index index z ktorego rozpoczynamy przesuwanie kamykow
     * @param gracz 1 - gracz pierwszy; 2 - gracz drugi; numer aktualnego gracza
     * @return ostatni index na ktory wpadl kamyk
     */
    int przesunKamyki(int index, int gracz) {
        int iloscDoPrzesuniecia = this.pole[index].getIloscKamykow();
        this.pole[index].setIloscKamykow(0);
        if (iloscDoPrzesuniecia <= 0) {
            //System.out.println("index: " + index + "; gracz: " + gracz +"; iloscDoPrzesuniecia: " + iloscDoPrzesuniecia);
            return -1;
        }
        for (int i = 1; i < iloscDoPrzesuniecia + 1; i++) {
            if (pole[(index + i) % (pole.length)].isCzyKoncowy() == true && pole[(index + i) % (pole.length)].getCzyjePole() != gracz) {
                iloscDoPrzesuniecia++;
            } else {
                pole[(index + i) % (pole.length)].setIloscKamykow(pole[(index + i) % (pole.length)].getIloscKamykow() + 1);
            }
        }
        return (index + iloscDoPrzesuniecia) % (pole.length);
    }

    /**
     * @param index index z ktorego zostal wykonany ruch
     * @param gracz 1 - gracz pierwszy; 2 - gracz drugi; numer aktualnego gracza
     * @return ktora spelniona zostala zasada
     */
    int sprawdzZasady(int index, int gracz) {
        int iloscDoPrzesuniecia = 0;
        for (int i = index; i < (this.pole[index].getIloscKamykow()) + index; i++) { // musze dodac do index 1 i do jakiego zakresu te dodac 1
            if (this.pole[i % (this.pole.length)].isCzyKoncowy() == true && !(this.pole[i % (this.pole.length)].getCzyjePole() == gracz)) {
                iloscDoPrzesuniecia += 2;
            } else {
                iloscDoPrzesuniecia += 1;
            }
        }
        Pole poleOstatniegoKamyka = this.pole[(index + iloscDoPrzesuniecia) % (pole.length)];
        int spelnienieZasady = 0;
        boolean czyKoncowy = poleOstatniegoKamyka.isCzyKoncowy();
        if (poleOstatniegoKamyka.getCzyjePole() == gracz && czyKoncowy) spelnienieZasady = 1;
        if ((poleOstatniegoKamyka.getIloscKamykow() == 0) && (!czyKoncowy) && (poleOstatniegoKamyka.getCzyjePole() == gracz)) {
            spelnienieZasady = 2;
        }
        return spelnienieZasady;
    }

    /**
     * Metoda wykonująca zasadę, która mówi: jeśli ostatni kamień wpadł do własnego pustego dołka, gracz bierze wszystkie kamienie z leżącego naprzeciw dołka przeciwnika i wkłada je do swojej bazy.
     * @param ostatniIndex index, na który wpadł ostatni kamyk
     * @param gracz numer gracza
     */
    void wykonajZasade2(int ostatniIndex, int gracz) {
        int przeciwnyIndex;
        int indexGlownego;

        if (gracz == 1) {
            przeciwnyIndex = ostatniIndex + ((6 - ostatniIndex) * 2);
            indexGlownego = pole.length / 2 - 1;
        } else {
            przeciwnyIndex = (ostatniIndex + ((13 - ostatniIndex) * 2)) % pole.length;
            indexGlownego = pole.length - 1;
        }
        if (pole[przeciwnyIndex].getIloscKamykow() == 0) {
            pole[indexGlownego].setIloscKamykow(pole[indexGlownego].getIloscKamykow() + pole[przeciwnyIndex].getIloscKamykow());
            pole[przeciwnyIndex].setIloscKamykow(0);
        } else {
            pole[indexGlownego].setIloscKamykow(pole[indexGlownego].getIloscKamykow() + pole[przeciwnyIndex].getIloscKamykow() + pole[ostatniIndex].getIloscKamykow());
            pole[przeciwnyIndex].setIloscKamykow(0);
            pole[ostatniIndex].setIloscKamykow(0);
        }
    }

    /**
     * metoda sprawdza czy gra dobiegła końca
     * @return true - zakończona; false - trwa nadal
     */
    boolean sprawdzCzyKoniec() {
        if (pole[6].getIloscKamykow() >= 0 || pole[13].getIloscKamykow() >= 0) {
            int ileSpelnia = 0;
            for (int i = 0; i < 6; i++) {
                if ((pole[i].getIloscKamykow() != 0)) ;
                else ileSpelnia++;
            }
            if (ileSpelnia == 6) return true;
            ileSpelnia = 0;
            for (int i = 7; i < 13; i++) {
                if ((pole[i].getIloscKamykow() != 0)) ;
                else ileSpelnia++;
            }
            if (ileSpelnia == 6) return true;
        }
        return false;
    }

    /**
     * Meroda używana po zakończeniu rogrywki, wrzuca pozostałe kamyki z mapy do baz
     */
    void zsumujKamyki() {
        int i = 0;
        for (; i < 6; i++) {
            pole[6].setIloscKamykow(pole[6].getIloscKamykow() + pole[i].getIloscKamykow());
        }
        for (i++; i < 13; i++) {
            pole[13].setIloscKamykow(pole[13].getIloscKamykow() + pole[i].getIloscKamykow());
        }
    }

    /**
     * Metoda wyświetlająca wyniki rozegranej rozgrywki
     */
    void podajWynik() {
        System.out.println("\n\n@@ WYNIKI @@");
        int wynikGracz1 = pole[6].getIloscKamykow();
        int wynikGracz2 = pole[13].getIloscKamykow();
        System.out.println("Gracz 1:" + wynikGracz1);
        System.out.println("Gracz 2:" + wynikGracz2);
        if (wynikGracz1 > wynikGracz2) System.out.println("Wygrał Gracz 1");
        else if (wynikGracz1 < wynikGracz2) System.out.println("Wygrał Gracz 2");
        else System.out.println("Remis");

    }

    /**
     * Metoda wyświetlajaca aktualna mape rogrywki
     */
    void pokazMape() {
        //baza to indexy 6 i 13
        int index = pole.length - 2;
        for (int i = 0; i < 6; i++) {
            System.out.print("\t" + pole[index].getIloscKamykow());
            index--;
        }
        index = 6;
        System.out.println("\n" + pole[pole.length - 1].getIloscKamykow() + "\t\t\t\t\t\t\t" + pole[index].getIloscKamykow());
        index = 0;

        for (int i = 0; i < 6; i++) {
            System.out.print("\t" + pole[index].getIloscKamykow());
            index++;
        }
    }

    public Pole[] getPole() {
        return pole;
    }

    public void setPole(Pole[] pole) {
        this.pole = pole;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        Mapa mapa = null;
        try {
            mapa = new Mapa();
        } catch (Exception e) {
            e.printStackTrace();
        }
        Pole[] pole = new Pole[14];
        for (int i = 0; i < 14; i++) {
            pole[i] = (Pole) this.getPole()[i].clone();
        }
        mapa.setPole(pole);

        return mapa;
    }
}