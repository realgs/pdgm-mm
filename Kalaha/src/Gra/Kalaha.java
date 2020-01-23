package Gra;
public class Kalaha {
    private Mapa mapa;
    private int aktualnyGracz;
    private int czyKolejnyGracz;

    public Kalaha() {
        this.mapa = new Mapa();
        aktualnyGracz = 2;
        czyKolejnyGracz = 1;
    }

    boolean przesunKamyki(int index) {
        this.czyKolejnyGracz = 1;
        int zasada = mapa.sprawdzZasady(index, this.aktualnyGracz);
        int ostatniIndex = mapa.przesunKamyki(index, this.aktualnyGracz);

        if (zasada == -1) {
            throw new IllegalArgumentException("Puste pole");
        }
        if (zasada == 0) {
            return true;
        }
        if (zasada == 1) {
            this.czyKolejnyGracz = 0;
            return false;
        }
        if (zasada == 2) {
            wykonajZasade2(ostatniIndex);
        }
        return true;
    }

    public boolean wykonajRuchGracz(int zPola, int jakiGracz) {
        this.aktualnyGracz = jakiGracz;
        if (this.aktualnyGracz == 1) {
            if (zPola > 6 || zPola < 1) {
                throw new IllegalArgumentException("Zle wybrane pole ruchu Gracz 1");
            } else {
                return przesunKamyki((zPola - 1));
            }
        } else if (this.aktualnyGracz == 2) {
            if (zPola > 6 || zPola < 1) {
                throw new IllegalArgumentException("Zle wybrane pole ruchu Gracz 1");
            } else {
                return przesunKamyki((zPola - 1) + (mapa.getPole().length / 2));
            }
        } else {
            throw new IllegalArgumentException("Zle podany gracz do ruchu");
        }
    }

    public int ilePunktowGracz(int jakiGracz) {
        if (this.sprawdzCzyKoniec()) {
            this.zsumujKamyki();
        }
        if (jakiGracz == 1) {
            return mapa.getPole()[6].getIloscKamykow();
        } else if (jakiGracz == 2) {
            return mapa.getPole()[13].getIloscKamykow();
        } else {
            throw new IllegalArgumentException("Zle podany gracz do odczytu punktow");
        }
    }

    public int czyjNastepnyRuch() {
        if (this.czyKolejnyGracz == 1) {
            if (aktualnyGracz == 1) return 2;
            if (aktualnyGracz == 2) return 1;
        } else {
            if (aktualnyGracz == 1) return 1;
            if (aktualnyGracz == 2) return 2;
        }
        return 0;
    }

    public boolean sprawdzCzyKoniec() {
        return mapa.sprawdzCzyKoniec();
    }

    public void pokazMape() {
        mapa.pokazMape();
    }

    public void zsumujKamyki() {
        mapa.zsumujKamyki();
    }

    public void podajWynik() {
        mapa.podajWynik();
    }

    void wykonajZasade2(int ostatniIndex) {
        mapa.wykonajZasade2(ostatniIndex, this.aktualnyGracz);
    }

    public Mapa getMapa() {
        return mapa;
    }

    public void setMapa(Mapa mapa) {
        this.mapa = mapa;
    }

    public int getAktualnyGracz() {
        return aktualnyGracz;
    }

    public void setAktualnyGracz(int aktualnyGracz) {
        this.aktualnyGracz = aktualnyGracz;
    }

    public int getCzyKolejnyGracz() {
        return czyKolejnyGracz;
    }

    public void setCzyKolejnyGracz(int czyKolejnyGracz) {
        this.czyKolejnyGracz = czyKolejnyGracz;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        Kalaha kalaha = null;
        try {
            kalaha = new Kalaha();
        } catch (Exception e) {
            e.printStackTrace();
        }
        Mapa mapa = (Mapa) this.getMapa().clone();
        kalaha.setMapa(mapa);
        kalaha.setAktualnyGracz(this.aktualnyGracz);
        kalaha.setCzyKolejnyGracz(this.czyKolejnyGracz);

        return kalaha;
    }
}