package Gra;

public class Pole {
    private int iloscKamykow;
    private boolean czyKoncowy;
    private int czyjePole;

    /**
     * @param iloscKamykow poczatkowa wartosc 6
     * @param czyKoncowy   zakres [true, false]
     * @param czyjePole    zakres [1,2]
     */
    Pole(int iloscKamykow, boolean czyKoncowy, int czyjePole) {
        this.iloscKamykow = iloscKamykow;
        this.czyKoncowy = czyKoncowy;
        if (!(czyjePole == 1 || czyjePole == 2))
            throw new IllegalArgumentException("Nieprawidlowa wartosc 'czyjKamyk'");
        this.czyjePole = czyjePole;
    }

    public int getIloscKamykow() {
        return this.iloscKamykow;
    }

    public void setIloscKamykow(int iloscKamykow) {
        this.iloscKamykow = iloscKamykow;
    }

    public boolean isCzyKoncowy() {
        return this.czyKoncowy;
    }

    public int getCzyjePole() {
        return this.czyjePole;
    }

    @Override
    public Object clone() {
        Pole pole = null;
        try {
            pole = new Pole(this.iloscKamykow, this.czyKoncowy, this.czyjePole);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return pole;
    }
}
