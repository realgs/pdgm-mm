package DecTree;

import Gra.Kalaha;

import java.util.Random;

public class Tree {
    private Node root; // referencja do korzenia
    private int czyjRuch;
    private int przeciwnik;
    final double PROG_ZMIANY = 0.7;
    private int najwiekszaWartosc = Integer.MIN_VALUE;
    private int pierwszyRuchNajwiekszejWartosci = 0;

    public Node getRoot() {
        return root;
    }

    public Tree(Kalaha kalaha) {
        this.root = new Node(null, -1, -1, 0, kalaha);
        this.czyjRuch = root.getAktualnaKalaha().czyjNastepnyRuch();
        if (czyjRuch == 1) this.przeciwnik = 2;
        else this.przeciwnik = 1;
        stworzPierwszyRzad();

    }

    /**
     * Metoda tworząca pierwszy rząd drzewa, wszystkie aktualnie możliwe ruchy. Jeden z nich zostanie wybrany jako najkorzystniejszy i wykonany
     */
    public void stworzPierwszyRzad() {
        for (int i = 1; i < 7; i++) {
            if (root.getAktualnaKalaha().czyjNastepnyRuch() == 1 && root.getAktualnaKalaha().getMapa().getPole()[i - 1].getIloscKamykow() <= 0)
                ;
            else if (root.getAktualnaKalaha().czyjNastepnyRuch() == 2 && root.getAktualnaKalaha().getMapa().getPole()[(i - 1) + (root.getAktualnaKalaha().getMapa().getPole().length / 2)].getIloscKamykow() <= 0)
                ;
            else {
                Kalaha kalahaPoRuchu = null;
                try {
                    kalahaPoRuchu = (Kalaha) (root.getAktualnaKalaha()).clone();
                } catch (CloneNotSupportedException e) {
                    e.printStackTrace();
                }
                kalahaPoRuchu.wykonajRuchGracz(i, root.getAktualnaKalaha().czyjNastepnyRuch());

                root.addChild(i, czyjRuch,
                        kalahaPoRuchu.ilePunktowGracz(this.czyjRuch) - kalahaPoRuchu.ilePunktowGracz(this.przeciwnik),
                        kalahaPoRuchu);
            }
        }
    }

    /**
     * Tworzenie nowego rzędu drzewa decyzyjnego
     * @param node początkowy wezel, od ktorego rozpoczynamy tworzenie
     */
    public void stworzNowyRzad(Node node) {
        inOrderTworzacyNoweDzieci(node);
    }

    /**
     * Metoda przechodząca po drzewie "inorder" i wywolujaca metode tworzaca nowe kroki dla gracza
     * @param node
     */
    public void inOrderTworzacyNoweDzieci(Node node) {
        if (node.isLeaf()) {
            stworzNoweKroki(node);
        } else {
            Node temp = node.getLeftMostChild();
            inOrderTworzacyNoweDzieci(temp);
            temp = temp.getRightSibling();
            while (temp != null) {
                inOrderTworzacyNoweDzieci(temp);
                temp = temp.getRightSibling();
            }
        }
    }

    /**
     * Metoda tworząca kolejny krok dla danego gracza
     * @param lisc znaleziony lisc, wezel bez dzieci
     */
    public void stworzNoweKroki(Node lisc) {
        //System.out.println("kolejny rozwoj liscia");
        for (int i = 1; i < 7; i++) {
            //System.out.println("kolejne dziecko " + i);
            if (lisc.getAktualnaKalaha().czyjNastepnyRuch() == 1 && lisc.getAktualnaKalaha().getMapa().getPole()[i - 1].getIloscKamykow() <= 0)
                ;
            else if (lisc.getAktualnaKalaha().czyjNastepnyRuch() == 2 && lisc.getAktualnaKalaha().getMapa().getPole()[(i - 1) + (lisc.getAktualnaKalaha().getMapa().getPole().length / 2)].getIloscKamykow() <= 0)
                ;
            else {
                Kalaha kalahaPoRuchu = null;
                try {
                    kalahaPoRuchu = (Kalaha) (lisc.getAktualnaKalaha()).clone();
                } catch (CloneNotSupportedException e) {
                    e.printStackTrace();
                }
                kalahaPoRuchu.wykonajRuchGracz(i, lisc.getAktualnaKalaha().czyjNastepnyRuch());

                lisc.addChild(lisc.getPoczatkowyRuch(), lisc.getAktualnaKalaha().czyjNastepnyRuch(),
                        kalahaPoRuchu.ilePunktowGracz(this.czyjRuch) - kalahaPoRuchu.ilePunktowGracz(this.przeciwnik),
                        kalahaPoRuchu);
            }
        }
    }


    public int znajdowanieNajlepszegoRuchuWDrzewie() {
        najwiekszaWartosc = Integer.MIN_VALUE;
        pierwszyRuchNajwiekszejWartosci = 0;
        znajdowanieNajlepszegoRuchuWDrzewie(this.root);
        return this.pierwszyRuchNajwiekszejWartosci;
    }

    public void znajdowanieNajlepszegoRuchuWDrzewie(Node n) {
        if (n.isLeaf()) {
            //System.out.print(n + " ");
            int aktualny = n.getRoznicaPunktow();
            int aktualnyPierwszyRuchNajwiekszejWartosci = n.getPoczatkowyRuch();
            if (aktualny > this.najwiekszaWartosc) {
                this.najwiekszaWartosc = aktualny;
                this.pierwszyRuchNajwiekszejWartosci = aktualnyPierwszyRuchNajwiekszejWartosci;
            }
            if (aktualny == this.najwiekszaWartosc) {
                Random rand = new Random();
                double liczba_rand = rand.nextDouble();
                if (liczba_rand > PROG_ZMIANY) {
                    this.najwiekszaWartosc = aktualny;
                    this.pierwszyRuchNajwiekszejWartosci = aktualnyPierwszyRuchNajwiekszejWartosci;
                }
            }
        } else {
            Node temp = n.getLeftMostChild();
            znajdowanieNajlepszegoRuchuWDrzewie(temp);
            //System.out.print(n + " ");
            temp = temp.getRightSibling();
            while (temp != null) {
                znajdowanieNajlepszegoRuchuWDrzewie(temp);
                temp = temp.getRightSibling();
            }
        }
    }

}