package DecTree;

import Gra.Kalaha;

import java.util.LinkedList;

public class Node {
    private int poczatkowyRuch; // pierwszy wykonany ruch czyli nasz wynik zapytania drzewa
    private int czyjAktualnyRuch;
    private int roznicaPunktow;
    private Kalaha aktualnaKalaha;
    private Node parent; // referencja do rodzica
    private LinkedList<Node> children; // lista dzieci


    public Node(Node parent, int poczatkowyRuch, int czyjAktualnyRuch, int roznicaPunktow, Kalaha aktualnaKalaha) { // konstruktor jednoparametrowy
        this.poczatkowyRuch = poczatkowyRuch;
        this.czyjAktualnyRuch = czyjAktualnyRuch;
        this.roznicaPunktow = roznicaPunktow;
        this.aktualnaKalaha = aktualnaKalaha;
        this.parent = parent;
        this.children = new LinkedList<Node>();
    }

    public Node getParent() {
        return parent;
    }

    public void setParent(Node parent) {
        this.parent = parent;
    }

    public int getDegree() {
        return children.size();
    }

    public Node getChild(int i) {
        return children.get(i);
    }

    /**
     * @return sprawdza czy węzeł jest liściem
     */
    public boolean isLeaf() {
        return children.isEmpty();
    }

    // dodaje do węzła dziecko (inny węzeł)
    public Node addChild(Node child) {
        child.setParent(this);
        children.add(child);
        return child;
    }

    // tworzy i dodaje do węzła dziecko z danymi
    public Node addChild(int poczatkowyRuch, int czyjAktualnyRuch, int roznicaPunktow, Kalaha aktualnaKalaha) {
        Node child = new Node(this, poczatkowyRuch, czyjAktualnyRuch, roznicaPunktow, aktualnaKalaha);
        children.add(child);
        return child;
    }

    // usuwa i-te dziecko węzła
    public Node removeChild(int i) {
        return children.remove(i);
    }

    // usuwa wszystkie dzieci węzła
    public void removeChildren() {
        children.clear();
    }

    // zwraca pierwsze dziecko węzła (z lewej)
    public Node getLeftMostChild() {
        if (children.isEmpty()) return null;
        return children.get(0);
    }

    // zwraca listę dzieci
    public LinkedList<Node> getChildren() {
        if (children.isEmpty()) return null;
        return children;
    }

    // zwraca kolejny element siostrzany węzła
    public Node getRightSibling() {
        if (parent != null) {
            LinkedList<Node> childrenParent = parent.getChildren();
            int pozycja = childrenParent.indexOf(this);
            if (childrenParent.size() > pozycja + 1)
                return childrenParent.get(pozycja + 1);
        }
        return null;
    }

    public int getPoczatkowyRuch() {
        return poczatkowyRuch;
    }

    public void setPoczatkowyRuch(int poczatkowyRuch) {
        this.poczatkowyRuch = poczatkowyRuch;
    }

    public int getCzyjAktualnyRuch() {
        return czyjAktualnyRuch;
    }

    public void setCzyjAktualnyRuch(int czyjRuch) {
        this.czyjAktualnyRuch = czyjRuch;
    }

    public int getRoznicaPunktow() {
        return roznicaPunktow;
    }

    public void setRoznicaPunktow(int roznicaPunktow) {
        this.roznicaPunktow = roznicaPunktow;
    }

    public Kalaha getAktualnaKalaha() {
        return aktualnaKalaha;
    }

    public void setAktualnaKalaha(Kalaha aktualnaKalaha) {
        this.aktualnaKalaha = aktualnaKalaha;
    }

    public void setChildren(LinkedList<Node> children) {
        this.children = children;
    }

    public String toString() {
        return String.valueOf(roznicaPunktow);
    }
}