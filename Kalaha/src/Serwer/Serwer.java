package Serwer;

import DecTree.SmallerTreeThread;
import DecTree.*;
import Gra.Kalaha;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Serwer {
    private Kalaha kalaha;

    public Serwer() {
        this.kalaha = new Kalaha();
    }

    /**
     * Metoda tworząca drzewo decyzyjne oraz rozdzielająca jego tworzenie na wątki
     * @return ruch jaki trzeba wykonać aby dojść do największej znalezionej wartości wyniku dla aktualnego gracza
     */
    public int znajdzNajlepszyRuch() {
        Tree tree = new Tree(this.kalaha);
        int liczbaDzieci = tree.getRoot().getDegree();

        ExecutorService executorService = Executors.newFixedThreadPool(liczbaDzieci);

        for (int i = 0; i < liczbaDzieci; i++) {
            Runnable worker = new SmallerTreeThread(tree, i);
            executorService.execute(worker);
        }

        executorService.shutdown();
        while(!executorService.isTerminated());
        /*try {
            executorService.awaitTermination(1, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            System.out.println("Znajdowanie najlepszego kroku zostalo przerwane - czas przekroczony.");
        }*/

        int pole = tree.znajdowanieNajlepszegoRuchuWDrzewie();
        return pole;
    }

    public boolean wykonajRuchGracz(int zPola, int jakiGracz) {
        return kalaha.wykonajRuchGracz(zPola, jakiGracz);
    }

    public boolean sprawdzCzyKoniec() {
        return kalaha.sprawdzCzyKoniec();
    }

    public void zsumujKamyki() {
        kalaha.zsumujKamyki();
    }

    public void pokazMape() {
        kalaha.pokazMape();
    }

    public void podajWynik() {
        kalaha.podajWynik();
    }
}
