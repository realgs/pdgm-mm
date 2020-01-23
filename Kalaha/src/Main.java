import Gra.*;
import DecTree.*;
import Serwer.Serwer;

import java.util.Scanner;

public class Main {

    static boolean komputer(Serwer serwer, boolean czyZakonczono, int jakiGracz) {
        boolean czyWykonanyRuch = false;
        while (!czyWykonanyRuch && !czyZakonczono) {
            System.out.println("\nKomputer: ");
            int pole = serwer.znajdzNajlepszyRuch();
            System.out.println("Wykonuję ruch z pola: " + pole);
            czyWykonanyRuch = serwer.wykonajRuchGracz(pole, jakiGracz);
            czyZakonczono = serwer.sprawdzCzyKoniec();
            if (czyZakonczono) {
                serwer.zsumujKamyki();
                break;
            }
            serwer.pokazMape();
        }
        return czyZakonczono;
    }


    static boolean gracz(Serwer serwer, boolean czyZakonczono, int jakiGracz) {
        Scanner scanner = new Scanner(System.in);
        boolean czyWykonanyRuch = false;
        while (!czyWykonanyRuch && !czyZakonczono) {
            System.out.println("\nGracz " + jakiGracz + ": ");
            System.out.print("Podaj ruch: ");
            int scan = 0;
            boolean poprawne = false;
            while(!poprawne){
                 scan = scanner.nextInt();
                 if(scan > 0 && scan < 7) poprawne = true;
                 else {
                     System.out.print("Zly numer pola! Podaj ponownie: ");
                 }
            }
            czyWykonanyRuch = serwer.wykonajRuchGracz(scan, jakiGracz);
            czyZakonczono = serwer.sprawdzCzyKoniec();
            if (czyZakonczono) {
                serwer.zsumujKamyki();
                break;
            }
            serwer.pokazMape();
        }
        return czyZakonczono;
    }

    static void komputerVSgracz() {
        Serwer serwer = new Serwer();
        System.out.println("Poczatkowa mapa:");
        serwer.pokazMape();

        boolean czyZakonczono = false;
        while (!czyZakonczono) {
            System.out.println();
            System.out.println();
            czyZakonczono = komputer(serwer, czyZakonczono, 1);
            if (czyZakonczono) break;
            System.out.println();
            System.out.println();
            czyZakonczono = gracz(serwer, czyZakonczono, 2);
        }
        serwer.podajWynik();
    }

    static void graczVSkomputer() {
        Serwer serwer = new Serwer();
        System.out.println("Poczatkowa mapa:");
        serwer.pokazMape();

        boolean czyZakonczono = false;
        while (!czyZakonczono) {
            System.out.println();
            System.out.println();
            czyZakonczono = gracz(serwer, czyZakonczono, 1);
            if (czyZakonczono) break;
            System.out.println();
            System.out.println();
            czyZakonczono = gracz(serwer, czyZakonczono, 2);
        }
        serwer.podajWynik();
    }

    static void komputerVSkomputer() {
        Serwer serwer = new Serwer();
        System.out.println("Poczatkowa mapa:");
        serwer.pokazMape();

        boolean czyZakonczono = false;
        while (!czyZakonczono) {
            System.out.println();
            System.out.println();
            czyZakonczono = komputer(serwer, czyZakonczono, 1);
            if (czyZakonczono) break;
            System.out.println();
            System.out.println();
            czyZakonczono = komputer(serwer, czyZakonczono, 2);
        }
        serwer.podajWynik();
    }

    static void graczVSgracz() {
        Serwer serwer = new Serwer();
        System.out.println("Poczatkowa mapa:");
        serwer.pokazMape();

        boolean czyZakonczono = false;
        while (!czyZakonczono) {
            System.out.println();
            System.out.println();
            czyZakonczono = gracz(serwer, czyZakonczono, 1);
            if (czyZakonczono) break;
            System.out.println();
            System.out.println();
            czyZakonczono = gracz(serwer, czyZakonczono, 2);
        }
        serwer.podajWynik();
    }


    public static void main(String[] args) throws Exception {
        System.out.println("Witam w grze Kalaha");
        System.out.println("Gdy gra poprosi Cie o numer ruchu podaj pole zgodnie z instrukcja:");
        System.out.println("\t\t\tGracz 2");
        System.out.println("\t6\t5\t4\t3\t2\t1");
        System.out.println("B2 \t\t\t\t\t\t\tB1");
        System.out.println("\t1\t2\t3\t4\t5\t6");
        System.out.println("\t\t\tGracz 1");
        System.out.println("Literka 'S' oznaczona jest baza danego gracza, w ktorej zsumuja sie punkty koncowe.");
        System.out.println();
        System.out.println("Wybierz tryb gry:");
        System.out.println("\t 1 - komputer VS gracz");
        System.out.println("\t 2 - gracz vs komputer");
        System.out.println("\t 3 - komputer vs komputer");
        System.out.println("\t 4 - gracz vs gracz");
        Scanner scan = new Scanner(System.in);
        int wybor = scan.nextInt();
        if (wybor == 1) komputerVSgracz();
        else if (wybor == 2) graczVSkomputer();
        else if (wybor == 3) komputerVSkomputer();
        else if (wybor == 4) graczVSgracz();
    }
}
