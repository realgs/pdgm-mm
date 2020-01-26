package GUI;

import KalahaGame.Kalaha;

public class KalahaGUI {
    public static void printGUI(Kalaha kalaha)
    {
        System.out.println("");

        for (int i=0 ; i<kalaha.getSizeOfPlayerBoard(); i++)
        {
            System.out.print("//////");
        }
        System.out.println("");

        System.out.print("   ");

        //Second player board
        for(int i = kalaha.getSecondScorePotOffset() - 1; i > kalaha.getFirstScorePotOffset(); i--)
        {
            System.out.print("[" + kalaha.getBoard()[i] + "]  ");
        }

        System.out.println("");

        //ScorePits
        for(int i = kalaha.getSecondScorePotOffset(); i >= kalaha.getFirstScorePotOffset(); i--)
        {
            if(kalaha.isItPointPit(i)) System.out.print("[" + kalaha.getBoard()[i] + "]");
            else System.out.print("     ");
        }

        System.out.println("");
        System.out.print("   ");

        //First player board
        for( int i = 0; i < kalaha.getSizeOfPlayerBoard(); i++)
        {
            System.out.print("[" + kalaha.getBoard()[i] + "]  ");
        }

        System.out.println("");

        for (int i=0 ; i<kalaha.getSizeOfPlayerBoard(); i++)
        {
            System.out.print("//////");
        }

        System.out.println("");

    }
}



//
//      0    0   0   0   0
//  0                        0
//      0    0   0   0   0
//