package Server;

import Constants.Constants;
import GUI.KalahaGUI;
import KalahaGame.Kalaha;
import KalahaGame.KalahaPlayer;
import KalahaGame.PlayerAI;

import java.util.Scanner;

public class GameMonitor {

    public void startGame(Kalaha kalaha, KalahaPlayer kalahaPlayerOne, KalahaPlayer kalahaPlayerTwo)
    {
        int move = 0;
        int nrPlayer = 0;

        KalahaGUI.printGUI(kalaha);

        while(!kalaha.isFinished())
        {
            if(kalaha.isWhichPlayerTurn() == Constants.FIRST_PLAYER_TURN)
            {
                move = kalahaPlayerOne.playerTurn(kalaha);
                kalaha.makeMove(move);
                nrPlayer = 1;
            }
            else
            {
                move = kalahaPlayerTwo.playerTurn(kalaha);
                kalaha.makeMove(move);
                nrPlayer = 2;
            }

            KalahaGUI.printGUI(kalaha);
            System.out.println("Gracz " + nrPlayer + " wykonał ruch " + move);
        }

        System.out.println("KONIEC GRY");
        KalahaGUI.printGUI(kalaha);
    }
    public void startAIvsAIGame(int kalahaBoardSize, int startingValues)
    {
        PlayerAI player1 = new PlayerAI(Constants.FIRST_PLAYER_TURN);
        PlayerAI player2 = new PlayerAI(Constants.SECOND_PLAYER_TURN);


        Kalaha kalaha = new Kalaha();
        kalaha.setEverything(kalahaBoardSize, startingValues);

        while(!kalaha.isFinished())
        {
            int move;
            System.out.print("Gracz ");
            if(kalaha.isWhichPlayerTurn() == Constants.FIRST_PLAYER_TURN)
            {
                move = player1.playerTurn(kalaha.copyKalaha());
                kalaha.makeMove(move);
                System.out.print(" 1 ");
            }
            else
            {
                move = player2.playerTurn(kalaha.copyKalaha());
                kalaha.makeMove(move);
                System.out.print(" 2 ");
            }

            System.out.println("Wykonany ruch to: " + move);

            KalahaGUI.printGUI(kalaha);
        }
        System.out.println("KONIEC GRY");
        KalahaGUI.printGUI(kalaha);
    }

    public void startPlayerVsAiGame(int kalahaBoardSize, int startingValues)
    {
        PlayerAI player2 = new PlayerAI(Constants.SECOND_PLAYER_TURN);

        Kalaha kalaha = new Kalaha();
        kalaha.setEverything(kalahaBoardSize, startingValues);
        Scanner sc = new Scanner(System.in);
        int move = 0;
        int nrPlayer = 0;

        KalahaGUI.printGUI(kalaha);

        while(!kalaha.isFinished())
        {
            if(kalaha.isWhichPlayerTurn() == Constants.FIRST_PLAYER_TURN)
            {
                System.out.println("Twój ruch: ");
                move = Integer.parseInt(sc.nextLine());
                kalaha.makeMove(move);
                nrPlayer = 1;
            }
            else
            {
                move = player2.playerTurn(kalaha);
                kalaha.makeMove(move);
                nrPlayer = 2;
            }

            KalahaGUI.printGUI(kalaha);
            System.out.println("Gracz " + nrPlayer + " wykonał ruch " + move);
        }
        System.out.println("KONIEC GRY");
        KalahaGUI.printGUI(kalaha);
    }
}
