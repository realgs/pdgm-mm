package KalahaGame;

import Constants.Constants;

import java.util.Scanner;

public class PlayerHuman implements KalahaPlayer{
    @Override
    public int playerTurn(Kalaha currentState) {
        int move = -1;
            System.out.println("Twój ruch, wprowadź cyfrę od" + " (0, " + (currentState.getSizeOfPlayerBoard()-1) + "): ");
            Scanner sc = new Scanner(System.in);
            move = sc.nextInt();
            if(currentState.checkIfMoveIsValid(move) == Constants.ERROR) playerTurn(currentState);
        return move;
    }
}
