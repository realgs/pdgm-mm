import DecisionTree.DecisionTree;
import Kalaha.Kalaha;

import java.util.Random;
import java.util.Scanner;

public class Game {
    private void showExampleOfBoard(){
        System.out.println("Example of field numbers:");
        System.out.println("Player 1");
        System.out.println("\t6\t5\t4\t3\t2\t1");
        System.out.println("0");
        System.out.println("\t\t\t\t\t\t\t0");
        System.out.println("\t1\t2\t3\t4\t5\t6");
        System.out.println("\t\t\t\t\t\t\tPlayer 0");
    }

    private int chooseOption() {
        Scanner scanner = new Scanner(System.in);
        int option;
        int wrongOptionsAmount = 0;

        System.out.println("Choose game option:");
        System.out.println(" 1) Computer vs Computer");
        System.out.println(" 2) Computer vs User");
        do {
            System.out.print("Enter option number: ");
            option = scanner.nextInt();
            if (option != 1 && option != 2) {
                System.out.println("Wrong option number: " + option + " nuber has to be 1 or 2");
                wrongOptionsAmount++;
            }
        } while (option != 1 && option != 2 && wrongOptionsAmount < 3);

        if (wrongOptionsAmount == 3) {
            option = -1;
        }

        return option;
    }

    private int userVsComputer() {
        // Random player starts
        int computerPlayer = new Random().nextInt(2); // 0 or 1
        int printPlayer = ((computerPlayer + 1) % 2) + 1;
        Scanner scanner = new Scanner(System.in);

        System.out.println("Your player number is: " + printPlayer);

        try{
            Thread.sleep(2500);
        }catch (InterruptedException ex){
            System.out.println("sleep in user vs computer method");
            ex.printStackTrace();
        }

        int holeNum;
        int whichPlayer = 0;

        Kalaha kalaha = new Kalaha();
        DecisionTree decisionTree;

        while (!kalaha.checkStopCondition()) {
            kalaha.printBoard();

            decisionTree = new DecisionTree(kalaha, computerPlayer);
            printPlayer = whichPlayer + 1;
            System.out.print("Player " + printPlayer + ". ");

            if(computerPlayer == whichPlayer) {
                System.out.println("is entering hole number: ");
            }else{
                System.out.println("please enter hole number: ");
            }

            int result;

            do {
                if (whichPlayer != computerPlayer) {
                    holeNum = scanner.nextInt();
                } else {
                    holeNum = decisionTree.findBestWay();
                    System.out.println(holeNum);
                }

                result = kalaha.move(holeNum, whichPlayer);
                if (result == -1)
                    System.out.println("Please enter hole number again: ");
            } while (result == -1);


            if (result != 1) {
                whichPlayer = (whichPlayer + 1) % 2;
            }
        }

        kalaha.collectRestOfStones();
        kalaha.printBoard();

        return kalaha.whichPlayerWon();
    }

    private int computerVsComputer() {
        Kalaha kalaha = new Kalaha();

        int whichPlayer = 0;
        int holeNum;

        DecisionTree gameEngine;

        while (!kalaha.checkStopCondition()) {
            kalaha.printBoard();

            int printPlayer = whichPlayer + 1;
            System.out.println("Player " + printPlayer + ". is entering hole number: ");
            int result;

            do {
                gameEngine = new DecisionTree(kalaha, whichPlayer);

                holeNum = gameEngine.findBestWay();
                System.out.println(holeNum);

                result = kalaha.move(holeNum, whichPlayer);
                if (result == -1) {
                    System.out.println("Please enter hole number again: ");
                }
            } while (result == -1);


            if (result != 1) {
                whichPlayer = (whichPlayer + 1) % 2;
            }
        }

        kalaha.collectRestOfStones();
        kalaha.printBoard();

        return kalaha.whichPlayerWon();
    }

    private void printWinnerInfo(int whichPlayer){
        System.out.println("Result of game:");
        if (whichPlayer == 0 || whichPlayer == 1) {
            int printPlayer = whichPlayer + 1;
            System.out.println("Player " + printPlayer + " won!");
        } else {
            if (whichPlayer == 2) {
                System.out.println("Draw");
            } else {
                System.out.println("Nobody won");
            }
        }
    }


    public static void main(String[] args) {
        Game game = new Game();
        game.showExampleOfBoard();

        System.out.println("\t--- Kalaha ---");
        int optionNumber = game.chooseOption();

        if (optionNumber != -1) {
            int winPlayer;
            if (optionNumber == 1) {
                winPlayer = game.computerVsComputer();
            } else {
                winPlayer = game.userVsComputer();
            }
            game.printWinnerInfo(winPlayer);
        }
    }
}
