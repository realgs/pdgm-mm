package com.company;
import java.util.*;

public class Kalaha_board {

    int index;
    int numberOfStones = 4;
    int[] board = new int[14];

    public Kalaha_board() {
        board[0] = 0;
        board[7] = 0;
        for (index = 1; index < 14; index++) {
            if (index != 7) board[index] = numberOfStones;
        }
    }

    String p1 = "";
    String p2 = "";
    int player1CounterPit = 7;
    int player2CounterPit = 0;

    Scanner input = new Scanner(System.in);

    Player player1 = new Player(p1, 0, player1CounterPit);
    Player player2 = new Player(p2, 7, player2CounterPit);

    void start() {
        System.out.println("----- Let's start a new game! -----");

        System.out.println("Player 1 write your nickname: ");
        p1 = input.nextLine();
        player1.setName(p1);
        System.out.println("Player 2 write your nickname: ");
        p2 = input.nextLine();
        player2.setName(p2);

        turn(player1);
    }

    private void printBoard() {
        int index;
        System.out.println("");
        for (index = 13; index > 7; index--) {
            System.out.print("\t" + board[index]);
        }
        System.out.print("\n\n" + board[0]);
        System.out.println("\t\t\t\t\t\t\t" + board[7] + "\n");
        for (index = 1; index < 7; index++) {
            System.out.print("\t" + board[index]);
        }
        System.out.println("\n");
    }

    private boolean makeMove(Player player, int pit, int counterPit) {
        int pitToSkip;  //opponent's mankala
        if (counterPit == 0) {
            pitToSkip = 7;
        } else pitToSkip = 0;

        int lastPit = board[pit] + pit;
        int amount = board[pit];
        board[pit] = 0;
        for (int i = pit + 1; amount > 0; i++) {
            if (i % 14 == pitToSkip) {
                lastPit++;
            }   // if the pit is opponent's mankala I skip this pit
            else {
                board[i % 14]++;
                amount--;
            }
        }
        int numberPit = lastPit % 14;
        if (numberPit == counterPit) return true; // if the last stone is put in my mankala I get extra turn
        else {
            if (board[numberPit] == 1 && numberPit >= 1 + player.getShift() && numberPit <= 6 + player.getShift()) {
                int temp = board[numberPit] + board[14 - numberPit];
                board[counterPit] += temp;
                board[numberPit] = 0;
                board[14 - numberPit] = 0;
                return false;
            } else return false;
        }
    }


    private void changePlayer(Player player) {   //method which gives turn to another player
        if (player.equals(player1)) turn(player2);
        else turn(player1);
    }


    private void turn(Player player) {
        printBoard();
        int index;

        if (!end()) {
            System.out.println(player.getName() + " \nWhich pit from 1 to 6 do you choose?");

            index = input.nextInt();

            if (index < 1 || index > 6) {
                System.out.println("This is not one of your pits!!!");
                turn(player);
            } else {
                int amount = board[index + player.getShift()];
                if (amount != 0) {

                    if (makeMove(player, index + player.getShift(), player.getPlayerCounter())) {
                        turn(player);
                    } else changePlayer(player);

                } else {
                    System.out.println("You cannot choose empty pit! Choose once again.");
                    turn(player);
                }
                printBoard();
            }
        }else endOfTheGame();
    }

    private boolean end() {
        int temp1 = 0;
        int temp2 = 0;
        for (int i = 1; i <= 6; i++) {
            temp1 += board[i];
        }
        for (int i = 8; i <= 13; i++) {
            temp2 += board[i];
        }
        if (temp1 == 0) {
            board[0] += temp2;
            return true;
        } else if (temp2 == 0) {
            board[7] += temp1;
            return true;
        } else return false;
    }

    private void endOfTheGame(){
        System.out.println("----- End of the game! -----  ");
        System.out.println("Amount of " + player1.getName() + "'s stones: " + board[7]);
        System.out.println("Amount of " + player2.getName() + "'s stones: " + board[0]);
        if(board[player1.getPlayerCounter()] > board[player2.getPlayerCounter()]) {
            System.out.println( player1.getName()  + " won!!!");
            System.exit(0);
        }else if(board[player2.getPlayerCounter()] > board[player1.getPlayerCounter()]){
            System.out.println( player2.getName()  + " won!!!");
            System.exit(0);
        } else System.out.println(" You get a draw!");
        System.exit(0);
    }
}
