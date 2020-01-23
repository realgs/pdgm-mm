package Kalaha;

public class Kalaha {
    private Board board;

    public Kalaha(){
        board = new Board();
        board.initBoard();
    }

    public Kalaha(Kalaha kalaha){
        this.board = new Board(kalaha.board);
    }

    public int[] getBoard(){
        return board.getBoard();
    }

    public void printBoard(){
        board.printBoard();
    }

    public boolean checkStopCondition(){
        return board.checkStopCondition();
    }

    public void collectRestOfStones(){
        board.collectRestOfStones();
    }

    /**
     * @param holeNum     values [1,6]          if not returns true
     * @param whichPlayer values 0 or 1         if not returns true
     * @return false if hole has stones*/
    public boolean holeIsEmpty(int holeNum, int whichPlayer){
        return board.holeIsEmpty(holeNum, whichPlayer);
    }

    /**
     *  @param whichPlayer needs to be 0 or 1
     *
     *  @return -1 when there is wrong param value
     *  @return stones amount in opponent base*/
    public int getStonesAmount(int whichPlayer){
        if(whichPlayer != 0 && whichPlayer != 1){
            return -1;
        }

        if(whichPlayer == 0){
            return board.getBoard()[(board.getBoard().length / 2) - 1]; // 6
        }
        return board.getBoard()[board.getBoard().length - 1]; // 13
    }

    /**
     *  @param whichPlayer needs to be 0 or 1
     *
     *  @return -1 when there is wrong param value
     *  @return stones amount in opponent base*/
    public int getOpponentStonesAmount(int whichPlayer) {
        if (whichPlayer != 0 && whichPlayer != 1) {
            return -1;
        }

        return getStonesAmount((whichPlayer + 1) % 2);
    }

    /**
     * @param holeNum     needs to be [1, 6]
     * @param whichPlayer needs to be 0 or 1
     *
     * @return -1 when there is wrong param value
     * @return 1  when player got additional move
     * @return 0  otherwise */
    public int move(int holeNum, int whichPlayer){
        int condition = board.checkLastHoleConditions(holeNum, whichPlayer);

        if(condition < 0){
            /*System.out.print("Wrong argument -");
            if(condition == -1){
                System.out.println(" number of hole.");
            }else{
                if(condition == -2) {
                    System.out.println(" player number.");
                }else{
                    System.out.println(" hole is empty.");
                }
            }*/

            return -1;
        }

        int result = board.moveStones(holeNum, whichPlayer);
        if(condition == 2){
            board.emptyHole(result);
        } else {
            if(condition == 1){
                return 1;
            }
        }

        return 0;
    }

    /**
     * @return -1 when nobody won yet
     * @return 0  when first player won
     * @return 1  when second player won
     * @return 2  when draw */
    public int whichPlayerWon(){
        return board.whichPlayerWon();
    }
}
