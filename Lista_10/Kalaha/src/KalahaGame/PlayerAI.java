package KalahaGame;

import AlgorithmsAI.DesiciveTree;
import AlgorithmsAI.MiniMaxTree;
import Constants.Constants;
import java.util.ArrayList;
import java.util.Random;

public class PlayerAI implements KalahaPlayer{

    private Kalaha kalahaGame;
    private Random random = new Random();
    private boolean whichPlayer;

    public PlayerAI(){};
    public PlayerAI(boolean whichPlayer)
    {
        this.whichPlayer = whichPlayer;
    }

    private int makeBestMove(Kalaha currentState)
    {
        whichPlayer = currentState.isWhichPlayerTurn();
        kalahaGame = currentState.copyKalaha();

        int bestScore = -9999;
        int move = random.nextInt(currentState.getSizeOfPlayerBoard());

        for(int i=0; i < currentState.getSizeOfPlayerBoard(); i++)
        {
            int help = kalahaGame.makeMove(i);
            if(help != Constants.ERROR) {

            int currentScore;

            if (whichPlayer == Constants.FIRST_PLAYER_TURN) // obliczenia czy ruch ten jest lepszy od ostatniego najlepszego
            {
                currentScore = kalahaGame.getFirstPlayerPoints() - kalahaGame.getSecondPlayerPoints();
                if (currentScore > bestScore) {
                    move = i;
                    bestScore = currentScore;
                }
            } else {
                currentScore = kalahaGame.getSecondPlayerPoints() - kalahaGame.getFirstPlayerPoints();
                if (currentScore > bestScore) {
                    move = i;
                    bestScore = currentScore;
                }
            }

            kalahaGame = currentState.copyKalaha(); // ustawienie do aktualnego stanu przed obliczeniem korzysci ruchu
            }
        }
        return move;
    }
    @Override
    public int playerTurn(Kalaha currentState)
    {
        MiniMaxTree miniMaxTree = new MiniMaxTree();
        int depth = 6;
        miniMaxTree.createTree(depth, currentState);

        return miniMaxTree.calculateMove(miniMaxTree.getRoot(), depth);
        //return makeBestMove(currentState);
    }


}
