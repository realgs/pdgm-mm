package AlgorithmsAI;

import Constants.Constants;
import KalahaGame.Kalaha;

import java.lang.reflect.Array;
import java.util.ArrayList;

import static java.lang.Integer.min;
import static sun.swing.MenuItemLayoutHelper.max;

public class MiniMaxTree {

    private Node root;

    public MiniMaxTree() {}

    public void createTree(int depth, Kalaha startingState)
    {
        ArrayList<Node> children = new ArrayList<>();

        root = new Node(0,0, startingState, children);

        fillTree(root, depth, startingState.isWhichPlayerTurn());
    }

    private void fillTree(Node current, int depth, boolean whichPlayer)
    {
        if(depth < 0) return;

        for(int i=0; i < current.getCurrentState().getSizeOfPlayerBoard(); i++)
        {
            ArrayList<Node> children = new ArrayList<>();
            if(current.getCurrentState().checkIfMoveIsValid(i) != Constants.ERROR)
            {
                Kalaha copy = current.getCurrentState().copyKalaha();
                copy.makeMove(i);
                Node child;

                if(copy.isWhichPlayerTurn() == current.currentState.isWhichPlayerTurn())
                {
                    boolean madeMove = false;
                    for(int j=0; j < copy.getSizeOfPlayerBoard(); j++)
                    {
                        if(copy.checkIfMoveIsValid(j) != Constants.ERROR && !madeMove)
                        {
                            copy.makeMove(j);
                            madeMove = true;
                        }
                    }
                }

                int score = Integer.MIN_VALUE;

                if(whichPlayer == Constants.FIRST_PLAYER_TURN) score = copy.getFirstPlayerPoints() - copy.getSecondPlayerPoints();
                else score = copy.getSecondPlayerPoints() - copy.getFirstPlayerPoints();

                child = new Node(copy.getFirstPlayerPoints() - copy.getSecondPlayerPoints(), i , copy, children);

                current.getChildren().add(child);

                fillTree(child, depth-1, whichPlayer);
            }
        }
    }

    public int calculateMove(Node node, int depth)
    {
        int best = Integer.MIN_VALUE;
        int move = 0;
        for (Node child: node.children) {
            int current = minimax(child, depth, true);
            if(best <= current)
            {
                move = child.move;
                best = current;
            }
        }
        return move;
    }

    private int minimax(Node node, int depth,  boolean max)
    {
        if(depth == 0) return node.score;

        if(max)
        {
            int maxEval = Integer.MIN_VALUE;
            for (Node child : node.children) {
                int eval = minimax(child, depth-1, !max);
                maxEval = max(maxEval, eval);
            }
            return maxEval;
        }
        else
        {
            int minEval = Integer.MAX_VALUE;
            for (Node child : node.children) {
                int eval = minimax(child, depth-1, !max);
                minEval = min(minEval, eval);
            }
            return minEval;
        }
    }

    public Node getRoot() {
        return root;
    }

    public void setRoot(Node root) {
        this.root = root;
    }

    private int getIndexOfLargest(ArrayList<Node> array )
    {
        if ( array == null || array.size() == 0 ) return 0; // null or empty

        int largest = 0;
        for ( int i = 1; i < array.size(); i++ )
        {
            if ( array.get(i).score > array.get(largest ).score ) largest = i;
        }
        return largest; // position of the first largest found
    }

    private int getIndexOfSmallest(ArrayList<Node> array )
    {
        if ( array == null || array.size() == 0 ) return 0; // null or empty

        int smallest = 0;
        for ( int i = 1; i < array.size(); i++ )
        {
            if ( array.get(i).score < array.get(smallest ).score ) smallest = i;
        }
        return smallest; // position of the first largest found
    }

    class Node {
        private int score;
        private int move;
        private Kalaha currentState;
        private ArrayList<Node> children;

        public Node(int score, int move, Kalaha currentState, ArrayList<Node> children) {
            this.score = score;
            this.move = move;
            this.currentState = currentState;
            this.children = children;
        }

        public int getScore() {
            return score;
        }

        public Kalaha getCurrentState() {
            return currentState;
        }

        public void setCurrentState(Kalaha currentState) {
            this.currentState = currentState;
        }

        public void setScore(int score) {
            this.score = score;
        }

        public int getMove() {
            return move;
        }

        public void setMove(int move) {
            this.move = move;
        }

        public ArrayList<Node> getChildren() {
            return children;
        }

        public void setChildren(ArrayList<Node> children) {
            this.children = children;
        }


    }
}
