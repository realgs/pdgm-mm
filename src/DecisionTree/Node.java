package DecisionTree;

import Kalaha.Kalaha;

import java.util.ArrayList;
import java.util.concurrent.*;

class Node {
    private int startHole;
    private int originalPlayer;
    private int whichPlayer;
    private int depth;
    private int collectedStones;
    private int lastResult;
    private ArrayList<Node> children;
    private Kalaha currentKalaha;
    private static final int MAX_DEPTH = 9;

    // root constructor
    Node (Kalaha currentKalaha, int whichPlayer) {
        this.depth = 0;
        this.collectedStones = 0;
        this.startHole = 0;
        this.lastResult = 0;
        this.whichPlayer = whichPlayer;
        this.originalPlayer = whichPlayer;

        this.currentKalaha = currentKalaha;
        children = new ArrayList<>(currentKalaha.getBoard().length);
    }

    private Node (Kalaha currentKalaha, int startHole, int depth, int collectedStones, int whichPlayer, int lastResult) {
         this.depth = depth;
         this.collectedStones = collectedStones;
         this.whichPlayer = whichPlayer;
         this.startHole = startHole;
         this.lastResult = lastResult;

         this.currentKalaha = currentKalaha;
         children = new ArrayList<>(currentKalaha.getBoard().length);
    }

    int getStartHole(){
        return startHole;
    }

    int getCollectedStones() {
        return collectedStones;
    }

    ArrayList<Node> getChildren(){
        return children;
    }

    ArrayList<Node> getAllLeafs(Node node){
        ArrayList<Node> leafs = new ArrayList<>();
        if(node.children.isEmpty()){
            leafs.add(node);
        }else{
            for(Node child: node.children){
                leafs.addAll(getAllLeafs(child));
            }
        }

        return leafs;
    }

    void createBranchToLeaf(Node node) {
        int holesAmount = (currentKalaha.getBoard().length / 2) - 1;
        for (int i = 0; i < holesAmount; i++) {
            Kalaha childKalaha = new Kalaha(node.currentKalaha);

            int player;
            if (node.lastResult == 1) {
                player = node.whichPlayer;
            } else {
                if (node.whichPlayer == 0) {
                    player = 1;
                } else {
                    player = 0;
                }
            }

            int result = childKalaha.move(i + 1, player);

            if (childKalaha.checkStopCondition()) {
                childKalaha.collectRestOfStones();
            }

            node.collectedStones = childKalaha.getStonesAmount(originalPlayer) - childKalaha.getOpponentStonesAmount(originalPlayer);


            if (node.depth < MAX_DEPTH && result != -1) {
                Node child;

                // result = 0 - change player
                if (result == 0) {
                    if (node.whichPlayer == 0) {
                        child = new Node(childKalaha, node.startHole, node.depth + 1, node.collectedStones, 1, result);
                    } else {
                        child = new Node(childKalaha, node.startHole, node.depth + 1, node.collectedStones, 0, result);
                    }
                } else {
                    // result = 1 - the same player
                    child = new Node(childKalaha, node.startHole, node.depth + 1, node.collectedStones, node.whichPlayer, result);
                }

                node.children.add(child);
                createBranchToLeaf(child);
            }
        }
    }

    void createBranches(){
        int holesAmount = (currentKalaha.getBoard().length / 2) - 1;

        //only for root's children
        ExecutorService executorService = Executors.newFixedThreadPool(holesAmount);

            for (int i = 0; i < holesAmount; i++) {
                Runnable worker = new BranchThread(this, i);
                executorService.execute(worker);
            }

            executorService.shutdown();
            while(!executorService.isTerminated());
    }

    /**
     * only for root*/
    void createChildren() {
        int holesAmount = (currentKalaha.getBoard().length / 2) - 1;

        for (int i = 0; i < holesAmount; i++) {
            Kalaha childKalaha = new Kalaha(currentKalaha);
            int result = childKalaha.move(i + 1, whichPlayer);

            int stonesInHole = childKalaha.getStonesAmount(whichPlayer);
            Node child = new Node(childKalaha, i + 1, depth + 1, stonesInHole, whichPlayer, result);
            children.add(child);
        }
    }
}
