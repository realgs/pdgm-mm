package DecisionTree;

public class BranchThread implements Runnable {
    private Node root;
    private int childIndex;

    BranchThread(Node root, int childIndex){
        this.root = root;
        this.childIndex = childIndex;
    }

    @Override
    public void run() {
        root.createBranchToLeaf(root.getChildren().get(childIndex));
    }
}
