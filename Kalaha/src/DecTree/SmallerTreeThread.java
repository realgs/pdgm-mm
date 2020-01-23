package DecTree;
import DecTree.Tree;

public class SmallerTreeThread implements Runnable {
    private Tree tree;
    private int childrenIndex;

    public SmallerTreeThread(Tree tree, int childrenIndex) {
        this.tree = tree;
        this.childrenIndex = childrenIndex;
    }

    @Override
    public void run() {
        for (int i = 0; i < 7; i++) {
            tree.stworzNowyRzad(tree.getRoot().getChild(childrenIndex));
        }
    }
}
