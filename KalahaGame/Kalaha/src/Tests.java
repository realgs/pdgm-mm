import AlgorithmsAI.DesiciveTree;
import AlgorithmsAI.MiniMaxTree;
import Constants.Constants;
import KalahaGame.Kalaha;
import KalahaGame.KalahaPlayer;
import KalahaGame.PlayerAI;
import KalahaGame.PlayerHuman;
import Server.GameMonitor;

public class Tests {
    public static void main(String[] args) throws CloneNotSupportedException {
        GameMonitor gameMonitor = new GameMonitor();

        KalahaPlayer playerFirst = new PlayerAI();
        KalahaPlayer playerSecond = new PlayerAI();

        Kalaha kalaha = new Kalaha();
        kalaha.setEverything(12, 5);


        gameMonitor.startGame(kalaha, playerFirst, playerSecond);

        /*Kalaha kalaha = new Kalaha();
        kalaha.setEverything(12,5);

        MiniMaxTree dt = new MiniMaxTree();

        dt.createTree(5, kalaha);

        System.out.println("dONE");*/
    }
}
