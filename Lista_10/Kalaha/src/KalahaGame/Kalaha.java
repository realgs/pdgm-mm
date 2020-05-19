package KalahaGame;

import Constants.Constants;
import com.sun.xml.internal.ws.wsdl.writer.document.http.Address;

import java.util.ArrayList;

public class Kalaha{
    private int kalahaBoardSize = 12;
    private int startingValues = 5;
    private int[] board = new int[kalahaBoardSize];
    private int sizeOfPlayerBoard = (kalahaBoardSize - 2) / 2;
    private int secondScorePotOffset = kalahaBoardSize - 1;
    private int firstScorePotOffset = sizeOfPlayerBoard;
    private boolean whichPlayerTurn = Constants.FIRST_PLAYER_TURN;
    // Pierwszy gracz ma dołki od 0 do 4
    // Drugi gracz ma dolki od 6 do 10
    // Zakladajac ze gramy z 5 dolkami po kazdej stronie

    public Kalaha(){}

    public int setEverything(int kalahaBoardSize, int startingValues)
    {
        board = null;
        board = new int[kalahaBoardSize];
        for(int i=0; i < kalahaBoardSize; i++)
        {
            if(isItPointPit(i) == false)
                board[i] = startingValues;
            else board[i] = 0;
        }

        return Constants.NO_ERROR;
    }

    // sprawdzenie czy offset to jedna z dziur z punktami
    public boolean isItPointPit(int offset)
    {
        return (secondScorePotOffset == offset|| firstScorePotOffset == offset);
    }

    public int makeMove(int offSet)
    {
        if(checkIfMoveIsValid(offSet) == Constants.ERROR) return Constants.ERROR;

        if (whichPlayerTurn == Constants.SECOND_PLAYER_TURN) {
            offSet += this.sizeOfPlayerBoard+1;
        }

        int rocks = board[potNr(offSet)]; // pobranie liczby kamieni z dołka
        board[potNr(offSet)] = 0; // dziura z której zabieramy jest zerowana

        disposeRocks(rocks, offSet + 1); // przyporządkowanie kamieni

        return Constants.NO_ERROR;
    }
    public int checkIfMoveIsValid(int offSet)
    {
        if(offSet < 0 || offSet > this.sizeOfPlayerBoard) return Constants.ERROR; // sprawdzenie czy nie błedne

        if(whichPlayerTurn == Constants.FIRST_PLAYER_TURN)
            offSet = offSet; // offset pozostaje taki sam
        else offSet = offSet + this.sizeOfPlayerBoard + 1; // Ustawia move na planszy drugiego gracza

        if(isFinished()) return finishGame();

        if(board[offSet] == 0 || isItPointPit(offSet)) return Constants.ERROR; // kolejne sprawdzenie

        return Constants.NO_ERROR;
    }
    private int finishGame() {
        int sum = 0;

        if(whichPlayerTurn == Constants.SECOND_PLAYER_TURN)
        {
            for(int i=0; i < this.sizeOfPlayerBoard; i++)
            {
                sum += board[i];
                board[i] = 0;
            }
            board[firstScorePotOffset] += sum;
        } else
        {
            for(int i = this.sizeOfPlayerBoard + 1; i < this.kalahaBoardSize - 1; i++)
            {
                sum += board[i];
                board[i] = 0;
            }
            board[secondScorePotOffset] += sum;
        }

        return Constants.NO_ERROR;
    }
    public int checkWhoWon()
    {
        if( getFirstPlayerPoints() < getSecondScorePotOffset() ) return Constants.SECOND_PLAYER_WON;
        else if( getFirstPlayerPoints() == getSecondScorePotOffset() ) return Constants.DRAW;
        else return Constants.FIRST_PLAYER_WON;
    }
    // funkcja do przyporządkowania kamieni po ruchu
    private int disposeRocks(int rocks, int offSet) {
        while(rocks > 0)
        {
            int i = potNr(offSet);
            if((whichPlayerTurn == Constants.FIRST_PLAYER_TURN && i == secondScorePotOffset) // kamyk nie moze wpasc do dolka z punktami przeciwnego gracza
                    ||
                    (whichPlayerTurn == Constants.SECOND_PLAYER_TURN && i == firstScorePotOffset))
            {
                offSet++; //Inkrementacja offestu, ominiecie dziury z punktami
            }
            else {
                int rocksInHole = board[i];
                board[i] = rocksInHole + 1; // wsadzenie kamienia do dziury

                rocks--; // Zmniejszamy liczbę kamieni, ponieważ wsadzilsmy jeden do dziury
                offSet++; //Inkrementacja offseta zeby rozdysponowac kamyki
            }
        } // while

        offSet--; // Dekrementacja zeby miec ustawiony ostatnią wsadzoną dziure

        if(whichPlayerTurn == Constants.FIRST_PLAYER_TURN) // Sprawdzenie dla pierwszego gracza
        {
            if(potNr(offSet) == firstScorePotOffset) // jesli ostatni kamyk wlecial do scora gracza wykonujacego ruch, powtarza on ruch
            {
                whichPlayerTurn = whichPlayerTurn; // tura pozostaje tego samego gracza
            }
            else if(potNr(offSet) < this.sizeOfPlayerBoard && board[potNr(offSet)] == 1) //oprogramowanie zbicia
            {
                int otherSidePitHole = kalahaBoardSize - 2 - potNr(offSet);
                int temp = board[otherSidePitHole];
                board[otherSidePitHole] = 0;
                board[firstScorePotOffset] += temp;
                board[firstScorePotOffset] += board[potNr(offSet)];
                board[potNr(offSet)] = 0;
                whichPlayerTurn = !whichPlayerTurn; // zmiana tury gracza
            }
            else {
                whichPlayerTurn = !whichPlayerTurn; // zmiana tury gracza
            }
        }
        else //sprawdzenie dla drugiego
        {
            if(potNr(offSet) == secondScorePotOffset) // jesli ostatni kamyk wlecial do scora gracza wykonujacego ruch, powtarza on ruch
            {
                whichPlayerTurn = whichPlayerTurn; //tura pozostaje tego samego gracza
            }
            else if(potNr(offSet) > this.sizeOfPlayerBoard && board[potNr(offSet)] == 1) //oprogramowanie zbicia dla drugiego gracza
            {
                int otherSidePitHole = kalahaBoardSize - 2 - potNr(offSet);
                int temp = board[otherSidePitHole];
                board[otherSidePitHole] = 0;
                board[secondScorePotOffset] += temp;
                board[secondScorePotOffset] += board[potNr(offSet)];
                board[potNr(offSet)] = 0;
                whichPlayerTurn = !whichPlayerTurn; // zmiana tury gracza
            }
            else {
                whichPlayerTurn = !whichPlayerTurn; // zmiana tury gracza
            }
        }
        return potNr(offSet);
    }

    public boolean isFinished()
    {
        boolean isFinish = true;

        if(whichPlayerTurn == Constants.FIRST_PLAYER_TURN)
        {
            for(int i=0; i < this.sizeOfPlayerBoard; i++)
            {
                if(board[i] != 0) isFinish = false;
            }
        } else
        {
            for(int i = this.sizeOfPlayerBoard + 1; i < this.kalahaBoardSize - 1; i++)
            {
                if(board[i] != 0) isFinish = false;
            }
        }


        if(isFinish)
        {
            finishGame();
        }

        return isFinish;
    }

    private int potNr(int offset)
    {
        return offset % kalahaBoardSize;
    }

    public Kalaha copyKalaha()
    {
        Kalaha newKalaha = new Kalaha();

        newKalaha.startingValues = this.startingValues;
        newKalaha.board = new int[this.kalahaBoardSize];
        for(int i = 0; i < this.kalahaBoardSize; i++)
        {
            newKalaha.board[i] = this.board[i];
        }
        newKalaha.whichPlayerTurn = this.whichPlayerTurn;
        newKalaha.secondScorePotOffset = this.secondScorePotOffset;
        newKalaha.firstScorePotOffset = this.firstScorePotOffset;
        newKalaha.sizeOfPlayerBoard = this.sizeOfPlayerBoard;
        newKalaha.kalahaBoardSize = this.kalahaBoardSize;

        return newKalaha;
    }
    public int getKalahaBoardSize() {
        return kalahaBoardSize;
    }

    public int getStartingValues() {
        return startingValues;
    }

    public void setStartingValues(int startingValues) {
        this.startingValues = startingValues;
    }

    public int[] getBoard() {
        return board;
    }

    public void setBoard(int[] board) {
        this.board = board;
    }

    public int getSizeOfPlayerBoard() {
        return sizeOfPlayerBoard;
    }

    public void setSizeOfPlayerBoard(int sizeOfPlayerBoard) {
        this.sizeOfPlayerBoard = sizeOfPlayerBoard;
    }
    public void setKalahaBoardSize(int kalahaBoardSize) {
        this.kalahaBoardSize = kalahaBoardSize;
    }

    public int getSecondScorePotOffset() {
        return secondScorePotOffset;
    }

    public void setSecondScorePotOffset(int secondScorePotOffset) {
        this.secondScorePotOffset = secondScorePotOffset;
    }

    public int getFirstScorePotOffset() {
        return firstScorePotOffset;
    }

    public void setFirstScorePotOffset(int firstScorePotOffset) {
        this.firstScorePotOffset = firstScorePotOffset;
    }

    public int getFirstPlayerPoints() {return board[getFirstScorePotOffset()];}
    public int getSecondPlayerPoints() {return board[getSecondScorePotOffset()];}

    public boolean isWhichPlayerTurn() {
        return whichPlayerTurn;
    }

    public void setWhichPlayerTurn(boolean whichPlayerTurn) {
        this.whichPlayerTurn = whichPlayerTurn;
    }
}
