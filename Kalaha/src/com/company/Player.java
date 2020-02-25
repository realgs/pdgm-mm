package com.company;
import java.util.Scanner;

public class Player {
    int shift;
    int playerCounter;
    String name;

    public Player(String name, int shift, int playerCounter){
        this.name = name;
        this.shift = shift;
        this.playerCounter = playerCounter;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getShift() {
        return shift;
    }

    public int getPlayerCounter() {
        return playerCounter;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Player other = (Player) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (shift != other.shift)
            return false;
        if(playerCounter != other.playerCounter)
            return false;
        return true;
    }
}
