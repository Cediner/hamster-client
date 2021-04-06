package hamster.ui.food.sort;

public enum Direction {
    ASC, DESC;

    public Direction reverse() {
        if(this == ASC)
            return DESC;
        else
            return ASC;
    }
}
