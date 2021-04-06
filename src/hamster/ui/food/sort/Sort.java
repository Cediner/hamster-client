package hamster.ui.food.sort;

import hamster.data.food.FoodData;

import java.util.Comparator;

public abstract class Sort implements Comparator<FoodData> {
    private final SortMethod method;

    public Sort(final SortMethod method) {
        this.method = method;
    }

    public SortMethod method() { return method; }

    public abstract Sort reverse();
}
