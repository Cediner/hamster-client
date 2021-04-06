package hamster.ui.food.filters;

import hamster.data.food.FoodData;

public class NameFilter implements Filter {
    public enum Op {
        INCLUDE, EXCLUDE
    }

    private final String name;
    private final Op op;

    public NameFilter(final String name, final Op op) {
        this.name = name.toLowerCase();
        this.op = op;
    }

    @Override
    public boolean included(FoodData item) {
        if(op == Op.INCLUDE) {
            return item.itemName.toLowerCase().contains(name);
        } else { //Exclude
            return !item.itemName.toLowerCase().contains(name);
        }
    }
}
