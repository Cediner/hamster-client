package hamster.ui.food.filters;

import hamster.data.food.FoodData;
import haven.FastText;
import haven.GOut;

public class NameFilter implements Filter {
    public enum Op {
        Include, Exclude
    }

    private final String name;
    private final Op op;

    public NameFilter(final String name, final Op op) {
        this.name = name.toLowerCase();
        this.op = op;
    }

    @Override
    public boolean included(FoodData item) {
        if(op == Op.Include) {
            return item.itemName.toLowerCase().contains(name);
        } else { //Exclude
            return !item.itemName.toLowerCase().contains(name);
        }
    }

    @Override
    public void render(GOut g) {
        FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Name %s %s", op, name);
    }

    @Override
    public String toString() {
        return String.format("%sname:%s", op  == Op.Include ? "" : "-", name);
    }
}
