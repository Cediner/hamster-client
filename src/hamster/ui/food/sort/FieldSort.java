package hamster.ui.food.sort;

import hamster.data.food.Food;

import java.util.Comparator;

public class FieldSort extends Sort implements Comparator<Food> {
    private final Supplier compare;
    private final Direction dir;
    public FieldSort(final SortMethod method, final Direction dir, final Supplier comparer) {
	super(method);
        this.dir = dir;
	this.compare = comparer;
    }

    @Override
    public Sort reverse() {
	return new FieldSort(method(), dir.reverse(), compare);
    }

    @Override
    public int compare(Food o1, Food o2) {
	return compare.apply(o1, o2) * (dir == Direction.ASC ? 1 : -1);
    }
}
