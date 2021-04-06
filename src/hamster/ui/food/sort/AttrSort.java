package hamster.ui.food.sort;

import hamster.data.food.FoodData;

import java.util.Comparator;

public class AttrSort extends Sort implements Comparator<FoodData> {
    private final Direction dir;
    private final FoodData.FepType attr;
    public AttrSort(final SortMethod method, final Direction dir, final FoodData.FepType attr) {
	super(method);
        this.dir = dir;
	this.attr = attr;
    }

    public FoodData.FepType attr() { return attr; }

    @Override
    public Sort reverse() {
        return new AttrSort(method(), dir, attr.complement());
    }

    @Override
    public int compare(FoodData o1, FoodData o2) {
        final FoodData.Fep o1fep = o1.getFep(attr);
        final FoodData.Fep o2fep = o2.getFep(attr);
        if(o1fep == null && o2fep == null)
            return 0;
        if(o1fep == null)
            return dir == Direction.ASC ? -1 : 1;
        if(o2fep == null)
            return dir == Direction.ASC ? 1 : -1;

        return Float.compare(o1fep.value, o2fep.value) * (dir == Direction.ASC ? 1 : -1);
    }
}
