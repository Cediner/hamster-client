package hamster.ui.food.sort;

import hamster.data.food.Fep;
import hamster.data.food.FepType;
import hamster.data.food.Food;

import java.util.Comparator;

public class AttrSort extends Sort implements Comparator<Food> {
    private final Direction dir;
    private final FepType attr;
    public AttrSort(final SortMethod method, final Direction dir, final FepType attr) {
	super(method);
        this.dir = dir;
	this.attr = attr;
    }

    public FepType attr() { return attr; }

    @Override
    public Sort reverse() {
        return new AttrSort(method(), dir, attr.complement());
    }

    @Override
    public int compare(Food o1, Food o2) {
        final Fep o1fep = o1.getFep(attr);
        final Fep o2fep = o2.getFep(attr);
        if(o1fep == null && o2fep == null)
            return 0;
        if(o1fep == null)
            return dir == Direction.ASC ? -1 : 1;
        if(o2fep == null)
            return dir == Direction.ASC ? 1 : -1;

        return Float.compare(o1fep.value, o2fep.value) * (dir == Direction.ASC ? 1 : -1);
    }
}
