package hamster.ui.food.sort;

import hamster.data.food.Food;

public interface Supplier {
    int apply(final Food o1, final Food o2);
}
