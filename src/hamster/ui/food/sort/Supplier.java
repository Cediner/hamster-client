package hamster.ui.food.sort;

import hamster.data.food.FoodData;

public interface Supplier {
    int apply(final FoodData o1, final FoodData o2);
}
