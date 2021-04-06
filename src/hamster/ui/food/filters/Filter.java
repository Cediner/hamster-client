package hamster.ui.food.filters;

import hamster.data.food.FoodData;

public interface Filter {
    /**
     * @return true if item is within the filter, false if it should be excluded from the final list
     */
    boolean included(final FoodData item);
}
