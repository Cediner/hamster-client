package hamster.ui.food.filters;

import hamster.data.food.Food;
import haven.GOut;

public interface Filter {
    /**
     * @return true if item is within the filter, false if it should be excluded from the final list
     */
    boolean included(final Food item);

    default void render(final GOut g) {}
}
