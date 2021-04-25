package hamster.ui.food.filters;

import hamster.data.food.Food;

import java.util.function.Function;

public class FieldFilter implements Filter {
    private final Function<Food, Boolean> filter;
    public FieldFilter(final Function<Food, Boolean> filter) {
        this.filter = filter;
    }

    @Override
    public boolean included(Food item) {
	return filter.apply(item);
    }
}
