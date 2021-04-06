package hamster.ui.food.filters;

import hamster.data.food.FoodData;

import java.util.function.Function;

public class FieldFilter implements Filter {
    private final Function<FoodData, Boolean> filter;
    public FieldFilter(final Function<FoodData, Boolean> filter) {
        this.filter = filter;
    }

    @Override
    public boolean included(FoodData item) {
	return filter.apply(item);
    }
}
