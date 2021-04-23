package hamster.ui.food.filters;

import hamster.data.food.Food;

import java.util.List;

public class OrFilter implements Filter {
    private final List<Filter> filters;

    public OrFilter(final List<Filter> filters) {
        this.filters = filters;
    }

    @Override
    public boolean included(Food item) {
	boolean ret = false;
	for(final var filter : filters)
	    ret = ret || filter.included(item);
	return ret;
    }
}
