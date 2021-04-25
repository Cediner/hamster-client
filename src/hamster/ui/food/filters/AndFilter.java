package hamster.ui.food.filters;

import hamster.data.food.Food;

import java.util.List;

public class AndFilter implements Filter {
    private final List<Filter> filters;

    public AndFilter(final List<Filter> filters) {
	this.filters = filters;
    }

    @Override
    public boolean included(Food item) {
	boolean ret = true;
	for(final var filter : filters) {
	    ret = ret && filter.included(item);
	    if(!ret)
	        break;
	}
	return ret;
    }
}
