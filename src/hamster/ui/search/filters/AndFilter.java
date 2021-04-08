package hamster.ui.search.filters;

import hamster.ui.search.ActList;

import java.util.List;

public class AndFilter implements Filter {
    private final List<Filter> filters;

    public AndFilter(final List<Filter> filters) {
	this.filters = filters;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	boolean ret = true;
	for(final var filter : filters) {
	    ret = ret && filter.included(item);
	    if(!ret)
	        break;
	}
	return ret;
    }
}
