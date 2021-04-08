package hamster.ui.search.filters;

import hamster.ui.search.ActList;

import java.util.List;

public class OrFilter implements Filter {
    private final List<Filter> filters;

    public OrFilter(final List<Filter> filters) {
        this.filters = filters;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	boolean ret = false;
	for(final var filter : filters)
	    ret = ret || filter.included(item);
	return ret;
    }
}
