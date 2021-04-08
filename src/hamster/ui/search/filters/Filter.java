package hamster.ui.search.filters;

import hamster.ui.search.ActList;
import haven.GOut;

public interface Filter {
    /**
     * @return true if item is within the filter, false if it should be excluded from the final list
     */
    boolean included(final ActList.ActItem item);

    default void render(final GOut g) {}
}
