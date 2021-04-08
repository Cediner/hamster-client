package hamster.ui.search.filters;

public enum StringOp {
    Include(""), Exclude("-");

    public final String symbol;
    StringOp(final String symbol) {
        this.symbol = symbol;
    }
}
