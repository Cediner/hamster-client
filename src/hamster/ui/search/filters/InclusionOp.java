package hamster.ui.search.filters;

public enum InclusionOp {
    Include(""), Exclude("-");

    public final String symbol;
    InclusionOp(final String symbol) {
        this.symbol = symbol;
    }
}
