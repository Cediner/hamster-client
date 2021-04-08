package hamster.ui.search.filters;

public enum NumberOp {
    Less("<"), LessThanOrEqual("<="), Equal("="), GreaterThanOrEqual(">="), Greater(">");

    public final String symbol;
    NumberOp(final String text) { this.symbol = text; }

    @Override
    public String toString() {
	return symbol;
    }
}
