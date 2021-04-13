package hamster.ui.search.filters;

import java.util.HashMap;
import java.util.Map;

public enum NumberOp {
    Less("<"), LessThanOrEqual("<="), Equal("="), GreaterThanOrEqual(">="), Greater(">");

    private static final Map<String, NumberOp> mapping = new HashMap<>();
    public final String symbol;
    NumberOp(final String text) { this.symbol = text; }

    @Override
    public String toString() {
	return symbol;
    }

    static {
        for(final var op : NumberOp.values()) {
            mapping.put(op.symbol, op);
            mapping.put(op.name().toLowerCase(), op);
        }
    }

    public static NumberOp parse(final String op) {
        return mapping.getOrDefault(op, null);
    }
}
