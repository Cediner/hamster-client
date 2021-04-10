package hamster.ui.search.filters.gildable;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.Gild;

import java.util.Optional;
import java.util.regex.Matcher;

/**
 * Applies to gildable and gildings
 *
 * This one is a bit odd.
 * For < and <= it'll compare against the max chance
 * For > and >= it'll compare against the min chance
 * For = it'll return true if the value is within [pmin, pmax]
 */
public class ChanceFilter implements Filter {
    public static final String TAG = "chance";
    public static final String pattern = "chance\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new ChanceFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public ChanceFilter(final NumberOp op, final float value) {
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var gildo = item.getinfo(Gild.class);
	return gildo.map(gild -> switch (op) {
	    case Less -> gild.pmax() < value;
	    case LessThanOrEqual -> gild.pmax() <= value;
	    case Equal -> gild.pmin() <= value && value <= gild.pmax();
	    case GreaterThanOrEqual -> gild.pmin() >= value;
	    case Greater -> gild.pmin() > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Chance %s %.2f", op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", TAG, op, value);
    }
}
