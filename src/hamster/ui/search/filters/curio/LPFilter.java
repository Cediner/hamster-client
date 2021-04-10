package hamster.ui.search.filters.curio;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.InclusionOp;
import hamster.ui.search.filters.NameFilter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.resutil.Curiosity;

import java.util.Optional;
import java.util.regex.Matcher;

public class LPFilter implements Filter {
    public static final String TAG = "lp";
    public static final String pattern = "lp\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
        final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new LPFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public LPFilter(final NumberOp op, final float value) {
        this.op = op;
        this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var curio = item.getinfo(Curiosity.class);
	return curio.map(curiosity -> switch (op) {
	    case Less -> curiosity.exp < value;
	    case LessThanOrEqual -> curiosity.exp <= value;
	    case Equal -> curiosity.exp == value;
	    case GreaterThanOrEqual -> curiosity.exp >= value;
	    case Greater -> curiosity.exp > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "LP %s %.2f", op, value);
    }

    @Override
    public String toString() {
        return String.format("%s%s%.2f", TAG, op, value);
    }
}
