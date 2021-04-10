package hamster.ui.search.filters.symbol;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.Gast;

import java.util.Optional;
import java.util.regex.Matcher;

public class ReductionFilter implements Filter {
    public static final String TAG = "reduction";
    public static final String pattern = "reduction\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new ReductionFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public ReductionFilter(final NumberOp op, final float value) {
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var gasto = item.getinfo(Gast.class);
	return gasto.map(gast -> switch (op) {
	    case Less -> gast.reduction() < value;
	    case LessThanOrEqual -> gast.reduction() <= value;
	    case Equal -> gast.reduction() == value;
	    case GreaterThanOrEqual -> gast.reduction() >= value;
	    case Greater -> gast.reduction() > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Hunger Reduction %s %.2f", op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", TAG, op, value);
    }
}
