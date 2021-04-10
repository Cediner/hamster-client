package hamster.ui.search.filters.symbol;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.Gast;

import java.util.Optional;
import java.util.regex.Matcher;

public class BonusFilter implements Filter {
    public static final String TAG = "bonus";
    public static final String pattern = "bonus\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new BonusFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public BonusFilter(final NumberOp op, final float value) {
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var gasto = item.getinfo(Gast.class);
	return gasto.map(gast -> switch (op) {
	    case Less -> gast.bonus() < value;
	    case LessThanOrEqual -> gast.bonus() <= value;
	    case Equal -> gast.bonus() == value;
	    case GreaterThanOrEqual -> gast.bonus() >= value;
	    case Greater -> gast.bonus() > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "FEP Bonus %s %.2f", op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", TAG, op, value);
    }
}
