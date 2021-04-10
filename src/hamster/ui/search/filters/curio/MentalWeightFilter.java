package hamster.ui.search.filters.curio;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.resutil.Curiosity;

import java.util.Optional;
import java.util.regex.Matcher;

public class MentalWeightFilter implements Filter {
    public static final String TAG = "mental";
    public static final String pattern = "mental\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new MentalWeightFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public MentalWeightFilter(final NumberOp op, final float value) {
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var curio = item.getinfo(Curiosity.class);
	return curio.map(curiosity -> switch (op) {
	    case Less -> curiosity.mw < value;
	    case LessThanOrEqual -> curiosity.mw <= value;
	    case Equal -> curiosity.mw == value;
	    case GreaterThanOrEqual -> curiosity.mw >= value;
	    case Greater -> curiosity.mw > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Mental Weight %s %.2f", op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", TAG, op, value);
    }
}
