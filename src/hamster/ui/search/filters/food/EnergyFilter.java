package hamster.ui.search.filters.food;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.resutil.FoodInfo;

import java.util.Optional;
import java.util.regex.Matcher;

public class EnergyFilter implements Filter {
    public static final String TAG = "energy";
    public static final String pattern = "energy\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new EnergyFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public EnergyFilter(final NumberOp op, final float value) {
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var foodo = item.getinfo(FoodInfo.class);
	return foodo.map(food -> switch (op) {
	    case Less -> food.energy() < value;
	    case LessThanOrEqual -> food.energy() <= value;
	    case Equal -> food.energy() == value;
	    case GreaterThanOrEqual -> food.energy() >= value;
	    case Greater -> food.energy() > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Energy %s %.2f", op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", TAG, op, value);
    }
}
