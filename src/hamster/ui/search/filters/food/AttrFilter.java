package hamster.ui.search.filters.food;

import hamster.data.character.Attribute;
import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.resutil.FoodInfo;

import java.util.Optional;
import java.util.regex.Matcher;

public class AttrFilter implements Filter {
    public static final String TAG = "food:";
    public static final String pattern = "food:(?<attr>[a-zA-Z0-9]+)\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
        final var attr = Attribute.parse(match.group("attr"));
	final var op = NumberOp.parse(match.group("op"));
	return attr != null ? Optional.of(new AttrFilter(attr, op, Float.parseFloat(match.group("arg")))) : Optional.empty();
    }

    private final Attribute attr;
    private final NumberOp op;
    private final float value;

    public AttrFilter(final Attribute attr, final NumberOp op, final float value) {
        this.attr = attr;
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var foodo = item.getinfo(FoodInfo.class);
	return foodo.map(food -> food.event(attr).map(ev -> switch (op) {
	    case Less -> ev.a < value;
	    case LessThanOrEqual -> ev.a <= value;
	    case Equal -> ev.a == value;
	    case GreaterThanOrEqual -> ev.a >= value;
	    case Greater -> ev.a > value;
	}).orElse(false)).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "%s %s %.2f", attr.display, op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%s%.2f", TAG, attr.name().toLowerCase(), op, value);
    }
}
