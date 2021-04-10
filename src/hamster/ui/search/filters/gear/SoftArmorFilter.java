package hamster.ui.search.filters.gear;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.Armor;

import java.util.Optional;
import java.util.regex.Matcher;

public class SoftArmorFilter implements Filter {
    public static final String TAG = "sarmor";
    public static final String pattern = "sarmor\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new SoftArmorFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public SoftArmorFilter(final NumberOp op, final float value) {
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var armo = item.getinfo(Armor.class);
	return armo.map(arm -> switch (op) {
	    case Less -> arm.soft < value;
	    case LessThanOrEqual -> arm.soft <= value;
	    case Equal -> arm.soft == value;
	    case GreaterThanOrEqual -> arm.soft >= value;
	    case Greater -> arm.soft > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Soft Armor %s %.2f", op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", TAG, op, value);
    }
}
