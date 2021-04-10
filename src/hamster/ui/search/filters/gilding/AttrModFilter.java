package hamster.ui.search.filters.gilding;

import hamster.data.character.Attribute;
import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.Slotted;

import java.util.Optional;
import java.util.regex.Matcher;

public class AttrModFilter implements Filter {
    public static final String TAG = "gildmod:";
    public static final String pattern = "gildmod:(?<attr>[a-zA-Z0-9]+)\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var attr = Attribute.parse(match.group("attr"));
	final var op = NumberOp.parse(match.group("op"));
	return attr != null ? Optional.of(new AttrModFilter(attr, op, Float.parseFloat(match.group("arg")))) : Optional.empty();
    }

    private final Attribute attr;
    private final NumberOp op;
    private final float value;

    public AttrModFilter(final Attribute attr, final NumberOp op, final float value) {
	this.attr = attr;
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var gildo = item.getinfo(Slotted.class);
	return gildo.map(gild -> gild.mod(attr).map(mod -> switch (op) {
	    case Less -> mod.mod < value;
	    case LessThanOrEqual -> mod.mod <= value;
	    case Equal -> mod.mod == value;
	    case GreaterThanOrEqual -> mod.mod >= value;
	    case Greater -> mod.mod > value;
	}).orElse(false)).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Gild Mod %s %s %.2f", attr.display, op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%s%.2f", TAG, attr.name().toLowerCase(), op, value);
    }
}
