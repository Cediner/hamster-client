package hamster.ui.search.filters.gildable;

import hamster.data.character.Attribute;
import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.InclusionOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.Gild;

import java.util.Optional;
import java.util.regex.Matcher;

/**
 * Applies to gildable and gildings
 *
 * This is for what the Gild bases chance against attr wise, not what it gives
 */
public class GildAttrFilter implements Filter {
    public static final String TAG = "gildattr";
    public static final String pattern = "(?<op>-)?gildattr:(?<arg>.+)";
    public static Optional<Filter> make(final Matcher match) {
	final var op = match.group("op") != null ? InclusionOp.Exclude : InclusionOp.Include;
	final var attr = Attribute.parse(match.group("arg"));
	return attr != null ? Optional.of(new GildAttrFilter(op, attr)) : Optional.empty();
    }

    private final Attribute attr;
    private final InclusionOp op;
    public GildAttrFilter(final InclusionOp op, final Attribute attr) {
	this.attr = attr;
	this.op = op;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var gildo = item.getinfo(Gild.class);
	final var contains = gildo.map(gild -> gild.hasAttr(attr)).orElse(false);
	return (op == InclusionOp.Include) == contains;
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Gild Attr %s %s", op, attr);
    }

    @Override
    public String toString() {
	return String.format("%s%s:%s", op.symbol, TAG, attr);
    }
}
