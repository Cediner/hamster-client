package hamster.ui.search.filters;

import hamster.ui.search.ActList;
import haven.FastText;
import haven.GOut;

import java.util.Optional;
import java.util.regex.Matcher;

public class NameFilter implements Filter {
    public static final String TAG = "name";
    public static final String pattern = "(?<op>-)?name:(?<arg>.+)";
    public static Optional<Filter> make(final Matcher match) {
	final var op = match.group("op") != null ? InclusionOp.Exclude : InclusionOp.Include;
	return Optional.of(new NameFilter(op, match.group("arg")));
    }

    private final String name;
    private final InclusionOp op;
    public NameFilter(final InclusionOp op, final String name) {
        this.name = name;
        this.op = op;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var contains = item.name.text.toLowerCase().contains(name.toLowerCase());
	return (op == InclusionOp.Include) == contains;
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Name %s %s", op, name);
    }

    @Override
    public String toString() {
	return String.format("%s%s:%s", op.symbol, TAG, name);
    }
}
