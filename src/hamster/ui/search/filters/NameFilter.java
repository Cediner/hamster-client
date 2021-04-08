package hamster.ui.search.filters;

import hamster.ui.search.ActList;
import haven.FastText;
import haven.GOut;

public class NameFilter implements Filter {
    private final String name;
    private final StringOp op;
    public NameFilter(final StringOp op, final String name) {
        this.name = name;
        this.op = op;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var contains = item.name.text.toLowerCase().contains(name.toLowerCase());
	return (op == StringOp.Include) == contains;
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Name %s %s", op, name);
    }

    @Override
    public String toString() {
	return String.format("%sname:%s", op.symbol, name);
    }
}
