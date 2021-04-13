package hamster.ui.search.filters;

import hamster.ui.search.ActList;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.Inputs;

import java.util.Optional;
import java.util.regex.Matcher;

public class InputFilter implements Filter {
    public static final String TAG = "from";
    public static final String pattern = "(?<op>-)?from:(?<arg>.+)";
    public static Optional<Filter>  make(final Matcher match) {
	final var op = match.group("op") != null ? InclusionOp.Exclude : InclusionOp.Include;
	return Optional.of(new InputFilter(op, match.group("arg")));
    }

    private final String name;
    private final InclusionOp op;
    public InputFilter(final InclusionOp op, final String name) {
	this.name = name;
	this.op = op;
    }

    @Override
    public boolean included(ActList.ActItem item) {
        final Optional<Inputs> inputs = item.getinfo(Inputs.class);
        if(inputs.isPresent()) {
            boolean contains = false;
            for(final var input : inputs.get().inputs) {
                if(input.spec.name().toLowerCase().contains(name.toLowerCase())) {
                    contains = true;
                    break;
		}
	    }
	    return (op == InclusionOp.Include) == contains;
	} else {
            return op == InclusionOp.Exclude;
	}
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Input %s %s", op, name);
    }

    @Override
    public String toString() {
	return String.format("%s%s:%s", op.symbol, TAG, name);
    }
}
