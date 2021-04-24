package hamster.ui.food.filters;

import hamster.data.food.FepType;
import hamster.data.food.Food;
import haven.FastText;
import haven.GOut;

public class AttrFilter implements Filter {
    public enum Op {
	Less("<"), LessThanOrEqual("<="), Equal("="), GreaterThanOrEqual(">="), Greater(">");

	final String text;
	Op(final String text) { this.text = text; }

	@Override
	public String toString() {
	    return text;
	}
    }

    private final FepType feptype;
    private final Op op;
    private final float value;

    public AttrFilter(final FepType fep, final Op op, final float value) {
        this.feptype = fep;
        this.op = op;
        this.value = value;
    }

    @Override
    public boolean included(Food item) {
        for(final var fep : item.feps) {
            if(fep.type == feptype) {
                switch (op) {
		    case Less -> {
			return fep.value < value;
		    }
		    case LessThanOrEqual -> {
			return fep.value <= value;
		    }
		    case Equal -> {
			return fep.value == value;
		    }
		    case GreaterThanOrEqual -> {
			return fep.value >= value;
		    }
		    case Greater -> {
			return fep.value > value;
		    }
		}
	    }
	}
        return false;
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "%s %s %.2f", feptype, op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", feptype.toString().toLowerCase(), op, value);
    }
}
