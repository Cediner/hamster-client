package hamster.ui.search.filters.gear;

import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.NumberOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.wpn.Grievous;

import java.util.Optional;
import java.util.regex.Matcher;

public class GrievousDmgFilter implements Filter {
    public static final String TAG = "grievous";
    public static final String pattern = "grievous\\s*(?<op>(<|>|=|(<=)|(>=)))\\s*(?<arg>([0-9]+(\\.[0-9]+)?))";
    public static Optional<Filter> make(final Matcher match) {
	final var op = NumberOp.parse(match.group("op"));
	return Optional.of(new GrievousDmgFilter(op, Float.parseFloat(match.group("arg"))));
    }

    private final NumberOp op;
    private final float value;

    public GrievousDmgFilter(final NumberOp op, final float value) {
	this.op = op;
	this.value = value;
    }

    @Override
    public boolean included(ActList.ActItem item) {
	final var dmgo = item.getinfo(Grievous.class);
	return dmgo.map(dmg -> switch (op) {
	    case Less -> dmg.dmg() < value;
	    case LessThanOrEqual -> dmg.dmg() <= value;
	    case Equal -> dmg.dmg() == value;
	    case GreaterThanOrEqual -> dmg.dmg() >= value;
	    case Greater -> dmg.dmg() > value;
	}).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Damage %s %.2f", op, value);
    }

    @Override
    public String toString() {
	return String.format("%s%s%.2f", TAG, op, value);
    }
}
