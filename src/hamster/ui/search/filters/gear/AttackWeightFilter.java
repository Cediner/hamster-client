package hamster.ui.search.filters.gear;

import hamster.data.character.AttackWeight;
import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.InclusionOp;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.wpn.Weight;

import java.util.Optional;
import java.util.regex.Matcher;

public class AttackWeightFilter implements Filter {
    public static final String TAG = "weight";
    public static final String pattern = "(?<op>-)?weight:(?<arg>.+)";
    public static Optional<Filter> make(final Matcher match) {
        final var op = match.group("op") != null ? InclusionOp.Exclude : InclusionOp.Include;
        final var aw = AttackWeight.parse(match.group("arg"));
        return aw != null ? Optional.of(new AttackWeightFilter(aw, op)) : Optional.empty();
    }

    private final AttackWeight aw;
    private final InclusionOp op;

    public AttackWeightFilter(final AttackWeight aw, final InclusionOp op) {
        this.aw = aw;
        this.op = op;
    }

    @Override
    public boolean included(ActList.ActItem item) {
        final var weighto = item.getinfo(Weight.class);
        return (op == InclusionOp.Include) == weighto.map(weight -> aw.is(weight.attr.name)).orElse(false);
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Atk Weight %s %s", op, aw.name());
    }

    @Override
    public String toString() {
	return String.format("%s%s:%s", op.symbol, TAG, aw.name().toLowerCase());
    }
}
