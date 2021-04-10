package hamster.ui.search.filters.gear;

import hamster.data.character.EquipmentType;
import hamster.data.itm.ItemData;
import hamster.ui.search.ActList;
import hamster.ui.search.filters.Filter;
import hamster.ui.search.filters.InclusionOp;
import haven.FastText;
import haven.GOut;

import java.util.Optional;
import java.util.regex.Matcher;

public class SlotFilter implements Filter {
    public static final String TAG = "slot";
    public static final String pattern = "(?<op>-)?slot:(?<arg>.+)";
    public static Optional<Filter> make(final Matcher match) {
	final var op = match.group("op") != null ? InclusionOp.Exclude : InclusionOp.Include;
	final var slot = EquipmentType.parse(match.group("arg"));
	return slot != null ? Optional.of(new SlotFilter(slot, op)) : Optional.empty();
    }

    private final EquipmentType slot;
    private final InclusionOp op;

    public SlotFilter(final EquipmentType slot, final InclusionOp op) {
	this.slot = slot;
	this.op = op;
    }

    @Override
    public boolean included(ActList.ActItem item) {
        final var data = ItemData.dataFor(item.name.text.toLowerCase());
	boolean ret = false;
        if(data != null && data.getGearData() != null) {
            final var gear = data.getGearData();
            for(final var slot : gear.slots()) {
		ret |= (op == InclusionOp.Include) == (this.slot == slot);
	    }
	}
	return ret;
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Slot %s %s", op, slot.name());
    }

    @Override
    public String toString() {
	return String.format("%s%s:%s", op.symbol, TAG, slot.name().toLowerCase());
    }
}
