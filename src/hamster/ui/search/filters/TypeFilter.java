package hamster.ui.search.filters;

import hamster.data.itm.ItemData;
import hamster.ui.search.ActList;
import haven.FastText;
import haven.GOut;
import haven.res.ui.tt.*;
import haven.res.ui.tt.wpn.Weight;
import haven.resutil.Curiosity;
import haven.resutil.FoodInfo;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;

public class TypeFilter implements Filter {
    public static final String TAG = "type";
    public static final String pattern = "(?<op>-)?type:(?<arg>.+)";
    private static final Map<String, Type> typemapping = new HashMap<>();
    public static Optional<Filter> make(final Matcher match) {
	final var op = match.group("op") != null ? InclusionOp.Exclude : InclusionOp.Include;
	final var type = typemapping.getOrDefault(match.group("arg"), null);
	return type != null ? Optional.of(new TypeFilter(type, op)) : Optional.empty();
    }

    public enum Type {
        Buildable, // Any signpost based items
	Craftable, // Any hand crafted based items
	Food, // Items with haven.resutil.FoodInfo
	Curio, // Items with haven.resutil.Curiosity
	Gear, // Based off ItemData
	Armor, // Items with haven.res.ui.tt.Armor
	Weapon, // Items with haven.res.ui.tt.wpn.Armpen/Damage or just Weight
	Gilding, // Items with haven.res.ui.tt.Slotted
	Gildable, // Items with haven.res.ui.tt.ISlots
	Symbol, // Items with haven.res.ui.tt.Gast
	Magic // Items with haven.res.ui.tt.Cost
    }
    static {
        for(final var type : Type.values()) {
            typemapping.put(type.name().toLowerCase(), type);
	}
    }

    private final Type type;
    private final InclusionOp op;

    public TypeFilter(final Type type, final InclusionOp op) {
        this.type = type;
        this.op = op;
    }

    @Override
    public boolean included(ActList.ActItem item) {
        final boolean contains = switch (type) {
	    case Buildable -> item.pagina.res().name.startsWith("paginae/bld/");
	    case Craftable -> item.pagina.res().name.startsWith("paginae/craft/");
	    case Food -> item.getinfo(FoodInfo.class).isPresent();
	    case Curio -> item.getinfo(Curiosity.class).isPresent();
	    case Gear -> ItemData.isEquipable(item.name.text.toLowerCase());
	    case Armor -> item.getinfo(Armor.class).isPresent();
	    case Weapon -> item.getinfo(Weight.class).isPresent();
	    case Gilding -> item.getinfo(Slotted.class).isPresent();
	    case Gildable -> item.getinfo(ISlots.class).isPresent();
	    case Symbol -> item.getinfo(Gast.class).isPresent();
	    case Magic -> item.getinfo(Cost.class).isPresent();
	};
	return (op == InclusionOp.Include) == contains;
    }

    @Override
    public void render(GOut g) {
	FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "Name %s %s", op, type);
    }

    @Override
    public String toString() {
	return String.format("%s%s:%s", op.symbol, TAG, type.name().toLowerCase());
    }
}
