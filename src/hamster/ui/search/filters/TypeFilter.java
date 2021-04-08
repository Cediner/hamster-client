package hamster.ui.search.filters;

import hamster.ui.search.ActList;
import haven.GOut;

public class TypeFilter implements Filter {
    public enum Type {
        Buildable, // Any signpost based items
	Craftable, // Any hand crafted based items
	Food, // Items with haven.resutil.FoodInfo
	Curio, // Items with haven.resutil.Curiosity
	Gear, // TODO: Base off ItemData? Requires a lot of work. Can base off haven.res.ui.tt.Equed?
	Armor, // Items with haven.res.ui.tt.Armor
	Weapon, // Items with haven.res.ui.tt.wpn.Armpen/Damage
	Gild, // Items with haven.res.ui.tt.ISlots
	Symbol, // Items with haven.res.ui.tt.Gast
	Magic // Items with haven.res.ui.tt.Cost
    }

    @Override
    public boolean included(ActList.ActItem item) {
	return false;
    }

    @Override
    public void render(GOut g) {

    }
}
