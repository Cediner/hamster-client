package haven.res.ui.tt;

import haven.Equipory;
import haven.ItemInfo;

//Tells you what slot the item is for.
public class Equed extends ItemInfo implements Equipory.SlotInfo {
    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
	    return mkinfo(owner, args);
	}
    }

    public final int slots;

    public Equed(ItemInfo.Owner paramOwner, int paramInt) {
	super(paramOwner);
	this.slots = paramInt;
    }

    public static Equed mkinfo(ItemInfo.Owner paramOwner, Object... paramVarArgs) {
	int i = (Integer) paramVarArgs[1];
	return new Equed(paramOwner, i);
    }

    public int slots() {
	return this.slots;
    }
}

