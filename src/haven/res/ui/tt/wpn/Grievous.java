package haven.res.ui.tt.wpn;

/* Preprocessed source code */
/* $use: ui/tt/wpn/info */
import haven.ItemInfo;
import haven.res.ui.tt.wpn.info.*;

/* >tt: Grievous */
public class Grievous extends WeaponInfo {
    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
	    return mkinfo(owner, args);
	}
    }

    public final double deg;

    public Grievous(Owner owner, double deg) {
	super(owner);
	this.deg = deg;
    }

    public static Grievous mkinfo(Owner owner, Object... args) {
	return(new Grievous(owner, ((Number)args[1]).doubleValue() * 0.01));
    }

    public String wpntips() {
	return(String.format("Grievous damage: %.1f%%", deg * 100));
    }

    public int order() {return(80);}

    public double dmg() {
        return deg * 100d;
    }
}

