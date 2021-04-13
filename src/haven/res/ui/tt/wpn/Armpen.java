package haven.res.ui.tt.wpn;

import haven.ItemInfo;
import haven.res.ui.tt.wpn.info.WeaponInfo;

public class Armpen extends WeaponInfo {
    public static final Armpen NOPEN = new Armpen(null, 0.0);

    public static class Fac implements ItemInfo.InfoFactory {
        @Override
        public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
            return new Armpen(owner, ((Number) args[1]).doubleValue() * 0.01D);
        }
    }

    public final double deg;

    public Armpen(ItemInfo.Owner owner, double deg) {
        super(owner);
        this.deg = deg;
    }

    public static Armpen mkinfo(ItemInfo.Owner owner, Object... args) {
        return new Armpen(owner, ((Number) args[1]).doubleValue() * 0.01D);
    }

    public String wpntips() {
        return String.format("Armor penetration: %.1f%%", this.deg * 100.0D);
    }

    public int order() {
        return 100;
    }

    public double pen() {
        return deg * 100d;
    }
}
