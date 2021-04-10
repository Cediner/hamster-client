package hamster.data.character;

import java.util.HashMap;
import java.util.Map;

public enum AttackWeight {
    Unarmed("Unarmed Combat", "gfx/hud/chr/unarmed"),
    Melee("Melee Combat", "gfx/hud/chr/melee");

    private static final Map<String, AttackWeight> mapping = new HashMap<>();
    public final String display;
    public final String resicon;
    AttackWeight(final String display, final String icon) {
        this.resicon = icon;
        this.display = display;
    }

    public boolean is(final String awnm) {
	return resicon.equals(awnm) || display.equalsIgnoreCase(awnm) || name().equalsIgnoreCase(awnm);
    }

    static {
	for(final var aw : AttackWeight.values()) {
	    mapping.put(aw.resicon, aw);
	    mapping.put(aw.display.toLowerCase(), aw);
	    mapping.put(aw.name().toLowerCase(), aw);
	}
    }

    public static AttackWeight parse(final String aw) {
	return mapping.get(aw.toLowerCase());
    }
}
