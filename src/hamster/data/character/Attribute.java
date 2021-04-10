package hamster.data.character;

import java.util.HashMap;
import java.util.Map;

/**
 * Standardizing any enums describing the Attributes of this game including special +2 attrs from foods.
 *
 * This also includes "Abilities" to simply things since Attributes and Abilities are both CAttrs and
 * usually not distinguished outside of the UI
 */
public enum Attribute {
    Unarmed("Unarmed Combat", "gfx/hud/chr/unarmed"), Melee("Melee Combat", "gfx/hud/chr/melee"),
    Marksmanship("Marksmanship", "gfx/hud/chr/ranged"), Exploration("Exploration", "gfx/hud/chr/explore"),
    Stealth("Stealth", "gfx/hud/chr/stealth"), Sewing("Sewing", "gfx/hud/chr/sewing"),
    Smithing("Smithing", "gfx/hud/chr/smithing"), Masonry("Masonry", "gfx/hud/chr/masonry"),
    Carpentry("Carpentry", "gfx/hud/chr/carpentry"), Cooking("Cooking", "gfx/hud/chr/cooking"),
    Farming("Farming", "gfx/hud/chr/farming"), Survival("Survival", "gfx/hud/chr/survive"),
    Lore("Lore", "gfx/hud/chr/lore"),

    Str("Strength", "gfx/hud/chr/str"), Agi("Agility", "gfx/hud/chr/agi"),
    Int("Intelligence", "gfx/hud/chr/int"), Con("Constitution", "gfx/hud/chr/con"),
    Per("Perception", "gfx/hud/chr/prc"), Cha("Charisma", "gfx/hud/chr/csm"),
    Dex("Dexterity", "gfx/hud/chr/dex"), Wil("Will", "gfx/hud/chr/wil"),
    Psy("Psyche", "gfx/hud/chr/psy"),

    Str2("Strength +2", ""), Agi2("Agility +2", ""),
    Int2("Intelligence +2", ""), Con2("Constitution +2", ""),
    Per2("Perception +2", ""), Cha2("Charisma +2", ""),
    Dex2("Dexterity +2", ""), Wil2("Will +2", ""),
    Psy2("Psyche +2", "");

    private static final Map<String, Attribute> mapping = new HashMap<>();
    public final String display;
    public final String resicon;
    Attribute(final String display, final String resicon) {
	this.display = display;
	this.resicon = resicon;
    }

    public boolean is(final String attrnm) {
        return display.equalsIgnoreCase(attrnm) || resicon.equals(attrnm) || name().equalsIgnoreCase(attrnm);
    }

    static {
        for(final var attr : Attribute.values()) {
            mapping.put(attr.resicon, attr);
            mapping.put(attr.display.toLowerCase(), attr);
            mapping.put(attr.name().toLowerCase(), attr);
	}
    }

    public static Attribute parse(final String attr) {
        return mapping.get(attr.toLowerCase());
    }
}
