package hamster.data.itm;

// Technically Attribute & Abilities
public enum Attribute {
    Strength("str"),
    Agility("agi"),
    Intelligence("int"),
    Constitution("con"),
    Perception("prc"),
    Charisma("csm"),
    Dexterity("dex"),
    Will("wil"),
    Psyche("psy"),
    UnarmedCombat("unarmed"),
    MeleeCombat("melee"),
    Marksmanship("ranged"),
    Exploration("explore"),
    Stealth("stealth"),
    Sewing("sewing"),
    Smithing("smithing"),
    Masonry("masonry"),
    Carpentry("carpentry"),
    Cooking("cooking"),
    Farming("farming"),
    Survival("survive"),
    Lore("lore");

    //This is what the server refers to it as with CAttrs
    public final String srvname;

    Attribute(final String nm) {
        this.srvname = nm;
    }
}
