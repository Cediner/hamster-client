package hamster.data.character;

import haven.Coord;
import haven.Equipory;

import java.util.HashMap;
import java.util.Map;

public enum EquipmentType {
    HeadGear(Equipory.ecoords[0], 0),   //1L
    Accessory(Equipory.ecoords[1], 1),  //2L
    Shirt(Equipory.ecoords[2], 2),      //3L
    TorsoArmor(Equipory.ecoords[3], 3), //3R
    Gloves(Equipory.ecoords[4], 4),     //4L
    Belt(Equipory.ecoords[5], 5),       //4R
    LeftHand(Equipory.ecoords[6], 6),   //5L
    RightHand(Equipory.ecoords[7], 7),  //5R
    LeftRing(Equipory.ecoords[8], 8),   //6L
    RightRing(Equipory.ecoords[9], 9),  //6R
    Robe(Equipory.ecoords[10], 10),     //7L
    Back(Equipory.ecoords[11], 11),     //7R
    Pants(Equipory.ecoords[12], 12),    //8L
    LegArmor(Equipory.ecoords[13], 13), //8R
    Cape(Equipory.ecoords[14], 14),     //9L
    Shoes(Equipory.ecoords[15], 15),    //9R
    CosmeticHat(Equipory.ecoords[16], 16), //1M
    Eyes(Equipory.ecoords[17], 17),     //1R
    Mouth(Equipory.ecoords[18], 18),    //2R
    Unknown(new Coord(-5, -5), -1);

    public static final Map<Coord, EquipmentType> eqmap = new HashMap<>();
    private static final Map<String, EquipmentType> mapping = new HashMap<>();
    public final Coord position;
    public final int slot;

    EquipmentType(final Coord pos, final int slot) {
        this.position = pos.add(1, 1);
        this.slot = slot;
    }

    public boolean is(final String nm) {
        return name().equalsIgnoreCase(nm) || Integer.toString(slot).equalsIgnoreCase(nm);
    }

    public boolean is(final int slot) {
        return this.slot == slot;
    }

    static {
        for (final EquipmentType type : values()) {
            eqmap.put(type.position, type);
            mapping.put(type.name().toLowerCase(), type);
            mapping.put(Integer.toString(type.slot), type);
        }
    }

    public static EquipmentType parse(final String eq) {
        return mapping.get(eq.toLowerCase());
    }
}
