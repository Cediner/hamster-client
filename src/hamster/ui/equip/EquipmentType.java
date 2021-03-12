package hamster.ui.equip;

import haven.Coord;
import haven.Equipory;

import java.util.HashMap;
import java.util.Map;

public enum EquipmentType {
    HeadGear(Equipory.ecoords[0], 0),
    Accessory(Equipory.ecoords[1], 1),
    Shirt(Equipory.ecoords[2], 2),
    TorsoArmor(Equipory.ecoords[3], 3),
    Gloves(Equipory.ecoords[4], 4),
    Belt(Equipory.ecoords[5], 5),
    LeftHand(Equipory.ecoords[6], 6),
    RightHand(Equipory.ecoords[7], 7),
    LeftRing(Equipory.ecoords[8], 8),
    RightRing(Equipory.ecoords[9], 9),
    Robe(Equipory.ecoords[10], 10),
    Back(Equipory.ecoords[11], 11),
    Pants(Equipory.ecoords[12], 12),
    LegArmor(Equipory.ecoords[13], 13),
    Cape(Equipory.ecoords[14], 14),
    Shoes(Equipory.ecoords[15], 15),
    CosmeticHat(Equipory.ecoords[16], 16),
    Eyes(Equipory.ecoords[17], 17),
    Mouth(Equipory.ecoords[18], 18),
    Unknown(new Coord(-5, -5), -1);

    public static final Map<Coord, EquipmentType> eqmap = new HashMap<>();
    public final Coord position;
    public final int slot;

    EquipmentType(final Coord pos, final int slot) {
        this.position = pos.add(1, 1);
        this.slot = slot;
    }

    static {
        for (final EquipmentType type : values()) {
            eqmap.put(type.position, type);
        }
    }
}
