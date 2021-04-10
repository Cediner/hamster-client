package hamster.data.itm;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;

public class ItemData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Map<String, ItemData> itmmap = new HashMap<>();

    public static void init() {
	logger.atInfo().log("Loading Item Data");
	final var gson = new Gson();
	try {
	    final var itms = gson.fromJson(new FileReader("data/ItemData.json5"), ItemData[].class);
	    for(final var  itm : itms) {
	        itmmap.put(itm.name, itm);
		itmmap.put(itm.name.toLowerCase(), itm);
	    }
	} catch (FileNotFoundException e) {
	    logger.atSevere().withCause(e).log("Failed to load item data");
	}
    }

    public static boolean hasTag(final String name, final ItemTag tag) {
        return itmmap.containsKey(name) && itmmap.get(name).hasTag(tag);
    }

    public static boolean isEquipable(final String name) {
        return hasTag(name, ItemTag.CAN_EQUIP);
    }

    public static double maxContent(final String name, final ContainerType type) {
        return itmmap.containsKey(name) ? itmmap.get(name).maxContent(type) : 0d;
    }

    public static int getWeaponDmg(final String name) {
        return itmmap.containsKey(name) && itmmap.get(name).weapondata != null
		? itmmap.get(name).weapondata.damage()
		: 0;
    }

    public static ItemData dataFor(final String name) {
        return itmmap.get(name);
    }

    private String name;
    private String res;
    private Set<ItemTag> tags;

    private ArmorData armordata = null;
    private WeaponData weapondata = null;
    private GearData geardata = null;
    private GlidData gliddata = null;
    private ContentData contentdata = null;

    public String name() { return name; }
    public String res() { return res; }
    public boolean hasTag(final ItemTag tag) { return tags.contains(tag); }
    public boolean isEquipable() { return hasTag(ItemTag.CAN_EQUIP); }
    public boolean isArmor() { return armordata != null; }
    public ArmorData getArmorData() { return armordata; }
    public boolean isWeapon() { return weapondata != null; }
    public WeaponData getWeaponData() { return weapondata; }
    public boolean isGear() { return geardata != null; }
    public GearData getGearData() { return geardata; }
    public boolean canGlid() { return gliddata != null; }
    public GlidData  getGlidData() { return gliddata; }
    public boolean hasContents() { return contentdata != null; }
    public ContentData getContentData() { return contentdata; }

    public double maxContent(final ContainerType type) {
        if(contentdata != null) {
	    return switch (type) {
		case LIQUID -> contentdata.liquid_max();
		case WEIGHT -> contentdata.weight_max();
		case SEED -> contentdata.seed_max();
	    };
	} else {
            return 0d;
	}
    }
}