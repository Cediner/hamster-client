package hamster.data.itm;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Set;

public class ItemData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static void init() {
	logger.atInfo().log("Loading Object Data");
	final var gson = new Gson();
	try {
	    final var itms = gson.fromJson(new FileReader("data/ItemData.json5"), ItemData[].class);
	} catch (FileNotFoundException e) {
	    logger.atSevere().withCause(e).log("Failed to load item data");
	}
    }


    private String name;
    private String res;
    private Set<ItemTag> tags;

    private ArmorData armordata;
    private WeaponData weapondata;
    private GearData geardata;
    private GlidData gliddata;
}