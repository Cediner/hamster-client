package hamster.data.food;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;
import haven.*;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;

public class FoodData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final List<FoodData> foods = new ArrayList<>();
    public static final Map<String, FepType> feptypemap = new HashMap<>();
    public static int longestname;

    public static class Ingredient {
        public String name;
        public int percentage;
    }

    public enum FepType {
        STR, AGI, INT, CON, PER, CHA, DEX, WIL, PSY,
	STR2, AGI2, INT2, CON2, PER2, CHA2, DEX2, WIL2, PSY2;

        public FepType complement() {
            return switch (this) {
                case STR  -> STR2;
		case AGI -> AGI2;
		case INT -> INT2;
		case CON -> CON2;
		case PER -> PER2;
		case CHA -> CHA2;
		case DEX -> DEX2;
		case WIL -> WIL2;
		case PSY -> PSY2;
		case STR2 -> STR;
		case AGI2 -> AGI;
		case INT2 -> INT;
		case CON2 -> CON;
		case PER2 -> PER;
		case CHA2 -> CHA;
		case DEX2 -> DEX;
		case WIL2 -> WIL;
		case PSY2 -> PSY;
	    };
	}
    }

    public static class Fep {
        public FepType type;
        public String name;
        public float value;
    }

    public static void init() {
	logger.atInfo().log("Loading Food Data");
	final Map<String, FepType> fepmap = new HashMap<>();
	fepmap.put("Strength +1", FepType.STR); feptypemap.put("str", FepType.STR);
	fepmap.put("Agility +1", FepType.AGI); feptypemap.put("agi", FepType.AGI);
	fepmap.put("Intelligence +1", FepType.INT); feptypemap.put("int", FepType.INT);
	fepmap.put("Constitution +1", FepType.CON); feptypemap.put("con", FepType.CON);
	fepmap.put("Perception +1", FepType.PER); feptypemap.put("per", FepType.PER);
	fepmap.put("Charisma +1", FepType.CHA); feptypemap.put("cha", FepType.CHA);
	fepmap.put("Dexterity +1", FepType.DEX); feptypemap.put("dex", FepType.DEX);
	fepmap.put("Will +1", FepType.WIL); feptypemap.put("wil", FepType.WIL);
	fepmap.put("Psyche +1", FepType.PSY); feptypemap.put("psy", FepType.PSY);
	fepmap.put("Strength +2", FepType.STR2); feptypemap.put("str2", FepType.STR2);
	fepmap.put("Agility +2", FepType.AGI2); feptypemap.put("agi2", FepType.AGI2);
	fepmap.put("Intelligence +2", FepType.INT2); feptypemap.put("int2", FepType.INT2);
	fepmap.put("Constitution +2", FepType.CON2); feptypemap.put("con2", FepType.CON2);
	fepmap.put("Perception +2", FepType.PER2); feptypemap.put("per2", FepType.PER2);
	fepmap.put("Charisma +2", FepType.CHA2); feptypemap.put("cha2", FepType.CHA2);
	fepmap.put("Dexterity +2", FepType.DEX2); feptypemap.put("dex2", FepType.DEX2);
	fepmap.put("Will +2", FepType.WIL2); feptypemap.put("wil2", FepType.WIL2);
	fepmap.put("Psyche +2", FepType.PSY2); feptypemap.put("psy2", FepType.PSY2);

	final var gson = new Gson();
	try {
	    final var itms = gson.fromJson(new FileReader("data/FoodData.json5"), FoodData[].class);
	    foods.addAll(Arrays.asList(itms));
	    int max = 0;
	    final var itr = foods.iterator();
	    for(var itm = itr.next(); itr.hasNext(); itm = itr.next()) {
	        itm.itemName = itm.itemName.replaceAll("[^\\x00-\\x7F()]", "").trim();
	        if(itm.itemName.matches("\\(.+\\)"))
	            itm.itemName = itm.itemName.substring(itm.itemName.indexOf('(')+1, itm.itemName.indexOf(')'));
	        if(itm.itemName.equals(""))
	            itr.remove();
	        max = Math.max(max, FastText.textw(itm.itemName));
		float val = 0;
	        for(final var fep : itm.feps) {
	            fep.type = fepmap.get(fep.name.trim());
	            val += fep.value;
		}
	        itm.totalFep = val;
	        if(itm.totalFep <= 0)
		    itr.remove();
	        if(itm.hunger <= 0)
		    itr.remove();

	        for(final var ing : itm.ingredients) {
	            ing.name = ing.name.replaceAll("[^\\x00-\\x7F()]", "").trim();
		    if(ing.name.matches("\\(.+\\)"))
			ing.name = ing.name.substring(ing.name.indexOf('(')+1, ing.name.indexOf(')'));
		}

	        final StringBuilder sb = new StringBuilder();
	        for(final var ing : itm.ingredients) {
	            if(sb.length() > 0)
	                sb.append(", ");
	            sb.append(ing.name);
	            sb.append(": ");
	            sb.append(ing.percentage);
	            sb.append('%');
		}
	        itm.ingredientsText = RichText.render(sb.toString(), UI.scale(300));
	    }
	    longestname = max;
	} catch (FileNotFoundException fe) {
	    logger.atSevere().withCause(fe).log("Failed to load food data");
	}
    }

    public String itemName;
    public String resourceName;
    public int energy;
    public float hunger;
    public float totalFep;
    public List<Ingredient> ingredients;
    public List<Fep> feps;

    public Text ingredientsText;

    public float fepPerHunger() {
        return totalFep / hunger;
    }

    public Fep getFep(final FepType type) {
        for(final var fep : feps) {
            if(fep.type == type)
                return fep;
	}
        return null;
    }
}
