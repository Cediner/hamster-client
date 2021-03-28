package hamster.data;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

public class FarmingData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public enum CropType {
        GROUND, TRELLIS
    }

    public static class Crop {
       	private String name;
       	private CropType type;
       	private int min_stage, final_stage;

       	public String name() { return name; }
       	public CropType type() { return type; }
       	public int min_stage() { return min_stage; }
       	public int final_stage() { return final_stage; }
       	public boolean multistage() { return min_stage < final_stage; }
    }

    private static final Map<String, Crop> cropmap = new HashMap<>();
    public static void init() {
	logger.atInfo().log("Loading Crop Data");
        final var gson = new Gson();
	try {
	    final var crops = gson.fromJson(new FileReader("data/FarmingData.json5"), Crop[].class);
	    for(final var crop : crops) {
	        cropmap.put(crop.name, crop);
	    }
	} catch (FileNotFoundException e) {
	    logger.atSevere().withCause(e).log("Failed to load crop data");
	}
    }

    public static boolean isACrop(final String name) {
        return cropmap.containsKey(name);
    }

    public static Crop getCropData(final String name) {
        return cropmap.get(name);
    }
}
