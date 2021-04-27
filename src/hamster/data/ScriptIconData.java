package hamster.data;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;
import haven.Resource;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

public class ScriptIconData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Map<String, Resource.Named> icons = new HashMap<>();
    private static final Resource.Named def = Resource.local().load("custom/paginae/default/script-icons/default");

    public static void init() {
	logger.atInfo().log("Loading Script Icon Data");
	final var gson = new Gson();
	final File dir = new File("data/scripts/");
	if (dir.exists()) {
	    final File[] files = dir.listFiles((fdir, name) -> (name.endsWith(".json") || name.endsWith(".json5")));
	    if (files != null) {
	        for(final var fn : files) {
	            try {
	                final var map = gson.fromJson(new FileReader(fn), Map.class);
	                for(final var key : map.keySet()) {
	                    icons.put((String)key, Resource.local().load((String)map.get(key)));
			}
		    } catch (FileNotFoundException e) {
			logger.atWarning().withCause(e).log("Failed to load script icon data");
		    }
		}
	    }
	}
    }

    public static Resource.Named getIcon(final String file) {
        return icons.getOrDefault(file, def);
    }
}
