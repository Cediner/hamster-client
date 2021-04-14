package hamster.data;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;
import hamster.GlobalSettings;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class TranslationLookup {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Map<String, Language> langs = new HashMap<>();
    public static class Language {
        public String name;
	public String display;
        public Map<String, String> translations;

        public String get(final String tag) {
            return translations.get(tag);
	}
    }

    public static void init() {
	logger.atInfo().log("Loading Language Data");
	final var gson = new Gson();
	try {
	    final var langs = gson.fromJson(new FileReader("data/LanguageData.json5"), Language[].class);
	    for(final var lang : langs) {
	        TranslationLookup.langs.put(lang.name, lang);
	    }
	} catch (FileNotFoundException e) {
	    logger.atSevere().withCause(e).log("Failed to load language data");
	}
    }

    public static Collection<Language> langs() {
        return langs.values();
    }

    public static String get(final String tag) {
        final String str = langs.get(GlobalSettings.LANG.get()).get(tag);
        return str != null ? str : langs.get("english").translations.getOrDefault(tag, tag);
    }
}
