package hamster.data;

import hamster.io.Storage;

import java.util.HashMap;
import java.util.Map;

/**
 * Additional stored data about Windows
 * This is for:
 *   * Remembering state of hidden window frames
 *   * Remembering state of folded windows
 *
 */
public class WindowData {
    private static final Map<String, Boolean> knownHides = new HashMap<>();
    private static final Map<String, Boolean> knownFolds = new HashMap<>();

    static {
        Storage.dynamic.ensure(sql -> {
            try (final var stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS window_hidden ( name TEXT PRIMARY KEY, hide BOOLEAN, fold BOOLEAN )");
            }
        });
        Storage.dynamic.ensure(sql -> {
            try (final var stmt = sql.createStatement()) {
                try (final var res = stmt.executeQuery("SELECT name, hide, fold FROM window_hidden")) {
                    while (res.next()) {
                        final var name = res.getString(1);
                        final var hide = res.getBoolean(2);
                        final var fold = res.getBoolean(3);
                        knownHides.put(name, hide);
                        knownFolds.put(name, fold);
                    }
                }
            }
        });
    }

    public static boolean shouldHide(final String name) {
        return knownHides.getOrDefault(name, false);
    }

    public static boolean shouldFold(final String name) {
        return knownFolds.getOrDefault(name, false);
    }

    public static void save(final String name, final boolean hide, final boolean fold) {
        knownHides.put(name, hide);
        Storage.dynamic.write(sql -> {
            final var stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO window_hidden VALUES (?, ?, ?)");
            stmt.setString(1, name);
            stmt.setBoolean(2, hide);
            stmt.setBoolean(3, fold);
            stmt.executeUpdate();
        });
    }
}
