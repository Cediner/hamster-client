package hamster.data;

import com.google.common.flogger.FluentLogger;
import hamster.io.Storage;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class MenuExclusionData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final List<String> excluded = new ArrayList<>();

    public static void init() {
	logger.atInfo().log("Loading Menu Exclusions");
	Storage.dynamic.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		stmt.executeUpdate("CREATE TABLE IF NOT EXISTS menu_exclusion ( opt TEXT PRIMARY KEY )");
	    }
	});
	Storage.dynamic.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		try (final ResultSet res = stmt.executeQuery("SELECT opt FROM menu_exclusion")) {
		    while (res.next()) {
			excluded.add(res.getString(1));
		    }
		}
	    }
	});
    }

    public static boolean isExcluded(final String opt) {
        return excluded.contains(opt);
    }

    public static void add(final String excludeopt) {
        if(!excluded.contains(excludeopt)) {
	    excluded.add(excludeopt);
	    Storage.dynamic.write(sql -> {
		final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR IGNORE INTO menu_exclusion VALUES (?)");
		stmt.setString(1, excludeopt);
		stmt.executeUpdate();
	    });
	}
    }

    public static void rem(final String excludeopt) {
        excluded.remove(excludeopt);
	Storage.dynamic.write(sql -> {
	    final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM menu_exclusion WHERE opt = ?");
	    stmt.setString(1, excludeopt);
	    stmt.executeUpdate();
	});
    }

    public static String get(final int idx) {
	return excluded.get(idx);
    }

    public static int size() {
	return excluded.size();
    }
}
