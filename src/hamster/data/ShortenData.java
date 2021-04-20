package hamster.data;

import com.google.common.flogger.FluentLogger;
import hamster.io.Storage;
import hamster.util.ObservableMap;
import hamster.util.ObservableMapListener;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Optional;

public class ShortenData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final ObservableMap<String, Float> shorten = new ObservableMap<>(new HashMap<>());

    public static void init() {
	logger.atInfo().log("Loading Shorten Data");
	Storage.dynamic.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_shorten ( name TEXT PRIMARY KEY, scale REAL)");
	    }
	});
	Storage.dynamic.ensure(sql -> {
	    try (final Statement stmt = sql.createStatement()) {
		try (final ResultSet res = stmt.executeQuery("SELECT name, scale FROM gob_shorten")) {
		    while (res.next()) {
			shorten.put(res.getString(1), res.getFloat(2));
		    }
		}
	    }
	});
    }

    public static Optional<Float> getShortenScaler(final String name) {
        return Optional.ofNullable(shorten.get(name));
    }

    public synchronized static void listen(final ObservableMapListener<String, Float> listener) {
	shorten.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableMapListener<String, Float> listener) {
	shorten.removeListener(listener);
    }

    public synchronized static void add(final String name, final float scaler) {
	shorten.put(name, scaler);
	Storage.dynamic.write(sql -> {
	    final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO gob_shorten VALUES (?, ?)");
	    stmt.setString(1, name);
	    stmt.setFloat(2, scaler);
	    stmt.executeUpdate();
	});
    }

    public synchronized static void rem(final String name) {
	shorten.remove(name);
	Storage.dynamic.write(sql -> {
	    final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_shorten WHERE name = ?");
	    stmt.setString(1, name);
	    stmt.executeUpdate();
	});
    }
}
