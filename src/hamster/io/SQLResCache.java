package hamster.io;

import com.google.common.flogger.FluentLogger;
import haven.ResCache;

import java.io.*;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class SQLResCache implements ResCache {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final SQLResCache resdb, mapdb;
    static {
        resdb = new SQLResCache("jdbc:sqlite:data/res.sqlite");
        mapdb = new SQLResCache("jdbc:sqlite:data/map.sqlite");
        Runtime.getRuntime().addShutdownHook(new Thread() {
            public void run() {
                resdb.close();
                mapdb.close();
            }
        });
    }

    private final Storage db;
    private final PreparedStatement upsert;
    private final PreparedStatement fetch;

    public SQLResCache(final String jdbc) {
        db = Storage.create(jdbc)
                .orElseThrow(() -> new RuntimeException("Failed to open map database"));
        db.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("""
                        CREATE TABLE IF NOT EXISTS resource (
                            name TEXT PRIMARY KEY,
                            data BLOB NOT NULL
                        )""");
            }
        });

        upsert = db.ensurePrepare("""
                INSERT INTO resource (name, data)
                    VALUES (?, ?)
                    ON CONFLICT (name) DO UPDATE SET
                        data=excluded.data""");
        fetch = db.ensurePrepare("SELECT data FROM resource WHERE name =  ?");
    }

    public void close() {
        db.close();
    }

    @Override
    public OutputStream store(final String name) {
        return new ByteArrayOutputStream() {
            @Override
            public void close() throws IOException {
                super.close();
                final byte[] data = toByteArray();
                db.write(sql -> {
                    upsert.setString(1, name);
                    upsert.setBytes(2, data);
                    upsert.execute();
                });
            }
        };
    }

    @Override
    public synchronized InputStream fetch(final String name) throws IOException {
        final byte[] data;

        try {
            fetch.setString(1, name);
            try (final ResultSet res = fetch.executeQuery()) {
                if (res.next()) {
                    data = res.getBytes(1);
                } else {
                    data = null;
                }
            }
        } catch (SQLException se) {
            logger.atSevere().withCause(se).log("Failed to load resource [%s]", name);
            throw new IOException("Failed to load resource from resdb", se);
        }

        if (data != null) {
            return new ByteArrayInputStream(data);
        } else {
            throw(new FileNotFoundException(name));
        }
    }
}
