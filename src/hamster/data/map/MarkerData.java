package hamster.data.map;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;
import hamster.io.Storage;
import haven.MapFile;

import java.awt.*;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.*;

public class MarkerData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static class Marker {
        public final String defname;
        public final String res;
        public final Type type;

        public Marker(final String defname, final String res, final Type type) {
            this.defname = defname;
            this.res = res;
            this.type = type;
        }
    }

    public static class LinkedMarker extends Marker {
        public final byte ltype;

        public LinkedMarker(final String defname, final String res, final Type type, final byte ltype) {
            super(defname, res, type);
            this.ltype = ltype;
        }
    }

    /* Used only to import config data */
    private static class MarkerDataImport {
        public Marker script;
        public Map<String, Marker> basic;
        public Map<String, LinkedMarker> linked;
    }

    public enum Type {
        LINKED, CUSTOM, REALM, VILLAGE, SCRIPT
    }

    private static final Map<String, Marker> markable = new HashMap<>();
    public static Marker scriptmarker;

    public static void init() {
        logger.atInfo().log("Loading Custom Marker Data");
        final var gson = new Gson();
        try {
            final var markers = gson.fromJson(new FileReader("data/MarkerData.json5"), MarkerDataImport.class);
            scriptmarker = markers.script;
            for(final var marker : markers.basic.keySet()) {
                markable.put(marker, markers.basic.get(marker));
            }
            for(final var marker : markers.linked.keySet()) {
                markable.put(marker, markers.linked.get(marker));
            }
        } catch (FileNotFoundException e) {
            logger.atSevere().withCause(e).log("Failed to load custom marker data");
            System.exit(1);
        }
    }

    public static Optional<Marker> marker(final String name) {
        return Optional.ofNullable(markable.get(name));
    }

    private static final HashMap<String, Integer> realmcolors = new HashMap<>();
    private static final HashMap<String, Integer> villagecolors = new HashMap<>();
    private static final Color[] colors = {
            new Color(255, 255, 255, 90),
            new Color(0, 255, 0, 90),
            new Color(255, 0, 0, 90),
            new Color(0, 0, 255, 90),
            new Color(0, 255, 255, 90),
            new Color(255, 255, 0, 90),
            new Color(255, 0, 255, 90),
            new Color(255, 0, 128, 90),
    };
    private static final Color[] bcolors = {
            new Color(255, 255, 255),
            new Color(0, 255, 0),
            new Color(255, 0, 0),
            new Color(0, 0, 255),
            new Color(0, 255, 255),
            new Color(255, 255, 0),
            new Color(255, 0, 255),
            new Color(255, 0, 128),
    };


    static {
        realmcolors.put("???", 2);
        villagecolors.put("???", 2);

        Storage.dynamic.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS realm_markers ( name TEXT PRIMARY KEY, color INTEGER)");
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS village_markers ( name TEXT PRIMARY KEY, color INTEGER)");
                try (final ResultSet res = stmt.executeQuery("SELECT name, color FROM realm_markers")) {
                    while (res.next()) {
                        realmcolors.put(res.getString(1), res.getInt(2));
                    }
                }
                try (final ResultSet res = stmt.executeQuery("SELECT name, color FROM village_markers")) {
                    while (res.next()) {
                        villagecolors.put(res.getString(1), res.getInt(2));
                    }
                }
            }
        });
    }

    public static void setRealmColor(final String realm, final int ind) {
        realmcolors.put(realm, ind);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO realm_markers VALUES (?, ?)");
            stmt.setString(1, realm);
            stmt.setInt(2, ind);
            stmt.executeUpdate();
        });
    }

    public static Color getRealmColor(final String realm) {
        return colors[realmcolors.getOrDefault(realm, 2)];
    }

    public static int getRealmColorID(final String realm) {
        return realmcolors.getOrDefault(realm, 2);
    }

    public static void setVillageColor(final String village, final int ind) {
        villagecolors.put(village, ind);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO village_markers VALUES (?, ?)");
            stmt.setString(1, village);
            stmt.setInt(2, ind);
            stmt.executeUpdate();
        });
    }

    public static Color getVillageColor(final String village) {
        return colors[villagecolors.getOrDefault(village, 2)];
    }
    public static Color getVillageBoldColor(final String village) {
        return bcolors[villagecolors.getOrDefault(village, 2)];
    }

    public static int getVillageColorID(final String village) {
        return villagecolors.getOrDefault(village, 2);
    }
}
