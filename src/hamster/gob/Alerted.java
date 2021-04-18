package hamster.gob;

import com.google.common.flogger.FluentLogger;
import hamster.GlobalSettings;
import hamster.io.Storage;
import hamster.util.ObservableMap;
import hamster.util.ObservableMapListener;
import haven.*;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;

public class Alerted {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final List<Resource.Named> sounds = new ArrayList<>();
    private static final ObservableMap<String, Resource.Named> sfxmap = new ObservableMap<>(new TreeMap<>());

    public static void init() {
        logger.atInfo().log("Loading sound data");
        Storage.dynamic.ensure((sql) -> {
            try (final Statement stmt = sql.createStatement()) {
                stmt.executeUpdate("CREATE TABLE IF NOT EXISTS gob_sound ( name TEXT PRIMARY KEY, sfx TEXT )");
            }
        });
        Storage.dynamic.ensure((sql) -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT name, sfx FROM gob_sound")) {
                    while (res.next()) {
                        sfxmap.put(res.getString(1),
                                Resource.remote().load(res.getString(2)));
                    }
                }
            }
        });
        final Path  dir = Path.of("data/res/custom/sfx/");
        if(Files.exists(dir)) {
            try {
                Files.walkFileTree(dir, new SimpleFileVisitor<>() {
                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                        if(file.toString().endsWith(".res")) {
                            sounds.add(Resource.remote().load(file.toString().substring("data/res/".length(), file.toString().length()-4)));
                        }
                        return FileVisitResult.CONTINUE;
                    }
                });
            } catch (IOException e) {
                logger.atSevere().withCause(e).log("Failed to load sound data");
                System.exit(1);
            }
            sounds.sort(Comparator.comparing(Resource.Named::name));
        }
    }

    public static void init(final Storage internal) {
        internal.ensure((sql) -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT res FROM alert_files")) {
                    while (res.next()) {
                        sounds.add(Resource.remote().load(res.getString(1)));
                    }
                }
            }
            sounds.sort(Comparator.comparing(Resource.Named::name));
        });

        for (final Resource.Named sound : sounds) {
            try {
                Resource.remote().loadwait(sound.name);
            } catch (Exception e) {
                //Ignore it
                logger.atSevere().withCause(e).log("Failed to load %s", sound);
            }
        }
    }

    public static synchronized void remove(final String name) {
        sfxmap.remove(name);
        Storage.dynamic.write(sql -> {
            final PreparedStatement stmt = Storage.dynamic.prepare("DELETE FROM gob_sound WHERE name = ?");
            stmt.setString(1, name);
            stmt.executeUpdate();
        });
    }

    public static synchronized void add(final String name, final Resource.Named sound) {
        if (!(sfxmap.containsKey(name) && sfxmap.get(name).equals(sound))) {
            //Only update if we have to.
            sfxmap.put(name, sound);
            Storage.dynamic.write(sql -> {
                final PreparedStatement stmt = Storage.dynamic.prepare("INSERT OR REPLACE INTO gob_sound VALUES (?, ?)");
                stmt.setString(1, name);
                stmt.setString(2, sound.name);
                stmt.executeUpdate();
            });
        }
    }

    public synchronized static void listen(final ObservableMapListener<String, Resource.Named> listener) {
        sfxmap.addListener(listener);
    }

    public synchronized static void unlisten(final ObservableMapListener<String, Resource.Named> listener) {
        sfxmap.removeListener(listener);
    }

    public static void checkAlert(final String name, final long plgob, final Gob g, final UI ui) {
        if (sfxmap.containsKey(name)) {
            if (!name.equals("gfx/borka/body")) {
                ui.sfx(sfxmap.get(name),  (GlobalSettings.ALERTVOL.get() / 1000f));
                g.glob.lastAlert = System.currentTimeMillis();
            } else if (plgob != -1 && g.id != plgob) {
                //For bodies only play on unknown or RED or village/realm member that you don't have kinned
                final KinInfo kin = g.getattr(KinInfo.class);
                final GobHealth hp = g.getattr(GobHealth.class);
                if ( hp == null && (kin == null || kin.group == GlobalSettings.BADKIN.get() ||
                        (kin.isVillager() && (kin.name == null || kin.name.equals("") || kin.name.equals(" "))))) {
                    ui.sfx(sfxmap.get(name), (GlobalSettings.ALERTVOL.get() / 1000f));
                    g.glob.lastAlert = System.currentTimeMillis();
                }
            }
        }
    }

    public static boolean shouldAlert(final String name) {
        return sfxmap.containsKey(name);
    }
}
