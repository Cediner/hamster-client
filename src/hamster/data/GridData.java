package hamster.data;

import hamster.io.Storage;
import haven.Coord;

import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 * this is for getting global grid coordinates from a source of know grid ids -> (x, y)
 */
public class GridData {
    private static final Storage gridstore;

    static {
        gridstore = Storage.create("jdbc:sqlite:data/static.sqlite")
                .orElseThrow(() -> new RuntimeException("Failed to open static.sqlite"));
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                gridstore.close();
            }
        });
    }


    public static Coord resolve(final long gridid) {
        try {
            final PreparedStatement stmt = gridstore.prepare("SELECT x,y FROM grid WHERE id = ?");
            stmt.setLong(1, gridid);
            try(final ResultSet ret = stmt.executeQuery()) {
                if(ret.next()) {
                    return new Coord(ret.getInt(1), ret.getInt(2));
                }
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }
}
