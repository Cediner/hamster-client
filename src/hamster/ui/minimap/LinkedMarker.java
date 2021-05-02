package hamster.ui.minimap;

import haven.Coord;
import haven.MapFile;
import haven.Resource;

import java.awt.*;
import java.util.Objects;

public class LinkedMarker extends CustomMarker {
    // Linked Marker data for Mineholes, ladders, caves to link between maps.
    public static final long NOLINK = -1;
    public static final byte MINEHOLE = (byte) 0;
    public static final byte LADDER = (byte) 1;
    public static final byte CAVE = (byte) 2;
    public static final byte CAVEIN = (byte) 3;

    public static boolean canLink(final byte l1, final byte l2) {
	return (l1 == CAVE && l2 == CAVEIN) ||
		(l1 == CAVEIN && l2 == CAVE) ||
		(l1 == MINEHOLE && l2 == LADDER) ||
		(l1 == LADDER && l2 == MINEHOLE);
    }

    public final byte type;
    public final long id;
    public long lid; //id of the marker we link to

    public LinkedMarker(long seg, Coord tc, String nm, Color color, Resource.Spec res, long id, byte type) {
	super(seg, tc, nm, color, res);
	this.id = id;
	this.type = type;
	lid = NOLINK;
    }

    public LinkedMarker(long seg, Coord tc, String nm, Color color, Resource.Spec res,
			long id, byte type, long lid) {
	super(seg, tc, nm, color, res);
	this.id = id;
	this.type = type;
	this.lid = lid;
    }

    @Override
    public char identifier() {
	return 'l';
    }

    @Override
    public int version() {
	return 2;
    }

    @Override
    public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;
	if (!super.equals(o)) return false;
	LinkedMarker that = (LinkedMarker) o;
	return type == that.type && id == that.id && lid == that.lid;
    }

    @Override
    public int hashCode() {
	return Objects.hash(super.hashCode(), type, id, lid);
    }
}
