package hamster.ui.minimap;

import haven.Coord;
import haven.MapFile;
import haven.Resource;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class WaypointMarker extends CustomMarker {
    public final long id;
    public final List<Long> links;

    public WaypointMarker(final long seg, final Coord tc, final String nm,
			  final Color color, final Resource.Spec res,
			  final long id) {
	super(seg, tc, nm, color, res);
	this.id = id;
	this.links = new ArrayList<>();
    }


    public WaypointMarker(final long seg, final Coord tc, final String nm,
			  final Color color, final Resource.Spec res,
			  final long id, final List<Long> links) {
	super(seg, tc, nm, color, res);
	this.id = id;
	this.links = links;
    }

    @Override
    public char identifier() {
	return 'w';
    }

    @Override
    public int version() {
	return 0;
    }

    @Override
    public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;
	if (!super.equals(o)) return false;
	WaypointMarker that = (WaypointMarker) o;
	return id == that.id && links.equals(that.links);
    }

    @Override
    public int hashCode() {
	return Objects.hash(super.hashCode(), id, links);
    }
}
