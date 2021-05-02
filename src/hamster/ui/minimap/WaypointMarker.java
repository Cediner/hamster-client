package hamster.ui.minimap;

import hamster.GlobalSettings;
import haven.*;

import java.awt.*;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class WaypointMarker extends CustomMarker {
    public final long id;
    public final Set<Long> links;

    public WaypointMarker(final long seg, final Coord tc, final String nm,
			  final Color color, final Resource.Spec res,
			  final long id) {
	super(seg, tc, nm, color, res);
	this.id = id;
	this.links = new HashSet<>();
    }


    public WaypointMarker(final long seg, final Coord tc, final String nm,
			  final Color color, final Resource.Spec res,
			  final long id, final Set<Long> links) {
	super(seg, tc, nm, color, res);
	this.id = id;
	this.links = links;
    }

    @Override
    public String tip(UI ui) {
	if(!GlobalSettings.DEBUG.get())
	    return String.format("%s [%d]", nm, id);
	else {
	    return String.format("%s [%d] (%s)", nm, id, links);
	}
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
    public void draw(GOut g, Coord c, Text tip, float scale, final MapFile file) {
	super.draw(g, c, tip, scale, file);
	//Draw the links between us and other markers if we have any
	if(!file.lock.readLock().tryLock())
	    return;
	try {
	    for (final var link : links) {
		final var marker = file.wmarkers.get(link);
		if(marker != null) {
		    final var mc = c.add(marker.tc.sub(tc).div(scale));
		    g.chcolor(Color.red);
		    g.dottedline(c, mc, 1);
		    g.chcolor();
		}
	    }
	} finally {
	    file.lock.readLock().unlock();
	}
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
