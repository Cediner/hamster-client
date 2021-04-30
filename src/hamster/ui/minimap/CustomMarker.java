package hamster.ui.minimap;

import hamster.GlobalSettings;
import haven.*;

import java.awt.*;
import java.util.Objects;

// Simple custom icons that ae a combo of PMarker (color) and SMarker (Custom res)
public class CustomMarker extends Marker {
    public Color color;
    public final Resource.Spec res;

    public CustomMarker(final long seq, final Coord tc, final String nm,
			final Color color, final Resource.Spec res) {
	super(seq, tc, nm);
	this.color = color;
	this.res = res;
    }

    public char identifier() {
	return 'r';
    }

    public int version() {
	return 1;
    }

    @Override
    public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;
	if (!super.equals(o)) return false;
	CustomMarker that = (CustomMarker) o;
	return color.equals(that.color) && res.equals(that.res);
    }

    @Override
    public void draw(GOut g, Coord c, Text tip, final float scale) {
	g.chcolor(color);
	try {
	    final var res = MapFile.loadsaved(Resource.remote(), this.res);
	    final var img = res.layer(Resource.imgc);
	    final var cc = img.ssz.div(2);
	    final Coord ul = c.sub(cc);
	    g.image(img.tex(), ul);
	    if (GlobalSettings.SHOWMMMARKERNAMES.get()) {
		final Coord tipc = new Coord(ul.x + img.tex().sz().x / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
		g.image(tip.tex(), tipc);
	    }
	} catch (Loading ignored) {}
	g.chcolor();
    }

    @Override
    public Area area() {
	try {
	    final var res = this.res.loadsaved(Resource.remote());
	    final var img = res.layer(Resource.imgc);
	    final Coord sz = img.tex().sz();
	    return Area.sized(sz.div(2).inv(), sz);
	} catch (Loading ignored) {
	    return null;
	}
    }

    @Override
    public int hashCode() {
	return Objects.hash(super.hashCode(), color, res);
    }
}
