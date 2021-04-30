package hamster.ui.minimap;

import hamster.GlobalSettings;
import haven.*;

import java.util.Objects;

public class SMarker extends Marker {
    public final long oid;
    public final Resource.Spec res;

    public SMarker(long seg, Coord tc, String nm, long oid, Resource.Spec res) {
	super(seg, tc, nm);
	this.oid = oid;
	this.res = res;
    }

    @Override
    public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;
	if (!super.equals(o)) return false;
	SMarker sMarker = (SMarker) o;
	return oid == sMarker.oid && res.equals(sMarker.res);
    }

    @Override
    public void draw(GOut g, Coord c, Text tip, final float scale, final MapFile file) {
        try {
            final var res = this.res.loadsaved(Resource.remote());
            final var img = res.layer(Resource.imgc);
            final var neg = res.layer(Resource.negc);
	    final var cc = neg != null ? neg.cc : img.ssz.div(2);
	    final Coord ul = c.sub(cc);
	    g.image(img, ul);
	    if (GlobalSettings.SHOWMMMARKERNAMES.get()) {
		final Coord tipc = new Coord(ul.x + img.ssz.x / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
		g.image(tip.tex(), tipc);
	    }
	} catch (Loading ignored) {}
    }

    @Override
    public Area area() {
        try {
	    final var res = this.res.loadsaved(Resource.remote());
	    final var img = res.layer(Resource.imgc);
	    final var neg = res.layer(Resource.negc);
	    final var cc = neg != null ? neg.cc : img.ssz.div(2);
	    return Area.sized(cc.inv(), img.ssz);
	} catch (Loading ignored) {
            return null;
	}
    }

    @Override
    public int hashCode() {
	return Objects.hash(super.hashCode(), oid, res);
    }
}
