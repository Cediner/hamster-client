package hamster.ui.minimap;

import hamster.GlobalSettings;
import hamster.data.map.MarkerData;
import haven.*;

import java.util.Objects;

// Special marker for Realm objects. These will have a name associated with them that can be user defined
// or auto-defined based off what realm you're in at the time if possible. Realm Markers also have an
// associated radius that can be displayed on the minimap
public class RealmMarker extends Marker {
    public final Resource.Spec res;
    public String realm;

    public RealmMarker(long seg, Coord tc, String nm, Resource.Spec res, String realm) {
	super(seg, tc, nm);
	this.res = res;
	this.realm = realm;
    }

    @Override
    public String tip(final UI ui) {
	return String.format("[%s] %s", realm, nm);
    }

    @Override
    public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;
	if (!super.equals(o)) return false;
	RealmMarker that = (RealmMarker) o;
	return res.equals(that.res) && realm.equals(that.realm);
    }

    @Override
    public void draw(GOut g, Coord c, Text tip, final float scale) {
        try {
            final var res = MapFile.loadsaved(Resource.remote(), this.res);
            final var img = res.layer(Resource.imgc);
	    final var sz = img.tex().sz();
	    final var cc = sz.div(2);
	    final Coord ul = c.sub(cc);
	    g.image(img.tex(), ul);
	    if (GlobalSettings.SHOWKMARKERRAD.get()) {
		g.chcolor(MarkerData.getRealmColor(realm));
		g.frect(c.sub(new Coord(250, 250).div(scale)), new Coord(500, 500).div(scale));
		g.chcolor();
	    }
	    if (GlobalSettings.SHOWMMMARKERNAMES.get()) {
		final Coord tipc = new Coord(ul.x + img.tex().sz().x / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
		g.image(tip.tex(), tipc);
	    }
	} catch (Loading ignored) {}
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
	return Objects.hash(super.hashCode(), res, realm);
    }
}
