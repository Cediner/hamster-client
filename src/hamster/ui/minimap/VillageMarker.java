package hamster.ui.minimap;

import hamster.GlobalSettings;
import hamster.data.map.MarkerData;
import haven.*;

import java.util.Objects;

// Same concept of the Realm Marker but for Village Objects
public class VillageMarker extends Marker {
    //either vidol or banner
    public final Resource.Spec res;
    public String village;

    public VillageMarker(long seg, Coord tc, String nm, Resource.Spec res, String village) {
	super(seg, tc, nm);
	this.res = res;
	this.village = village;
    }

    @Override
    public String tip(final UI ui) {
	return String.format("[%s] %s", village, nm);
    }

    @Override
    public void draw(GOut g, Coord c, Text tip, final float scale, final MapFile file) {
	try {
	    final var res = MapFile.loadsaved(Resource.remote(), this.res);
	    final var img = res.layer(Resource.imgc);
	    final var sz = img.tex().sz();
	    final var cc = sz.div(2);
	    final Coord ul = c.sub(cc);
	    g.image(img.tex(), ul);
	    if (GlobalSettings.SHOWVMARKERRAD.get()) {
		final int offset, isz;
		if (nm.equals("Idol")) {
		    offset = 50;
		    isz = 101;
		} else {
		    //Banner
		    offset = 30;
		    isz = 61;
		}

		g.chcolor(MarkerData.getVillageColor(village));
		g.frect(c.sub(new Coord(offset, offset).div(scale)), new Coord(isz, isz).div(scale));
		g.chcolor();
	    }
	    if (GlobalSettings.SHOWMMMARKERNAMES.get() && GlobalSettings.SHOWVMARKERTIPS.get()) {
		final Coord tipc = new Coord(ul.x + img.tex().sz().x / 2 - tip.sz().x / 2, ul.y - tip.sz().y);
		g.chcolor(MarkerData.getVillageBoldColor(village));
		g.image(tip.tex(), tipc);
		g.chcolor();
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
    public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;
	if (!super.equals(o)) return false;
	VillageMarker that = (VillageMarker) o;
	return res.equals(that.res) && village.equals(that.village);
    }

    @Override
    public int hashCode() {
	return Objects.hash(super.hashCode(), res, village);
    }
}
