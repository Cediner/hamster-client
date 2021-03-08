/* Preprocessed source code */
/* $use: ui/tt/q/qbuff */
package haven.res.ui.tt.q.qbuff;
import haven.*;

import java.awt.Color;

/* >tt: haven.res.ui.tt.q.qbuff.Quality */
public class Quality extends QBuff implements GItem.OverlayInfo<Tex> {
    private static final Color qcol = Color.WHITE;
    private static final Color qolcol = Color.BLACK;
    private static final Color bgcol = new Color(128, 128, 128, 128);
    public static boolean show = false;

    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
	    return new Quality(owner, ((Number) args[1]).doubleValue());
	}
    }

    public Quality(Owner owner, double q) {
	super(owner, Resource.remote().loadwait("ui/tt/q/quality").layer(Resource.Image.class, 0).scaled(),
		"Quality", q);
    }

    public static ItemInfo mkinfo(Owner owner, Object... args) {
	return(new Quality(owner, ((Number)args[1]).doubleValue()));
    }

    @Override
    public Tex overlay() {
	return Text.renderstroked(String.format("%.1f", q), qcol, qolcol).tex();
    }

    @Override
    public void drawoverlay(GOut g, Tex ol) {
	final UI ui = ((GItem) owner).ui;
	if(ui.gui != null && ui.gui.settings.SHOWITEMQ.get()) {
	    final Coord tsz = ol.sz();
	    final Coord c = new Coord(g.sz().x - tsz.x, 0);
	    g.chcolor(bgcol);
	    g.frect(c, tsz);
	    g.chcolor();
	    g.image(ol, c);
	}
    }
}

/* >pagina: ShowQuality$Fac */
