package haven.res.ui.tt;
/* Preprocessed source code */
import haven.*;
import haven.res.ui.tt.wpn.Armpen;

import java.awt.Color;

/* >tt: Level */
public class Level extends ItemInfo implements GItem.OverlayInfo<Double> {
    public static final Color defcolor = Color.WHITE;
    public static final int h = UI.scale(2);
    public static final int m = UI.scale(1);
    public final double max, cur;
    public final Color color, ocolor;

    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
	    double max = ((Number)args[1]).doubleValue();
	    double cur = ((Number)args[2]).doubleValue();
	    Color color = (args.length > 3) ? (Color)args[3] : null;
	    if(color == null)
		color = defcolor;
	    return new Level(owner, max, cur, color);
	}
    }

    public Level(Owner owner, double max, double cur, Color color) {
	super(owner);
	this.max = max;
	this.cur = cur;
	this.color = color;
	this.ocolor = Color.BLACK;
    }

    public Double overlay() {
	return(cur / max);
    }

    public void drawoverlay(GOut g, Double l) {
	final UI ui = ((GItem) owner).ui;
	if(ui.gui != null && ui.gui.settings.SHOWITEMCONT.get()) {
	    Coord sz = g.sz();
	    g.chcolor(ocolor);
	    g.frect2(new Coord(1 + m, sz.y - 3 - m - h), new Coord(sz.x - 2 - m, sz.y - 1 - m));
	    g.line(new Coord(m, sz.y - 2 - m - h), new Coord(m, sz.y - 2 - m), 1);
	    g.line(new Coord(sz.x - 2 - m, sz.y - 2 - m - h), new Coord(sz.x - 2 - m, sz.y - 2 - m), 1);
	    g.chcolor(this.color);
	    g.frect2(new Coord(1 + m, sz.y - 2 - m - h), new Coord(m + (int) Math.floor(l * (sz.x - 2 - (m * 2))), sz.y - 2 - m));
	    g.chcolor();
	}
    }

    public static ItemInfo mkinfo(Owner owner, Object... args) {
	double max = ((Number)args[1]).doubleValue();
	double cur = ((Number)args[2]).doubleValue();
	Color color = (args.length > 3) ? (Color)args[3] : null;
	if(color == null)
	    color = defcolor;
	return(new Level(owner, max, cur, color));
    }
}
