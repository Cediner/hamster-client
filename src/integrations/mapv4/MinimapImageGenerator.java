package integrations.mapv4;

import haven.*;

import java.awt.*;
import java.awt.image.BufferedImage;

/**
 * @author APXEOLOG (Artyom Melnikov), at 28.01.2019
 */
public class MinimapImageGenerator {

    private static BufferedImage tileimg(MapSource m, BufferedImage[] texes, int t) {
	BufferedImage img = texes[t];
	if (img == null) {
	    Tileset set = m.tileset(t);
	    if (set == null)
		return (null);
	    Resource r = set.getres();
	    Resource.Image ir = r.layer(Resource.imgc);
	    if (ir == null)
		return (null);
	    img = ir.img;
	    texes[t] = img;
	}
	return (img);
    }

    public static BufferedImage drawmap(MapSource m, Area a) {
	Coord sz = a.sz();
	BufferedImage[] texes = new BufferedImage[256];
	BufferedImage buf = TexI.mkbuf(sz);
	Coord c = new Coord();
	for (c.y = 0; c.y < sz.y; c.y++) {
	    for (c.x = 0; c.x < sz.x; c.x++) {
		int t = m.gettile(a.ul.add(c));
		if (t < 0) {
		    buf.setRGB(c.x, c.y, 0);
		    continue;
		}
		BufferedImage tex = tileimg(m, texes, t);
		int rgb = 0;
		if (tex != null)
		    rgb = tex.getRGB(Utils.floormod(c.x + a.ul.x, tex.getWidth()),
			    Utils.floormod(c.y + a.ul.y, tex.getHeight()));
		buf.setRGB(c.x, c.y, rgb);
	    }
	}
	for (c.y = 1; c.y < sz.y - 1; c.y++) {
	    for (c.x = 1; c.x < sz.x - 1; c.x++) {
		int t = m.gettile(a.ul.add(c));
		if (t < 0)
		    continue;
		Tiler tl = m.tiler(t);
		if (tl instanceof haven.resutil.Ridges.RidgeTile) {
		    if (haven.resutil.Ridges.brokenp(m, a.ul.add(c))) {
			for (int y = c.y - 1; y <= c.y + 1; y++) {
			    for (int x = c.x - 1; x <= c.x + 1; x++) {
				Color cc = new Color(buf.getRGB(x, y));
				buf.setRGB(x, y, Utils.blendcol(cc, Color.BLACK, ((x == c.x) && (y == c.y)) ? 1 : 0.1).getRGB());
			    }
			}
		    }
		}
	    }
	}
	for (c.y = 0; c.y < sz.y; c.y++) {
	    for (c.x = 0; c.x < sz.x; c.x++) {
		int t = m.gettile(a.ul.add(c));
		if ((m.gettile(a.ul.add(c).add(-1, 0)) > t) ||
			(m.gettile(a.ul.add(c).add(1, 0)) > t) ||
			(m.gettile(a.ul.add(c).add(0, -1)) > t) ||
			(m.gettile(a.ul.add(c).add(0, 1)) > t))
		    buf.setRGB(c.x, c.y, Color.BLACK.getRGB());
	    }
	}
	return (buf);
    }
}
