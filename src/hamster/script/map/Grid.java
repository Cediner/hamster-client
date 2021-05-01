package hamster.script.map;

import com.google.common.flogger.FluentLogger;
import haven.*;
import haven.resutil.Ridges;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;

import static haven.MCache.cmaps;

public class Grid {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private final MCache mc;
    public final Coord gc, ul;
    public final int[] tiles;
    public final float[] zmap;


    public Grid(final MCache mc, final MCache.Grid from) {
        this.mc = mc;
        gc = from.gc;
        ul = from.ul;
        tiles = new int[from.tiles.length];
        System.arraycopy(from.tiles, 0, tiles, 0, from.tiles.length);
    	zmap = new float[from.z.length];
    	System.arraycopy(from.z, 0, zmap, 0, from.z.length);
    }

    public int gettile(Coord c) {
	return(tiles[c.x + (c.y * cmaps.x)] & 0xff);
    }

    public double getfz(Coord c) {
	return(zmap[c.x + (c.y * cmaps.x)]);
    }

    private BufferedImage tiletex(int t, BufferedImage[] texes, boolean[] cached) {
	if(!cached[t]) {
	    Resource r = null;
	    try {
		r = mc.tilesetr(t);
	    } catch(Loading l) {
		throw(l);
	    } catch(Exception e) {
		logger.atSevere().withCause(e).log("could not load tileset resource %d", t);
	    }
	    if(r != null) {
		Resource.Image ir = r.layer(Resource.imgc);
		if(ir != null) {
		    texes[t] = ir.img;
		}
	    }
	    cached[t] = true;
	}
	return(texes[t]);
    }

    private Tiler tiler(int t, Tiler[] tilers, boolean[] cached) {
	if (!cached[t]) {
	    tilers[t] = mc.tiler(t);
	    cached[t] = true;
	}
	return tilers[t];
    }

    private static final Coord[] tecs = {
	    new Coord(0, -1),
	    new Coord(1, 0),
	    new Coord(0, 1),
	    new Coord(-1, 0)
    };
    private static final Coord[] tccs = {
	    new Coord(0, 0),
	    new Coord(1, 0),
	    new Coord(1, 1),
	    new Coord(0, 1)
    };

    private boolean brokenp(Tiler t, Coord tc, final Tiler[] tilers, final boolean[] tlcache) {
	double bz = ((Ridges.RidgeTile) t).breakz();  //The distance at which a ridge is formed
	//Look at the four tiles around us to get the minimum break distance
	for (Coord ec : tecs) {
	    t = tiler(gettile(tc.add(ec)), tilers, tlcache);
	    if (t instanceof Ridges.RidgeTile)
		bz = Math.min(bz, ((Ridges.RidgeTile) t).breakz());
	}

	//Now figure out based on other tiles around us if we hit that break limit and should be a ridge
	for (int i = 0; i < 4; i++) {
	    final double z1 = getfz(tc.add(tccs[i]));
	    final double z2 = getfz(tc.add(tccs[(i + 1) % 4]));
	    if (Math.abs(z2 - z1) > bz) {
		return (true);
	    }
	}
	return (false);
    }

    public BufferedImage rendergrid() {
	BufferedImage[] texes = new BufferedImage[256];
	boolean[] cached = new boolean[256];
	WritableRaster buf = PUtils.imgraster(cmaps);
	Coord c = new Coord();
	for(c.y = 0; c.y < cmaps.y; c.y++) {
	    for(c.x = 0; c.x < cmaps.x; c.x++) {
		int t = gettile(c);
		BufferedImage tex = tiletex(t, texes, cached);
		int rgb = 0;
		if(tex != null)
		    rgb = tex.getRGB(Utils.floormod(c.x, tex.getWidth()),
			    Utils.floormod(c.y, tex.getHeight()));
		buf.setSample(c.x, c.y, 0, (rgb & 0x00ff0000) >>> 16);
		buf.setSample(c.x, c.y, 1, (rgb & 0x0000ff00) >>>   8);
		buf.setSample(c.x, c.y, 2, (rgb & 0x000000ff));
		buf.setSample(c.x, c.y, 3, (rgb & 0xff000000) >>> 24);
	    }
	}
	for(c.y = 1; c.y < cmaps.y - 1; c.y++) {
	    for(c.x = 1; c.x < cmaps.x - 1; c.x++) {
		int p = gettile(c);
		if((gettile(c.add(-1, 0)) > p) ||
			(gettile(c.add( 1, 0)) > p) ||
			(gettile(c.add(0, -1)) > p) ||
			(gettile(c.add(0,  1)) > p))
		{
		    buf.setSample(c.x, c.y, 0, 0);
		    buf.setSample(c.x, c.y, 1, 0);
		    buf.setSample(c.x, c.y, 2, 0);
		    buf.setSample(c.x, c.y, 3, 255);
		}
	    }
	}
	//Check ridges
	Tiler[] tilers = new Tiler[256];
	boolean[] tlcached = new boolean[256];
	for (c.y = 1; c.y < MCache.cmaps.y - 1; ++c.y) {
	    for (c.x = 1; c.x < MCache.cmaps.x - 1; ++c.x) {
		final Tiler t = tiler(gettile(c), tilers, tlcached);
		if (t instanceof Ridges.RidgeTile && brokenp(t, c, tilers, tlcached)) {
		    for (int y = c.y - 1; y <= c.y + 1; ++y) {
			for (int x = c.x - 1; x <= c.x + 1; ++x) {
			    Color cc = new Color(buf.getSample(x, y, 0), buf.getSample(x, y, 1),
				    buf.getSample(x, y, 2), buf.getSample(x, y, 3));
			    final Color blended = Utils.blendcol(cc, Color.BLACK, x == c.x && y == c.y ? 1.0 : 0.1);
			    buf.setSample(x, y, 0, blended.getRed());
			    buf.setSample(x, y, 1, blended.getGreen());
			    buf.setSample(x, y, 2, blended.getBlue());
			    buf.setSample(x, y, 3, blended.getAlpha());
			}
		    }
		}
	    }
	}
	return(PUtils.rasterimg(buf));
    }
}
