package hamster.script.map;

import haven.*;

import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

public class MapExport {
    private final Resource markres = Resource.local().loadwait("custom/mm/pl/unknown");
    private final Map<Coord, Grid> gridmap = new HashMap<>();
    private final Coord ul, br;

    public MapExport(final MCache mc) {
        int mx = Integer.MAX_VALUE, my = Integer.MAX_VALUE;
        int bx = Integer.MIN_VALUE, by = Integer.MIN_VALUE;
        for(final var grid : mc.grids()) {
            final var xgrid = new Grid(mc, grid);
            gridmap.put(xgrid.ul, xgrid);
            mx = Math.min(mx, xgrid.ul.x);
	    bx = Math.max(bx, xgrid.ul.x + MCache.cmaps.x);
	    my = Math.min(my, xgrid.ul.y);
	    by = Math.max(by, xgrid.ul.y + MCache.cmaps.y);
	}
        ul = new Coord(mx, my);
        br = new Coord(bx, by);
    }

    public BufferedImage rendermap() {
        final var img = TexI.mkbuf(br.sub(ul));
        final var g = img.createGraphics();
        for(final var grid : gridmap.values()) {
            final var c = grid.ul.sub(ul);
            g.drawImage(grid.rendergrid(), c.x, c.y, null);
	}
        g.dispose();
        return img;
    }

    public BufferedImage renderWithMarkAt(final Coord2d markc, final double a) {
        final var mark = markc.floor(MCache.tilesz);
        final var img = rendermap();
        final var g = img.createGraphics();
        final var imgsz = markres.layer(Resource.imgc).sz.div(2);
        final var offset = mark.sub(ul).sub(imgsz);
        if(offset.x > 0 && offset.y > 0 && offset.x < img.getWidth() && offset.y < img.getHeight()) {
            final var tx = AffineTransform.getRotateInstance(a, imgsz.x, imgsz.y);
            final var op = new AffineTransformOp(tx, AffineTransformOp.TYPE_BILINEAR);
            g.drawImage(op.filter(markres.layer(Resource.imgc).img, null), offset.x, offset.y, null);
        }
        g.dispose();
        return img;
    }
}
