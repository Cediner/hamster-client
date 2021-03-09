package hamster.script.pathfinding;

import haven.Coord;
import haven.Coord2d;
import haven.Gob;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.PathIterator;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.*;

/**
 * A hitmap for Gobs
 * Only contains gobs that can be hit
 *
 * TODO: Gobs should be broken up into grids similar to how SuperGrid > Grid > Minimaps work to reduce
 *       how many gobs we need to compare again as it can be a bit of a delay with many hundreds of gobs.
 */
public class GobHitmap {
    private static final List<Gob> gobs = new ArrayList<>();
    private static final Map<Gob, java.awt.geom.Area> hbmap = new HashMap<>();

    public GobHitmap() { }

    public synchronized void add(final Gob g) {
        final Hitbox hb = g.hitbox();
        if(hb != null && hb.canHit()) {
            gobs.add(g);
            final Area ar = hb.hitbox.createTransformedArea(AffineTransform.getRotateInstance(g.a));
            ar.transform(AffineTransform.getTranslateInstance(g.rc.x, g.rc.y));
            hbmap.put(g, ar);
        }
    }

    public synchronized void remove(final Gob g) {
        gobs.remove(g);
        hbmap.remove(g);
    }

    public synchronized boolean checkHit(final Hitbox hb, final Coord c, final double a) {
        final var hbarea = hb.hitbox.createTransformedArea(AffineTransform.getRotateInstance(a));
        hbarea.transform(AffineTransform.getTranslateInstance(c.x, c.y));
        final var bound = hbarea.getBounds2D();
        for(final var g : gobs) {
            if(!g.blackout()) {
                final var area = hbmap.get(g);
                if (area.intersects(bound))
                    return true;
            }
        }
        return false;
    }

    public synchronized BufferedImage debug2(final Coord tl, final Coord br) {
        final List<Polygon> polys = new ArrayList<>();
        //Update tl/br if needed
        for(final var g : gobs) {
            if(!g.blackout()) {
                final var area = hbmap.get(g);
                final var path = area.getPathIterator(new AffineTransform());
                final List<List<Coord2d>> shapes = new ArrayList<>();
                List<Coord2d> coords = new ArrayList<>();

                // Get the coordinates of each polygon broken by SEG_MOVETO
                while(!path.isDone()) {
                    final double[] points = new double[6];
                    final var type = path.currentSegment(points);
                    switch (type) {
                        case PathIterator.SEG_MOVETO -> {
                            shapes.add(coords);
                            coords = new ArrayList<>();
                            coords.add(new Coord2d(points[0], points[1]));
                        }
                        case PathIterator.SEG_LINETO -> coords.add(new Coord2d(points[0], points[1]));
                        case PathIterator.SEG_QUADTO -> {
                            coords.add(new Coord2d(points[0], points[1]));
                            coords.add(new Coord2d(points[2], points[3]));
                        }
                        case PathIterator.SEG_CUBICTO -> {
                            coords.add(new Coord2d(points[0], points[1]));
                            coords.add(new Coord2d(points[2], points[3]));
                            coords.add(new Coord2d(points[4], points[5]));
                        }
                        case PathIterator.SEG_CLOSE -> shapes.add(coords);
                    }
                    path.next();
                }

                // Separate them out into polygons and get our top left / bottom right coordinates.
                for(final var shape : shapes) {
                    final Polygon poly = new Polygon();
                    for(final var c : shape) {
                        poly.addPoint((int)c.x, (int)c.y);
                        if (c.x < tl.x)
                            tl.x = (int)Math.ceil(c.x);
                        else if (c.x > br.x)
                            br.x = (int)Math.floor(c.x);

                        if (c.y < tl.y)
                            tl.y = (int)Math.ceil(c.y);
                        else if (c.y > br.y)
                            br.y = (int)Math.floor(c.y);
                    }
                    polys.add(poly);
                }
            }
        }

        final BufferedImage buf = new BufferedImage(br.x - tl.x + 1, br.y - tl.y + 1, BufferedImage.TYPE_INT_RGB);
        final Graphics gfx = buf.getGraphics();

        gfx.setColor(Color.RED);
        for (final Polygon poly : polys) {
            poly.translate(-tl.x, -tl.y);
            gfx.drawPolygon(poly);
        }

        return buf;
    }
}
