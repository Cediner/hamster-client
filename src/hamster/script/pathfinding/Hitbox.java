package hamster.script.pathfinding;

import hamster.util.ResHashMap;
import haven.*;
import haven.res.gfx.terobjs.consobj.Consobj;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import java.util.List;
import java.util.Optional;

/**
 * Gob Hitbox
 */
public class Hitbox {
    private static final ResHashMap<Hitbox> hitboxes = new ResHashMap<>();
    private static final Hitbox NOHIT;

    static {
        NOHIT = new Hitbox(false);
        hitboxes.put("gfx/terobjs/herbs", NOHIT);
        hitboxes.put("gfx/terobjs/items", NOHIT);
        hitboxes.put("gfx/terobjs/plants", NOHIT);

        //misc
        //hitboxes.put("gfx/terobjs/consobj", new Rectangular(new Coord2d(-4, -4), new Coord2d(8, 8)));
        //hitboxes.put("gfx/terobjs/skeleton", new Rectangular(new Coord2d(-4, -4), new Coord2d(8, 8)));
        //hitboxes.put("gfx/terobjs/smelter", new Rectangular(new Coord2d(-12, -12), new Coord2d(25, 33)));
        hitboxes.put("gfx/terobjs/clue", NOHIT);
        hitboxes.put("gfx/terobjs/boostspeed", NOHIT);
        hitboxes.put("gfx/kritter/jellyfish/jellyfish", NOHIT);

        //stone This looks wrong...
        //hitboxes.put("gfx/terobjs/bumlings", new Rectangular(new Coord2d(8, 8), new Coord2d(-16, -16)));

        //trellis, definitely a decimal so this is an overestimate
        //hitboxes.put("gfx/terobjs/plants/trellis", new Rectangular(new Coord2d(-1, -5), new Coord2d(3, 11)));
    }

    // These are your simple Neg based Hitboxes.
    public static class Rectangular extends Hitbox {
        static {
            //animals
            hitboxes.put("gfx/kritter/horse", new Rectangular(new Coord2d(-8, -4), new Coord2d(16, 8)));
            hitboxes.put("gfx/kritter/cattle/calf", new Rectangular(new Coord2d(-9, -3), new Coord2d(18, 6)));
            hitboxes.put("gfx/kritter/cattle/cattle", new Rectangular(new Coord2d(-12, -4), new Coord2d(24, 8)));
            hitboxes.put("gfx/kritter/pig", new Rectangular(new Coord2d(-6, -3), new Coord2d(12, 6)));
            hitboxes.put("gfx/kritter/goat", new Rectangular(new Coord2d(-6, -2), new Coord2d(12, 4)));
            hitboxes.put("gfx/kritter/sheep/lamb", new Rectangular(new Coord2d(-6, -2), new Coord2d(12, 4)));
            hitboxes.put("gfx/terobjs/boostspeed", new Rectangular(new Coord2d(-3, -3), new Coord2d(6, 6), false));
        }

        private final Coord2d off;
        private final Coord2d sz;

        public Rectangular(final Coord2d off, final Coord2d sz, boolean hitable) {
            super(hitable);
            this.off = off;
            this.sz = sz;
            hitbox.add(new Area(new Rectangle2D.Double(off.x, off.y, sz.x, sz.y)));
        }

        public Rectangular(final Coord2d off, final Coord2d sz) {
            this(off, sz, true);
        }

        public Coord2d offset() {
            return off;
        }

        public Coord2d size() {
            return sz;
        }

        @Override
        public FastMesh mesh() {
            final Coord3f off = new Coord3f((float)this.off.x, (float)this.off.y, 1f);
            final Coord3f sz = new Coord3f((float)this.sz.x, (float)this.sz.y, 0f);

            final Coord3f[][] verts ={
                    { off, off.add(sz.x, 0, 0), off.add(sz), off.add(0, sz.y, 0) }, //Top
                    { off, off.add(sz.x, 0, 0), off.add(sz.x, 0, -1f), off.add(0, 0, -1f) }, //Side
                    { off, off.add(0, sz.y, 0), off.add(0, sz.y, -1f), off.add(0, 0, -1f) }, //Side
                    { off.add(sz), off.add(0, sz.y, 0), off.add(0, sz.y, -1f), off.add(sz.x, sz.y, -1f) }, //Side
                    { off.add(sz), off.add(sz.x, 0, 0), off.add(sz.x, 0, -1f), off.add(sz.x, sz.y, -1f) }, //Side
            };
            return Obst.makeMesh(verts, Color.BLACK);
        }
    }

    public static class Polygon extends Hitbox {
        private final Obst obst;
        public Polygon(final Obst obst) {
            super(true);
            this.obst = obst;
            for(var poly = 0; poly < obst.polygons(); ++poly) {
                final List<Coord2d> verts = obst.verts(poly);
                final int[] xp = new int[verts.size()];
                final int[] yp = new int[verts.size()];
                for(var i = 0; i < verts.size(); ++i) {
                    xp[i] = (int)Math.ceil(verts.get(i).x);
                    yp[i] = (int)Math.ceil(verts.get(i).y);
                }

                hitbox.add(new Area(new java.awt.Polygon(xp, yp, verts.size())));
            }
        }

        @Override
        public FastMesh mesh() {
            return obst.makeMesh(Color.BLACK, 1f);
        }
    }

    private final boolean hitable;
    public final Area hitbox;

    public Hitbox(boolean hitable) {
        this.hitable = hitable;
        hitbox = new Area();
    }

    boolean canHit() {
        return hitable;
    }

    public boolean checkHit(final Coord2d c, final double a, final Rectangle2D obj) {
        final var area = hitbox.createTransformedArea(AffineTransform.getRotateInstance(a));
        area.transform(AffineTransform.getTranslateInstance(c.x, c.y));
        return area.intersects(obj);
    }

    public FastMesh mesh() {
        return null;
    }

    /**
     * We should favor Obst over Neg
     * Obst > Mesh.link().obst > Neg > Mesh.link().Neg
     */
    private static Hitbox loadHitboxFromRes(final Resource res) {
        if(hitboxes.get(res.name).isEmpty()) {
            final Obst obst = res.layer(Obst.class);
            if(obst != null) {
                return new Polygon(obst);
            } else {
                Resource.Neg neg = res.layer(Resource.negc);

                for(RenderLink.Res link : res.layers(RenderLink.Res.class)) {
                    final Optional<Resource> meshres = link.mesh();
                    if (meshres.isPresent()) {
                        final Obst mobst = meshres.get().layer(Obst.class);
                        if(mobst != null) {
                            return new Polygon(mobst);
                        }
                        final Resource.Neg mneg = meshres.get().layer(Resource.negc);
                        if(mneg != null && neg == null) {
                            neg = mneg;
                        }
                    }
                }

                if(neg != null) {
                    Coord2d hsz = new Coord2d(Math.abs(neg.bc.x) + Math.abs(neg.bs.x),
                            Math.abs(neg.bc.y) + Math.abs(neg.bs.y));
                    Coord2d hoff = new Coord2d(neg.bc);
                    final Hitbox hb = new Rectangular(hoff, hsz, true);
                    hitboxes.put(res.name, hb);
                    return hb;
                } else {
                    return NOHIT;
                }
            }
        } else {
            return NOHIT;
        }
    }

    public static Hitbox hbfor(final String res) {
        return hitboxes.get(res).orElse(NOHIT);
    }

    public static Hitbox hbfor(final Gob g) {
        return hbfor(g, false);
    }

    public static Hitbox hbfor(final Gob g, final boolean force) {
        final Optional<Resource> res = g.res();
        if (res.isPresent()) {
            if (!force) {
                if (!res.get().name.endsWith("gate") && !res.get().name.endsWith("/pow") && !res.get().name.endsWith("consobj")) {
                    return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                } else if (res.get().name.endsWith("gate") && res.get().name.startsWith("gfx/terobjs/arch")) {
                    ResDrawable rd = g.getattr(ResDrawable.class);
                    if (rd != null && (rd.sdtnum() == 1)) {
                        return NOHIT;
                    } else {
                        return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                    }
                } else if (res.get().name.endsWith("/pow")) {
                    ResDrawable rd = g.getattr(ResDrawable.class);
                    if (rd != null && (rd.sdtnum() == 17 || rd.sdtnum() == 33)) {
                        return NOHIT;
                    } else {
                        return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                    }
                } else if (res.get().name.endsWith("consobj")) {
                    for(final var ol : g.overlays()) {
                        if(ol.spr instanceof Consobj) {
                            final var cons = (Consobj)ol.spr;
                            final var sz = new Coord2d(cons.br.sub(cons.ul));
                            return new Rectangular(new Coord2d(cons.ul), sz, true);
                        }
                    }
                    //Couldn't find Consobj sprite
                    return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                } else {
                    return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
                }
            } else {
                return hitboxes.get(res.get().name).orElse(loadHitboxFromRes(res.get()));
            }
        } else {
            return NOHIT;
        }
    }
}
