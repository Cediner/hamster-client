package hamster.ui.core;

import haven.*;

public class Scrollport extends Widget {
    private static final Tex vchainb = Theme.tex("scroll/vertical", 2);
    private static final Tex vchainm = Theme.tex("scroll/vertical", 1);
    private static final Tex vchaint = Theme.tex("scroll/vertical", 0);
    private static final Tex vflarp = Theme.tex("scroll/vertical", 3);
    private static final Tex hchainl = Theme.tex("scroll/horizontal", 0);
    private static final Tex hchainm = Theme.tex("scroll/horizontal", 1);
    private static final Tex hchainr = Theme.tex("scroll/horizontal", 2);
    private static final Tex hflarp = Theme.tex("scroll/horizontal", 3);
    private static final int sensitivity = 10;

    private final Coord ul;
    private Coord vsz;
    private Coord rsz;
    private Coord msz;
    private Coord2d step;

    private UI.Grab grabv, grabh;

    public Scrollport(Coord vsz) {
        super(vsz);
        this.vsz = vsz;
        this.msz = vsz.sub(vflarp.sz().x, hflarp.sz().y);
        ul = new Coord(0, 0);
        resize(vsz);
    }

    public void resizeView(final Coord sz) {
        this.sz = sz;
        pack();
    }

    public Coord msz() { return msz; }

    @Override
    public void resize(Coord sz) {
        rsz = sz;
        vsz = new Coord(this.sz);
        msz = this.sz.sub(vflarp.sz().x, hflarp.sz().y);
        if (vsz.x < rsz.x)
            vsz.y -= hflarp.sz().y;
        if (vsz.y < rsz.y)
            vsz.x -= vflarp.sz().x;
        step = new Coord2d((double) (rsz.x - vsz.x) / (vsz.x - hflarp.sz().x), (double) (rsz.y - vsz.y) / (vsz.y - vflarp.sz().y));
    }

    @Override
    public void draw(GOut g) {
        if (vsz.x < rsz.x) {
            g.image(hchainl, new Coord(0, vsz.y));
            g.rimageh(hchainm, new Coord(hchainl.sz().x, vsz.y), vsz.x - (hchainl.sz().x + hchainr.sz().x));
            g.image(hchainr, new Coord(vsz.x - hchainr.sz().x, vsz.y));
            g.image(hflarp, new Coord((int) ((vsz.x - hflarp.sz().x) * ((double) ul.x / (rsz.x - vsz.x))), vsz.y));
        }
        if (vsz.y < rsz.y) {
            g.image(vchaint, new Coord(vsz.x, 0));
            g.rimagev(vchainm, new Coord(vsz.x, vchaint.sz().y), vsz.y - (vchainb.sz().y + vchaint.sz().y));
            g.image(vchainb, new Coord(vsz.x, vsz.y - vchainb.sz().y));
            g.image(vflarp, new Coord(vsz.x, (int) ((vsz.y - vflarp.sz().y) * ((double) ul.y / (rsz.y - vsz.y)))));
        }
        super.draw(g);
    }

    @Override
    public void draw(GOut g, boolean strict) {
        Widget next;
        final GOut viewport = g.reclip(Coord.z, vsz);

        for (Widget wdg = child; wdg != null; wdg = next) {
            next = wdg.next;
            if (!wdg.visible)
                continue;
            Coord cc = xlate(wdg.c, true);
            GOut g2;
            if (strict)
                g2 = viewport.reclip(cc, wdg.sz);
            else
                g2 = viewport.reclipl(cc, wdg.sz);
            wdg.draw(g2);
        }
    }

    @Override
    public Coord xlate(Coord c, boolean in) {
        return c.sub(ul);
    }

    @Override
    public boolean mousewheel(Coord c, int amount) {
        if (!super.mousewheel(c, amount)) {
            if(amount > 0)
                ul.y += Math.ceil(step.y * amount * sensitivity);
            else
                ul.y += Math.floor(step.y * amount * sensitivity);
            if (ul.y > (rsz.y - vsz.y))
                ul.y = (rsz.y - vsz.y);
            if (ul.y < 0)
                ul.y = 0;
        }
        return true;
    }

    @Override
    public boolean mousedown(Coord c, int button) {
        if (vsz.x < rsz.x && c.isect(new Coord((int) (sz.x * ((double) ul.x / (rsz.x - vsz.x))), vsz.y), vflarp.sz())) {
            grabh = ui.grabmouse(this);
            return true;
        } else if (vsz.y < rsz.y && c.isect(new Coord(vsz.x, (int) ((vsz.y - vflarp.sz().y) * ((double) ul.y / (rsz.y - vsz.y)))),
                vflarp.sz())) {
            grabv = ui.grabmouse(this);
            return true;
        } else {
            return super.mousedown(c, button);
        }
    }

    @Override
    public boolean mouseup(Coord c, int button) {
        if (grabv != null) {
            grabv.remove();
            grabv = null;
            return true;
        } else if (grabh != null) {
            grabh.remove();
            grabh = null;
            return true;
        }
        {
            return super.mouseup(c, button);
        }
    }

    @Override
    public void mousemove(Coord c) {
        if (grabv != null) {
            final Coord cc = new Coord(c);
            cc.y -= vflarp.sz().y / 2;
            if (cc.y < this.c.y)
                cc.y = 0;
            else if (cc.y > this.c.y + this.vsz.y - vflarp.sz().y)
                cc.y = this.vsz.y - vflarp.sz().y;
            ul.y = (int) ((double) cc.y / (vsz.y - vflarp.sz().y) * ((double) rsz.y - vsz.y));
        } else if (grabh != null) {
            final Coord cc = new Coord(c);
            cc.x -= hflarp.sz().x / 2;
            if (cc.x < this.c.x)
                cc.x = 0;
            else if (cc.x > this.c.x + this.vsz.x - hflarp.sz().x)
                cc.x = this.vsz.x - hflarp.sz().x;

            ul.x = (int) ((double) cc.x / (vsz.x - hflarp.sz().x) * ((double) rsz.x - vsz.x));
        } else {
            super.mousemove(c);
        }
    }
}
