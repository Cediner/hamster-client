package hamster.gob.sprites;

import haven.*;
import haven.render.Homo3D;
import haven.render.Pipe;

import java.awt.*;

public class GobCombatSprite extends Sprite implements PView.Render2D {
    public static final int id = -244942;
    private Fightview.Relation rel;
    private Tex myip, relip;
    private int myipi = -1, relipi = -1;

    public GobCombatSprite(final Gob g, final Fightview.Relation relation) {
        super(g, null);
        this.rel = relation;
    }

    public void draw(GOut g, Pipe state) {
        if (rel != null) {
            Coord sc = Homo3D.obj2view(new Coord3f(0, 0, 15), state).round2();
            final Coord c = sc.sub(0, 90);
            final Coord bc = c.copy();

            //Draw Buffs
            for (Widget wdg = rel.buffs.child; wdg != null; wdg = wdg.next) {
                if (!(wdg instanceof Buff))
                    continue;
                final Buff buf = (Buff) wdg;
                if (buf.ameter() >= 0 && buf.isOpening()) {
                    buf.fightdraw(g.reclip(bc.copy(), Buff.scframe.tex().sz()));
                    bc.x += Buff.scframe.tex().sz().x + 2;
                }
            }

            //Draw IP
            g.chcolor(new Color(60, 60, 60, 168));
            g.frect(c.sub(40, 0), myip.sz());
            g.frect(c.sub(40, -15), relip.sz());
            g.chcolor(Color.GREEN);
            g.image(myip, c.sub(40, 0));
            g.chcolor(Color.RED);
            g.image(relip, c.sub(40, -15));
            g.chcolor();
        }
    }

    private void updateIP() {
        if(myipi != rel.ip) {
            myipi = rel.ip;
            if(myip != null)
                myip.dispose();
            myip = Text.renderstroked("IP " + myipi).tex();
        }
        if(relipi != rel.oip) {
            relipi = rel.oip;
            if(relip != null)
                relip.dispose();
            relip = Text.renderstroked("IP " + relipi).tex();
        }
    }

    @Override
    public boolean tick(double dt) {
        if(rel != null) {
            updateIP();
        }
        return super.tick(dt);
    }

    public void update(final Fightview.Relation rel) {
        this.rel = rel;
        if(this.rel != null) {
            updateIP();
        }
    }
}
