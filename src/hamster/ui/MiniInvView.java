package hamster.ui;

import hamster.GlobalSettings;
import hamster.ui.core.MovableWidget;
import hamster.ui.core.Theme;
import haven.*;

public class MiniInvView extends MovableWidget {
    private static final Coord spacer = UI.scale(new Coord(1,1));
    private enum State {
        USED(Theme.tex("minv", 2)),
        NOTUSED(Theme.tex("minv", 0)),
        BAD(Theme.tex("minv", 5)),
        ALMOSTBAD(Theme.tex("minv", 4)),
        MID(Theme.tex("minv", 3)),
        GOOD(Theme.tex("minv", 6));

        public final Tex icon;

        State(final Tex icon) {
            this.icon = icon;
        }
    }

    private final State[] wearstate = new State[]{State.BAD, State.ALMOSTBAD, State.MID, State.GOOD};
    private final Inventory inv;
    private Coord isz;
    private boolean hover;

    public MiniInvView(final Inventory monitor) {
        super(monitor.isz.mul(State.USED.icon.sz().add(spacer)).add(Window.wbox.bisz()), "mini-player-inv");
        inv = monitor;
        hover = false;
        isz = inv.isz;
    }

    @Override
    protected void added() {
        super.added();
        visible = GlobalSettings.SHOWMINIINV.get();
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
        return true;
    }

    @Override
    public void tick(double dt) {
        super.tick(dt);
        if (!isz.equals(inv.isz)) {
            isz = inv.isz;
            sz = isz.mul(State.USED.icon.sz().add(spacer)).add(Window.wbox.bisz());
        }
    }

    public void draw(GOut g) {
        Coord c = new Coord();
        Coord csz = State.USED.icon.sz().add(spacer);
        Coord ic;
        WItem wi;
        final WItem[][] map = inv.itemmap();
        for (c.y = 0; c.y < inv.isz.y; c.y++) {
            for (c.x = 0; c.x < inv.isz.x; c.x++) {
                wi = map[c.x][c.y];
                ic = c.mul(csz).add(Window.wbox.btloff());
                if (wi != null) {
                    int wear = wi.wearlevel();
                    if (wear < 0) {
                        g.image(State.USED.icon, ic);
                    } else {
                        g.image(wearstate[wear].icon, ic);
                    }
                } else {
                    g.image(State.NOTUSED.icon, ic);
                }
            }
        }

        if (hover) {
            Window.wbox.draw(g, Coord.z, sz);
        }
    }


    @Override
    public boolean mousedown(Coord mc, int button) {
        if (button == 1) {
            ui.gui.invwnd.toggleVisibility();
            return true;
        } else {
            return super.mousedown(mc, button);
        }
    }

    @Override
    public void mousemove(Coord mc) {
        super.mousemove(mc);
        hover = mc.isect(Coord.z, sz);
    }
}
