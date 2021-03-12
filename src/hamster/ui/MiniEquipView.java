package hamster.ui;

import hamster.ui.core.MovableWidget;
import hamster.ui.core.Theme;
import haven.*;

public class MiniEquipView extends MovableWidget {
    private static final Coord isz = new Coord(3, 9);
    private static final Coord[] ecoords = {
            new Coord( 0, 0), //head
            new Coord( 0, 1), //main accessory
            new Coord( 0, 2), //Shirt
            new Coord(2, 2),  //Torso Armor
            new Coord( 0, 3), //Gloves
            new Coord(2, 3), //Belt
            new Coord( 0, 4), //Left Hand
            new Coord(2, 4), //Right Hand
            new Coord( 0, 5), //Left Hand Ring
            new Coord(2, 5), //Right Hand Ring
            new Coord( 0, 6), //Cloaks & Robes
            new Coord(2, 6), //Back
            new Coord( 0, 7), //Pants
            new Coord(2, 7), //Leg Armor
            new Coord( 0, 8), //Cape
            new Coord(2, 8), //Shoes
            new Coord(1, 0), //Cosmetic Hat
            new Coord(2, 0), //Eyes
            new Coord(2, 1), //Mouth
    };


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

    private final Equipory inv;
    private boolean hover;

    public MiniEquipView(final Equipory eq) {
        super(isz.mul(State.USED.icon.sz().add(1, 1)).add(Window.wbox.bisz()), "mini-equ-view");
        inv = eq;
        hover = false;
    }

    @Override
    protected void added() {
        super.added();
        visible = ui.gui.settings.SHOWMINIEQU.get();
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
        return true;
    }

    public void draw(final GOut g) {
        Coord csz = State.USED.icon.sz().add(1, 1);
        Coord ic;
        WItem wi;
        int i;
        for (i = 0; i < ecoords.length; ++i) {
            wi = inv.slots[i];
            ic = ecoords[i].mul(csz).add(Window.wbox.btloff());
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

        if (hover) {
            Window.wbox.draw(g, Coord.z, sz);
        }
    }


    @Override
    public boolean mousedown(Coord mc, int button) {
        if (button == 1) {
            ui.gui.equwnd.toggleVisiblity();
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
