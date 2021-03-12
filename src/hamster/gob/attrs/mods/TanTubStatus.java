package hamster.gob.attrs.mods;

import haven.GAttrib;
import haven.Gob;
import haven.UI;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;

/**
 * Color mod for Tanning Tub gobs
 */
public class TanTubStatus extends GAttrib implements Gob.SetupMod {
    enum State {
        EMPTY,
        DONE,
        WATER,
        WATERBARK,
        WORKING
    }

    private static final haven.render.State tubDone = new BaseColor(new Color(87, 204, 73, 255));
    private static final haven.render.State tubEmpty = new BaseColor(new Color(255, 0, 0, 200));
    private static final haven.render.State tubNeedsWater = new BaseColor(new Color(0, 0, 255, 200));
    private static final haven.render.State tubNeedsBark = new BaseColor(new Color(232, 255, 0, 200));

    private State state;

    public TanTubStatus(final Gob g) {
        super(g);
        this.state = State.WORKING;
    }

    @Override
    public Pipe.Op gobstate() {
        final UI ui = gob.glob.ui.get();
        if (ui != null && ui.gui != null && ui.gui.settings.COLORFULTUBS.get()) {
            return switch (state) {
                case EMPTY -> tubEmpty;
                case DONE -> tubDone;
                case WATERBARK -> tubNeedsBark;
                case WATER -> tubNeedsWater;
                default -> null;
            };
        } else {
            return null;
        }
    }

    @Override
    public void ctick(double dt) {
        final int sdt = gob.sdt();
        switch (sdt) {
            //done
            case 10, 9, 8 -> state = State.DONE;
            //empty need water + bark
            case 2, 1, 0 -> state = State.EMPTY;
            //Has items + some water, needs more water or bark
            case 5 -> state = State.WATERBARK;
            //Has items, needs bark + water
            case 4 -> state = State.WATER;
            //6 - working
            default -> state = State.WORKING;
        }
    }
}
