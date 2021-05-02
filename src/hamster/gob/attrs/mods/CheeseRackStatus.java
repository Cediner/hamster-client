package hamster.gob.attrs.mods;

import hamster.GlobalSettings;
import haven.GAttrib;
import haven.Gob;
import haven.UI;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;

/**
 * Color mod for CheeseRacks gobs
 */
public class CheeseRackStatus extends GAttrib implements Gob.SetupMod {
    enum State {
        EMPTY,
        FULL,
        INBETWEEN
    }

    private static final haven.render.State cheeserackfull = new BaseColor(new Color(255, 0, 0, 255));
    private static final haven.render.State cheeserackempty = new BaseColor(new Color(0, 255, 0, 255));

    private State state;

    public CheeseRackStatus(final Gob g) {
        super(g);
        this.state = State.INBETWEEN;
    }

    @Override
    public Pipe.Op gobstate() {
        final UI ui = gob.glob.ui.get();
        if (ui != null && ui.gui != null &&  GlobalSettings.COLORFULCHEESERACKS.get()) {
            return switch (state) {
                case EMPTY -> cheeserackempty;
                case FULL -> cheeserackfull;
                default -> null;
            };
        } else {
            return null;
        }
    }

    @Override
    public void ctick(double dt) {
        final int count = gob.ols.size();
        switch (count) {
            case 0 -> state = State.EMPTY;
            case 3 -> state = State.FULL;
            default -> state = State.INBETWEEN;
        }
    }

    @Override
    public String toString() {
        return "CheeseRackStatus(" +
                "state=" + state +
                ')';
    }
}
