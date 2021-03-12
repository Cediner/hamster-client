package hamster.gob.attrs.mods;

import haven.GAttrib;
import haven.Gob;
import haven.UI;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;

/**
 * Color mod for Cupboard gobs
 */
public class CupboardStatus extends GAttrib implements Gob.SetupMod {
    enum State {
        EMPTY,
        FULL,
        INBETWEEN
    }

    private static final haven.render.State cupboardfull = new BaseColor(new Color(255, 0, 0, 255));
    private static final haven.render.State cupboardempty = new BaseColor(new Color(0, 255, 0, 255));

    private State state;

    public CupboardStatus(final Gob g) {
        super(g);
        this.state = State.INBETWEEN;
    }

    @Override
    public Pipe.Op gobstate() {
        final UI ui = gob.glob.ui.get();
        if (ui != null && ui.gui != null &&  ui.gui.settings.COLORFULCUPBOARDS.get()) {
            return switch (state) {
                case EMPTY -> cupboardempty;
                case FULL -> cupboardfull;
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
            case 30, 29 -> state = State.FULL;
            case 1, 2 -> state = State.EMPTY;
            default -> state = State.INBETWEEN;
        }
    }
}
