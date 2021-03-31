package hamster.gob.attrs.mods;

import hamster.GlobalSettings;
import haven.*;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;

/**
 * Color mod for Drying Frame gobs
 */
public class DryingFrameStatus extends GAttrib implements Gob.SetupMod {
    enum State {
        EMPTY,
        DONE,
        DRYING
    }

    private static final haven.render.State dframeDone = new BaseColor(new Color(87, 204, 73, 255));
    private static final haven.render.State dframeEmpty = new BaseColor(new Color(209, 42, 42, 255));

    private State state;

    public DryingFrameStatus(final Gob g) {
        super(g);
        state = State.DRYING;
    }


    public Pipe.Op gobstate() {
        return switch (state) {
            case DONE -> dframeDone;
            case EMPTY -> dframeEmpty;
            default -> null;
        };
    }

    @Override
    public void ctick(double dt) {
        super.ctick(dt);
        final UI ui = gob.glob.ui.get();
        if(ui != null && ui.gui != null && GlobalSettings.COLORFULFARMES.get()) {
            boolean done = true;
            boolean empty = true;
            for (final Gob.Overlay ol : gob.ols) {
                try {
                    Indir<Resource> olires = ol.res;
                    if (olires != null) {
                        empty = false;
                        Resource olres = olires.get();
                        if (olres != null) {
                            if (olres.name.endsWith("-blood") || olres.name.endsWith("-windweed") || olres.name.endsWith("-fishraw")) {
                                done = false;
                                break;
                            }
                        }
                    }
                } catch (Loading ignored) {
                    //Skip frame
                }
            }
            state = empty ? State.EMPTY : done ? State.DONE : State.DRYING;
        } else {
            state = State.DRYING;
        }
    }
}
