package hamster.gob.attrs.monitors;

import hamster.GlobalSettings;
import hamster.gob.sprites.HaloSprite;
import haven.*;

/**
 * This just monitors and watches for when a Gob should have a Halo displayed
 * The Halo itself is an Overlay
 */
public class Halo extends GAttrib {
    private boolean show;

    public Halo(final Gob g) {
        super(g);
        show = false;
    }

    private boolean isHearthingOrKnocked() {
        Drawable d = gob.getattr(Drawable.class);
        if (d instanceof Composite) {
            Composite comp = (Composite) d;

            if (comp.oldposes != null) {
                for (ResData res : comp.oldposes) {
                    final String nm = gob.rnm(res.res);
                    if(nm != null) {
                        if (nm.equals("gfx/borka/knock") || nm.equals("gfx/borka/pointhome")) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    @Override
    public void ctick(double dt) {
        super.ctick(dt);
        final UI ui = gob.glob.ui.get();
        final boolean nshow;
        if (ui != null && ui.gui != null) {
            if (GlobalSettings.SHOWGOBHALO.get()){
                nshow = true;
            } else if (GlobalSettings.SHOWGOBHALOONHEARTH.get()) {
                nshow = isHearthingOrKnocked();
            } else {
                nshow = false;
            }
        } else {
            nshow = false;
        }

        if(show != nshow) {
            show = nshow;
            if(show) {
                if(gob.findol(HaloSprite.id) == null) {
                    gob.addol(new Gob.Overlay(gob, HaloSprite.id, new HaloSprite(gob)));
                }
            } else  {
                final Gob.Overlay ol = gob.findol(HaloSprite.id);
                if(ol != null) {
                    final HaloSprite hspr = (HaloSprite)ol.spr;
                    hspr.rem();
                }
            }
        }
    }
}
