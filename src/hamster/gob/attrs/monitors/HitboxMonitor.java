package hamster.gob.attrs.monitors;

import hamster.GlobalSettings;
import hamster.gob.sprites.HitboxSprite;
import haven.GAttrib;
import haven.Gob;
import haven.UI;

//TODO: When Hitbox color is changed it should refresh hitbox meshes in here as well.
public class HitboxMonitor extends GAttrib {
    private boolean show;

    public HitboxMonitor(final Gob g) {
        super(g);
        show = false;
    }

    @Override
    public void ctick(double dt) {
        super.ctick(dt);
        final UI ui = gob.glob.ui.get();
        if(ui != null && ui.gui != null) {
            if(show != GlobalSettings.SHOWHITBOX.get()) {
                show = GlobalSettings.SHOWHITBOX.get();
                if(show) {
                    if(gob.findol(HitboxSprite.id) == null) {
                        gob.addol(new Gob.Overlay(gob, HitboxSprite.id, new HitboxSprite(gob)));
                    }
                } else {
                    final Gob.Overlay ol = gob.findol(HitboxSprite.id);
                    if(ol != null) {
                        final HitboxSprite hspr = (HitboxSprite)ol.spr;
                        hspr.rem();
                    }
                }
            }
        }
    }
}
