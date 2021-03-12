package hamster.gob.attrs.monitors;

import hamster.data.HighlightData;
import hamster.gob.sprites.HitboxSprite;
import hamster.gob.sprites.Mark;
import haven.GAttrib;
import haven.Gob;

public class HighlightMonitor extends GAttrib {
    private boolean show;

    public HighlightMonitor(final Gob g) {
        super(g);
        show = false;
    }

    @Override
    public void ctick(double dt) {
        super.ctick(dt);
        if(show != HighlightData.isHighlighted(gob.name())) {
            show = HighlightData.isHighlighted(gob.name());
            if(show) {
                if(gob.findol(HitboxSprite.id) == null) {
                    gob.addol(new Gob.Overlay(gob, Mark.id, new Mark(-1)));
                }
            } else {
                final Gob.Overlay ol = gob.findol(Mark.id);
                if(ol != null) {
                    final Mark hspr = (Mark)ol.spr;
                    hspr.revoke();
                }
            }
        }
    }
}
