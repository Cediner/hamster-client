package hamster.gob.attrs.monitors;

import hamster.gob.sprites.MyGobIndicatorSprite;
import haven.GAttrib;
import haven.Gob;
import haven.MapView;
import haven.UI;
import haven.render.RenderTree;

/**
 * This just monitors and watches for when a Gob should have an indicator displayed
 * The indicator itself is an Overlay
 */
public class MyGobIndicator extends GAttrib implements RenderTree.Node {
    private boolean show;

    public MyGobIndicator(final Gob g) {
        super(g);
        show = false;
    }

    @Override
    public void ctick(double dt) {
        super.ctick(dt);
        final UI ui = gob.glob.ui.get();
        final boolean nshow = ui != null && ui.gui != null && ui.gui.map != null &&
                (ui.gui.map.camera instanceof MapView.Fixator || ui.gui.map.camera instanceof MapView.FreeStyle);

        if(nshow != show) {
            show = nshow;
            if(show) {
                if(gob.findol(MyGobIndicatorSprite.id) == null) {
                    gob.addol(new Gob.Overlay(gob, MyGobIndicatorSprite.id, new MyGobIndicatorSprite(gob)));
                }
            } else  {
                final Gob.Overlay ol = gob.findol(MyGobIndicatorSprite.id);
                if(ol != null) {
                    final MyGobIndicatorSprite hspr = (MyGobIndicatorSprite)ol.spr;
                    hspr.rem();
                }
            }
        }
    }
}
