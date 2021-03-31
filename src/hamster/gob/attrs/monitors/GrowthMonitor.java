package hamster.gob.attrs.monitors;

import hamster.GlobalSettings;
import hamster.data.gob.CropData;
import hamster.data.gob.ObjData;
import hamster.gob.sprites.GrowthSprite;
import haven.GAttrib;
import haven.Gob;
import haven.UI;

public class GrowthMonitor extends GAttrib {
    private boolean show;
    private int stage = -1;
    private final CropData cropdata;

    public GrowthMonitor(final Gob g, final String name) {
        super(g);
        show = false;
        this.cropdata = ObjData.getCropData(name);
    }

    private void remol() {
        final Gob.Overlay ol = gob.findol(GrowthSprite.id);
        if(ol != null) {
            final GrowthSprite hspr = (GrowthSprite)ol.spr;
            hspr.rem();
        }
    }

    @Override
    public void ctick(double dt) {
        super.ctick(dt);
        final UI ui = gob.glob.ui.get();
        if(ui != null && ui.gui != null) {
            final int nstg = !gob.fallowplant() ? gob.sdt() : -1;
            if(show != GlobalSettings.SHOWCROPSTAGE.get() || stage != nstg) {
                if(stage != nstg) {
                    remol();
                }
                show = GlobalSettings.SHOWCROPSTAGE.get();
                stage = nstg;

                if(show) {
                    if(gob.findol(GrowthSprite.id) == null) {
                        gob.addol(new Gob.Overlay(gob, GrowthSprite.id, new GrowthSprite(gob, stage,
                                cropdata.min_stage(), cropdata.final_stage())));
                    }
                } else {
                    remol();
                }
            }
        }
    }
}
