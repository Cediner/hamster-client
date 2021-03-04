package hamster.ui.opt;

import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirCheckBox;
import hamster.ui.core.indir.IndirHSlider;
import hamster.ui.core.indir.IndirLabel;
import hamster.ui.core.indir.IndirRadioGroup;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.util.function.Consumer;

import static hamster.GlobalSettings.*;


public class VideoPanel extends Scrollport {
    public VideoPanel(final UI ui, final GSettings gprefs) {
        super(new Coord(UI.scale(500), UI.scale(395)));
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping disp = new LinearGrouping("Display Settings", spacer, false);
        final Grouping shadow = new LinearGrouping("Shadow Settings", spacer, false);
        final Grouping ol = new LinearGrouping("Outline Settings", spacer, false);

        { //Display
            final IndirRadioGroup<String> fsm = disp.add(new IndirRadioGroup<>("Frame Sync Mode", UI.scale(450), FRAMESYNCMODE));
            fsm.add("One-frame overlap", JOGLPanel.SyncMode.FRAME.name());
            fsm.add("Tick overlap", JOGLPanel.SyncMode.TICK.name());
            fsm.add("CPU-sequential", JOGLPanel.SyncMode.SEQ.name());
            fsm.add("GPU-sequential", JOGLPanel.SyncMode.FINISH.name());

            disp.add(new IndirCheckBox("VSync Mode", VSYNC));
            disp.add(new IndirLabel(() -> String.format("FPS: %d", FPS.get())));
            disp.add(new IndirHSlider(UI.scale(210), 5, 240, FPS));
            disp.add(new IndirLabel(() -> String.format("Background FPS: %d", BGFPS.get())));
            disp.add(new IndirHSlider(UI.scale(210), 5, 240, BGFPS));

                    //TODO redo with an IndirHSlider
            disp.add(new Label("Render scale"));
            {
                Label dpy = disp.add(new Label(""));
                final int steps = 4;
                disp.add(new HSlider(UI.scale(160), -2 * steps, 2 * steps, (int) Math.round(steps * Math.log(gprefs.rscale.val) / Math.log(2.0f))) {
                    protected void added() {
                        dpy();
                    }

                    void dpy() {
                        dpy.settext(String.format("%.2f\u00d7", Math.pow(2, this.val / (double) steps)));
                    }

                    public void changed() {
                        try {
                            float val = (float) Math.pow(2, this.val / (double) steps);
                            ui.setgprefs(gprefs.update(null, gprefs.rscale, val));
                        } catch (GSettings.SettingException e) {
                            if(ui.gui != null) {
                                ui.gui.error(e.getMessage());
                            }
                            return;
                        }
                        dpy();
                    }
                });
            }

            //TODO redo with an IndirHSlider
            disp.add(new Label("UI Scale (Requires Restart)"));
            {
                Label dpy = disp.add(new Label(""));
                final double smin = 1, smax = Math.floor(UI.maxscale() / 0.25) * 0.25;
                final int steps = (int)Math.round((smax - smin) / 0.25);
                disp.add(new HSlider(UI.scale(160), 0, steps, (int)Math.round(steps*(UISCALE.get() - smin) / (smax - smin))) {
                    protected void added() {
                        dpy();
                    }
                    void dpy() {
                        dpy.settext(String.format("%.2f\u00d7", smin + (((double)this.val / steps) * (smax - smin))));
                    }
                    public void changed() {
                        double val = smin + (((double)this.val / steps) * (smax - smin));
                        UISCALE.set(val);
                        dpy();
                    }
                });
            }
            disp.pack();
        }
        { //Shadow
            final Consumer<Integer> resetshadows = val -> {
                if (ui.gui != null && ui.gui.map != null) {
                    ui.gui.map.resetshadows();
                }
            };

            shadow.add(new IndirCheckBox("Render shadows", SHADOWS));
            //shadow quality
            {
                final IndirRadioGroup<Integer> sqrg = shadow.add(new IndirRadioGroup<>("Shadow Quality", UI.scale(450), SHADOWQUALITY, resetshadows));
                for(int i = 0; i < MapView.shadowmap.length; ++i) {
                    sqrg.add(Integer.toString(MapView.shadowmap[i]), i);
                }
            }
            {
                final IndirRadioGroup<Integer> sqrg = shadow.add(new IndirRadioGroup<>("Shadow Size", UI.scale(450), SHADOWSIZE, resetshadows));
                for(int i = 0; i < MapView.shadowsizemap.length;++i) {
                    sqrg.add(Integer.toString(MapView.shadowsizemap[i]), i);
                }
            }
            {
                final IndirRadioGroup<Integer> sqrg = shadow.add(new IndirRadioGroup<>("Shadow Depth", UI.scale(450), SHADOWDEPTH, resetshadows));
                for(int i = 0; i < MapView.shadowdepthmap.length;++i) {
                    sqrg.add(Integer.toString(MapView.shadowdepthmap[i]), i);
                }
            }
            shadow.pack();
        }
        { //Outlines
            ol.add(new IndirCheckBox("Symmetric Outlines", SYMMETRICOUTLINES));
            ol.pack();
        }

        int y = 0;
        y += add(disp, new Coord(0, y)).sz.y + spacer.y;
        y += add(shadow, new Coord(0, y)).sz.y + spacer.y;
        y += add(ol, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}
