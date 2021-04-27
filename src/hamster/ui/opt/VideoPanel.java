package hamster.ui.opt;

import hamster.data.TranslationLookup;
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
        super(OptionsWnd.PANEL_SIZE);
        final Coord spacer = new Coord(UI.scale(20), UI.scale(5));

        final Grouping disp = new LinearGrouping(TranslationLookup.get("opt_video_display"), spacer, false);
        final Grouping shadow = new LinearGrouping(TranslationLookup.get("opt_video_shadow"), spacer, false);
        final Grouping ol = new LinearGrouping(TranslationLookup.get("opt_video_outline"), spacer, false);

        { //Display
            final IndirRadioGroup<String> fsm = disp.add(new IndirRadioGroup<>(TranslationLookup.get("opt_video_frame_mode"), UI.scale(450), FRAMESYNCMODE));
            fsm.add(TranslationLookup.get("opt_video_frame_one"), JOGLPanel.SyncMode.FRAME.name());
            fsm.add(TranslationLookup.get("opt_video_frame_tick"), JOGLPanel.SyncMode.TICK.name());
            fsm.add(TranslationLookup.get("opt_video_frame_cpu"), JOGLPanel.SyncMode.SEQ.name());
            fsm.add(TranslationLookup.get("opt_video_frame_gpu"), JOGLPanel.SyncMode.FINISH.name());

            disp.add(new IndirCheckBox(TranslationLookup.get("opt_video_wireframe"), WIREFRAMEMODE));
            disp.add(new IndirCheckBox(TranslationLookup.get("opt_video_vsync"), VSYNC));
            disp.add(new IndirLabel(() -> String.format("%s%d", TranslationLookup.get("opt_video_fps"), FPS.get())));
            disp.add(new IndirHSlider(UI.scale(210), 5, 240, FPS));
            disp.add(new IndirLabel(() -> String.format("%s%d", TranslationLookup.get("opt_video_bgfps"), BGFPS.get())));
            disp.add(new IndirHSlider(UI.scale(210), 5, 240, BGFPS));

                    //TODO redo with an IndirHSlider
            disp.add(new Label(TranslationLookup.get("opt_video_render")));
            {
                Label dpy = disp.add(new Label(""));
                final int steps = 4;
                disp.add(new HSlider(UI.scale(210), -2 * steps, 2 * steps, (int) Math.round(steps * Math.log(gprefs.rscale.val) / Math.log(2.0f))) {
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
            disp.add(new Label(TranslationLookup.get("opt_video_ui_scale")));
            {
                Label dpy = disp.add(new Label(""));
                final double smin = 1, smax = Math.floor(UI.maxscale() / 0.25) * 0.25;
                final int steps = (int)Math.round((smax - smin) / 0.25);
                disp.add(new HSlider(UI.scale(210), 0, steps, (int)Math.round(steps*(UISCALE.get() - smin) / (smax - smin))) {
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
            final Consumer<Integer> resetshadows = val -> MapView.MessageBus.send(new MapView.ResetShadows());

            shadow.add(new IndirCheckBox(TranslationLookup.get("opt_video_shadow_render"), SHADOWS));
            //shadow quality
            {
                final IndirRadioGroup<Integer> sqrg = shadow.add(new IndirRadioGroup<>(TranslationLookup.get("opt_video_shadow_quality"), UI.scale(450), SHADOWQUALITY, resetshadows));
                for(int i = 0; i < MapView.shadowmap.length; ++i) {
                    sqrg.add(Integer.toString(MapView.shadowmap[i]), i);
                }
            }
            {
                final IndirRadioGroup<Integer> sqrg = shadow.add(new IndirRadioGroup<>(TranslationLookup.get("opt_video_shadow_size"), UI.scale(450), SHADOWSIZE, resetshadows));
                for(int i = 0; i < MapView.shadowsizemap.length;++i) {
                    sqrg.add(Integer.toString(MapView.shadowsizemap[i]), i);
                }
            }
            {
                final IndirRadioGroup<Integer> sqrg = shadow.add(new IndirRadioGroup<>(TranslationLookup.get("opt_video_shadow_depth"), UI.scale(450), SHADOWDEPTH, resetshadows));
                for(int i = 0; i < MapView.shadowdepthmap.length;++i) {
                    sqrg.add(Integer.toString(MapView.shadowdepthmap[i]), i);
                }
            }
            shadow.pack();
        }
        { //Outlines
            ol.add(new IndirCheckBox(TranslationLookup.get("opt_video_outline_sym"), SYMMETRICOUTLINES, (val) -> MapView.MessageBus.send(new MapView.ToggleOutlines(val))));
            ol.pack();
        }

        int y = 0;
        y += add(disp, new Coord(0, y)).sz.y + spacer.y;
        y += add(shadow, new Coord(0, y)).sz.y + spacer.y;
        y += add(ol, new Coord(0, y)).sz.y + spacer.y;
        pack();
    }
}
