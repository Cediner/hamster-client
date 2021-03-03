/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package hamster.ui.opt;

import hamster.IndirSetting;
import hamster.ui.core.indir.IndirBaseColorPreview;
import hamster.ui.core.indir.IndirColorPreview;
import haven.UI;
import haven.Coord;
import haven.Widget;
import haven.GameUI;
import haven.HSlider;
import haven.Audio;
import haven.Button;
import haven.Label;
import haven.Window;
import haven.render.BaseColor;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.function.Consumer;


public class OptionsWnd extends Window {
    public final Panel main, audio;
    public Panel current;

    public void chpanel(Panel p) {
        Coord cc = this.c.add(this.sz.div(2));
        if (current != null)
            current.hide();
        (current = p).show();
        pack();
        move(cc.sub(this.sz.div(2)));
    }

    static class Panel extends Widget {
        public Panel() {
            visible = false;
            c = Coord.z;
        }
    }

    public class PButton extends Button {
        public final Panel tgt;
        public final int key;

        public PButton(int w, String title, int key, Panel tgt) {
            super(w, title);
            this.tgt = tgt;
            this.key = key;
        }

        public void click() {
            chpanel(tgt);
        }

        public boolean keydown(KeyEvent ev) {
            if ((this.key != -1) && (ev.getKeyChar() == this.key)) {
                click();
                return (true);
            }
            return (false);
        }
    }

    public OptionsWnd(boolean gopts, UI ui) {
        super(Coord.z, "Options", "options", true);
        main = add(new Panel());
        audio = add(new Panel());
        final Panel uip = add(new Panel());
        final Panel gp = add(new Panel());
        final Panel video = add(new Panel());
        final Panel theme = add(new Panel());
        final Panel mbinds = add(new Panel());
        final Panel kbinds = add(new Panel());
        final int spacer = 5;

        { //Main Menu
            int y = 0;
            y += main.add(new PButton(200, "Video settings", 'v', video), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(200, "Audio settings", 'a', audio), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(200, "Theme settings", 't', theme), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(200, "Mousebind settings", 'm', mbinds), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(200, "Keybind settings", 'b', kbinds), new Coord(0, y)).sz.y + spacer;
            if (gopts) {
                y += main.add(new PButton(200, "UI settings", 'u', uip), new Coord(0, y)).sz.y + spacer;
                y += main.add(new PButton(200, "Gameplay settings", 'g', gp), new Coord(0, y)).sz.y + spacer;
                y += main.add(new Button(200, "Switch character") {
                    public void click() {
                        getparent(GameUI.class).act("lo", "cs");
                    }
                }, new Coord(0, y)).sz.y + spacer;
                y += main.add(new Button(200, "Log out") {
                    public void click() {
                        getparent(GameUI.class).act("lo");
                    }
                }, new Coord(0, y)).sz.y + spacer;
            }
            y += main.add(new Button(200, "Close") {
                public void click() {
                    OptionsWnd.this.hide();
                }
            }, new Coord(0, y)).sz.y + spacer;
            main.pack();
        }

        { //Audio
            int y = 0;
            //TODO redo with an IndirHSlider
            audio.add(new Label("Master audio volume"), new Coord(0, y));
            y += 15;
            audio.add(new HSlider(200, 0, 1000, (int) (Audio.volume * 1000)) {
                public void changed() {
                    Audio.setvolume(val / 1000.0);
                }
            }, new Coord(0, y));
            y += 30;
            //TODO redo with an IndirHSlider
            audio.add(new Label("In-game event volume"), new Coord(0, y));
            y += 15;
            audio.add(new HSlider(200, 0, 1000, 0) {
                protected void attach(UI ui) {
                    super.attach(ui);
                    val = (int) (ui.audio.pos.volume * 1000);
                }

                public void changed() {
                    ui.audio.pos.setvolume(val / 1000.0);
                }
            }, new Coord(0, y));
            y += 20;
            //TODO redo with an IndirHSlider
            audio.add(new Label("Ambient volume"), new Coord(0, y));
            y += 15;
            audio.add(new HSlider(200, 0, 1000, 0) {
                protected void attach(UI ui) {
                    super.attach(ui);
                    val = (int) (ui.audio.amb.volume * 1000);
                }

                public void changed() {
                    ui.audio.amb.setvolume(val / 1000.0);
                }
            }, new Coord(0, y));
            y += 35;
            audio.add(new PButton(200, "Back", 27, main), new Coord(0, 180));
            audio.pack();
        }


        { //Theme Panel
            int y = 0;
            y += theme.add(new ThemePanel(ui)).sz.y + 5;
            theme.pack();
            theme.adda(new PButton(200, "Back", 27, main), new Coord(200 / 2, y), -0.5, 0);
            theme.pack();
        }
        { //Video Panel
            int y = 0;
            y += video.add(new VideoPanel(ui, ui.gprefs)).sz.y + 5;
            video.pack();
            video.adda(new PButton(200, "Back", 27, main), new Coord(200 / 2, y), -0.5, 0);
            video.pack();
        }
        { //Mousebind panel
            int y = 0;
            y += mbinds.add(new MouseBindsPanel(ui)).sz.y + 5;
            mbinds.pack();
            mbinds.adda(new PButton(200, "Back", 27, main), new Coord(200 / 2, y), -0.5, 0);
            mbinds.pack();
        }
        { //Keybind panel
            int y = 0;
            y += kbinds.add(new KeyBindPanel(ui)).sz.y + 5;
            kbinds.pack();
            kbinds.adda(new PButton(200, "Back", 27, main), new Coord(200, y), -0.5, 0);
            kbinds.pack();
        }
        if (gopts) {
            { //UI Panel
                int y = 0;
                y += uip.add(new UIPanel(ui)).sz.y + 5;
                uip.pack();
                uip.adda(new PButton(200, "Back", 27, main), new Coord(200 / 2, y), -0.5, 0);
                uip.pack();
            }
            { //Gameplay Panel
                int y = 0;
                y += gp.add(new GameplayPanel(ui)).sz.y + 5;
                gp.pack();
                gp.adda(new PButton(200, "Back", 27, main), new Coord(200 / 2, y), -0.5, 0);
                gp.pack();
            }
        }


        chpanel(main);
    }

    public OptionsWnd(UI ui) {
        this(ui.gui != null, ui);
    }

    public static Widget BaseColorPreWithLabel(final String text, final IndirSetting<BaseColor> cl) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final IndirBaseColorPreview pre = new IndirBaseColorPreview(new Coord(16, 16), cl);
        final int height = Math.max(lbl.sz.y, pre.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y / 2));
        container.pack();
        return container;
    }

    public static Widget ColorPreWithLabel(final String text, final IndirSetting<Color> cl) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final IndirColorPreview pre = new IndirColorPreview(new Coord(16, 16), cl);
        final int height = Math.max(lbl.sz.y, pre.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y / 2));
        container.pack();
        return container;
    }

    public static Widget ColorPreWithLabel(final String text, final IndirSetting<Color> cl, final Consumer<Color> cb) {
        final Widget container = new Widget();
        final Label lbl = new Label(text);
        final IndirColorPreview pre = new IndirColorPreview(new Coord(16, 16), cl, cb);
        final int height = Math.max(lbl.sz.y, pre.sz.y) / 2;
        container.add(lbl, new Coord(0, height - lbl.sz.y / 2));
        container.add(pre, new Coord(lbl.sz.x, height - pre.sz.y / 2));
        container.pack();
        return container;
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
        if ((sender == this) && (msg.equals("close"))) {
            hide();
        } else {
            super.wdgmsg(sender, msg, args);
        }
    }

    public void show() {
        chpanel(main);
        super.show();
    }
}
