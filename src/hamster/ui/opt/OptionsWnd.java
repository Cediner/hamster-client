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
import hamster.data.TranslationLookup;
import hamster.ui.core.indir.IndirBaseColorPreview;
import hamster.ui.core.indir.IndirColorPreview;
import haven.UI;
import haven.Coord;
import haven.Widget;
import haven.GameUI;
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
        super(Coord.z, TranslationLookup.get("opt_options"), "options", true);
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
            y += main.add(new PButton(UI.scale(200), TranslationLookup.get("opt_video_settings"), 'v', video), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(UI.scale(200), TranslationLookup.get("opt_audio_settings"), 'a', audio), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(UI.scale(200), TranslationLookup.get("opt_theme_settings"), 't', theme), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(UI.scale(200), TranslationLookup.get("opt_mb_settings"), 'm', mbinds), new Coord(0, y)).sz.y + spacer;
            y += main.add(new PButton(UI.scale(200), TranslationLookup.get("opt_kb_settings"), 'b', kbinds), new Coord(0, y)).sz.y + spacer;
            if (gopts) {
                y += main.add(new PButton(UI.scale(200), TranslationLookup.get("opt_ui_settings"), 'u', uip), new Coord(0, y)).sz.y + spacer;
                y += main.add(new PButton(UI.scale(200), TranslationLookup.get("opt_gameplay_settings"), 'g', gp), new Coord(0, y)).sz.y + spacer;
                y += main.add(new Button(UI.scale(200), TranslationLookup.get("opt_switch_char")) {
                    public void click() {
                        getparent(GameUI.class).act("lo", "cs");
                    }
                }, new Coord(0, y)).sz.y + spacer;
                y += main.add(new Button(UI.scale(200), TranslationLookup.get("opt_logout")) {
                    public void click() {
                        getparent(GameUI.class).act("lo");
                    }
                }, new Coord(0, y)).sz.y + spacer;
            }
            main.add(new Button(UI.scale(200), TranslationLookup.get("close")) {
                public void click() {
                    OptionsWnd.this.hide();
                }
            }, new Coord(0, y));
            main.pack();
        }

        { //Audio
            int y = 0;
            y += audio.add(new AudioPanel(ui)).sz.y + 5;
            audio.pack();
            final var t = audio.adda(new PButton(UI.scale(200), TranslationLookup.get("back"), 27, main), new Coord(audio.sz.x / 2, y), 0.5, 0);
            audio.pack();
        }


        { //Theme Panel
            int y = 0;
            y += theme.add(new ThemePanel(ui)).sz.y + 5;
            theme.pack();
            theme.adda(new PButton(UI.scale(200), TranslationLookup.get("back"), 27, main), new Coord(theme.sz.x / 2, y), 0.5, 0);
            theme.pack();
        }
        { //Video Panel
            int y = 0;
            y += video.add(new VideoPanel(ui, ui.gprefs)).sz.y + 5;
            video.pack();
            video.adda(new PButton(UI.scale(200), TranslationLookup.get("back"), 27, main), new Coord(video.sz.x / 2, y), 0.5, 0);
            video.pack();
        }
        { //Mousebind panel
            int y = 0;
            y += mbinds.add(new MouseBindsPanel(ui)).sz.y + 5;
            mbinds.pack();
            mbinds.adda(new PButton(UI.scale(200), TranslationLookup.get("back"), 27, main), new Coord(mbinds.sz.x / 2, y), 0.5, 0);
            mbinds.pack();
        }
        { //Keybind panel
            int y = 0;
            y += kbinds.add(new KeyBindPanel(ui)).sz.y + 5;
            kbinds.pack();
            kbinds.adda(new PButton(UI.scale(200), TranslationLookup.get("back"), 27, main), new Coord(kbinds.sz.x / 2, y), 0.5, 0);
            kbinds.pack();
        }
        if (gopts) {
            { //UI Panel
                int y = 0;
                y += uip.add(new UIPanel(ui)).sz.y + 5;
                uip.pack();
                uip.adda(new PButton(UI.scale(200), TranslationLookup.get("back"), 27, main), new Coord(uip.sz.x / 2, y), 0.5, 0);
                uip.pack();
            }
            { //Gameplay Panel
                int y = 0;
                y += gp.add(new GameplayPanel(ui)).sz.y + 5;
                gp.pack();
                gp.adda(new PButton(UI.scale(200), TranslationLookup.get("back"), 27, main), new Coord(gp.sz.x / 2, y), 0.5, 0);
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
