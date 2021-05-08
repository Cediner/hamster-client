package hamster.ui;

import hamster.GlobalSettings;
import hamster.ui.core.MovableWidget;
import hamster.ui.core.layout.GridGrouping;
import hamster.ui.core.layout.Grouping;
import hamster.util.ObservableListener;
import haven.*;

import java.util.*;

public class SessionDisplay extends MovableWidget implements ObservableListener<UI> {
    private static class UIDisplay extends Widget {
        private final UI ui;
        private final Button btn;
        private String nm;
        private boolean alerted = false;

        public UIDisplay(final UI ui) {
            super(Coord.z);
            this.ui = ui;
            this.nm = "Login";
            add(this.btn = new Button(UI.scale(100), nm, this::click));
            pack();
        }

        @Override
        public void draw(GOut g) {
            if(MainFrame.instance.p.isActiveUI(this.ui))
                btn.highlight();
            else
                btn.unhighlight();
            super.draw(g);
        }

        @Override
        public void tick(double dt) {
            if (nm.equals("Login") && ui.sess != null && ui.sess.username != null) {
                nm = ui.sess.username;
                btn.change(nm);
            }

            if (ui.sess != null && (System.currentTimeMillis() - ui.sess.glob.lastAlert) < 5000) {
                btn.change(nm + " (A)");
                alerted = true;
            } else if (alerted) {
                btn.change(nm);
                alerted = false;
            }
        }

        private void click() {
            MainFrame.instance.p.setActiveUI(ui);
        }
    }

    private final Map<UI, UIDisplay> uimap;
    private final Grouping grp;
    private final Button add;

    public SessionDisplay() {
        super(Coord.z, "Session Display");
        uimap = new HashMap<>();
        grp = new GridGrouping("Sessions", new Coord(UI.scale(5), UI.scale(5)), 0,UI.scale(100), true);
        add = new Button(UI.scale(100), "New Session", this::newSession);
        add(grp);
        grp.add(add);
        MainFrame.instance.p.listenToSessions(this);
    }

    private void newSession() {
        MainFrame.instance.makeNewSession();
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
        return btn == 3;
    }

    @Override
    public void dispose() {
        super.dispose();
        MainFrame.instance.p.stopListeningToSessions(this);
    }

    @Override
    public void tick(double dt) {
        super.tick(dt);
        if(ui != null && ui.gui != null) {
            if (visible && !GlobalSettings.SHOWSESSIONS.get()) {
                hide();
            } else if (!visible && GlobalSettings.SHOWSESSIONS.get()) {
                show();
            }
        }
    }

    public List<UI> uis() {
        final List<UI> uis = new ArrayList<>();
        Widget wdg;
        for(wdg = grp.child; wdg != null; wdg = wdg.next) {
            if(wdg instanceof UIDisplay)
                uis.add(((UIDisplay)wdg).ui);
        }
        return uis;
    }

    public void init(Collection<UI> base) {
        for (final UI lui : base) {
            final UIDisplay display = new UIDisplay(lui);
            uimap.put(lui, display);
            grp.add(display);
        }
        add.raise();
        grp.pack();
        pack();
    }

    public void added(UI item) {
        final UIDisplay display = new UIDisplay(item);
        uimap.put(item, display);
        grp.add(display);
        add.raise();
        grp.pack();
        pack();
    }

    public void remove(UI item) {
        final UIDisplay display = uimap.remove(item);
        display.destroy();
        grp.pack();
        pack();
    }
}
