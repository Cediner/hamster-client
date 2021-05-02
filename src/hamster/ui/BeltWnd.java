package hamster.ui;

import hamster.IndirSetting;
import hamster.KeyBind;
import hamster.data.BeltData;
import hamster.ui.core.MovableWidget;
import haven.*;

import java.awt.event.KeyEvent;
import java.util.List;
import java.util.Optional;

/**
 * Hotkeys are stored in dnyamic.sqlite only for custom items not stored on server-side
 * Table: beltslot
 * Columns:
 * character_id
 * slot_id
 * pagina_key
 */
public class BeltWnd extends MovableWidget {
    private static final Coord offc = new Coord(UI.scale(1), UI.scale(1));

    public class BeltBtn extends Widget implements DTarget, DropTarget {
        //Key to activate
        private final KeyBind key, key_ctrl;
        //Server slot id
        private int slot;
        //What to render, either a Pagina or a Resource, never both
        private MenuGrid.Pagina pag;
        private GameUI.BeltSlot bslot;
        //For dragging this btn if it has anything
        private boolean dragging = false;
        private UI.Grab dm = null;
        //tooltip
        private Tex tt;

        private BeltBtn(final KeyBind key, final KeyBind key_ctrl) {
            super(Inventory.invsq.sz());
            this.key = key;
            this.key_ctrl = key_ctrl;
            cancancel = false;
        }

        private Optional<Tex> img() {
            if (bslot != null) {
                try {
                    return Optional.of(bslot.res.get().layer(Resource.imgc).tex());
                } catch (Loading e) {
                    return Optional.empty();
                }
            } else if (pag != null) {
                try {
                    return Optional.of(pag.img.get());
                } catch (Loading l) {
                    return Optional.empty();
                }
            }
            return Optional.empty();
        }

        @Override
        public void draw(GOut g) {
            g.image(Inventory.invsq, Coord.z);
            //if we have something draw it
            img().ifPresent(tex -> {
                if (!dragging) {
                    g.image(tex, offc);
                } else {
                    ui.drawafter(g2 -> g2.image(tex, ui.mc.sub(tex.sz().div(2))));
                }
            });
            //always show our hotkey key
            final Coord ksz = FastText.size(key.bind.get().replace("NumPad-", "N"));
            final Coord tkeyc = sz.sub(ksz);
            g.chcolor(new java.awt.Color(128, 128, 128, 128));
            g.frect(tkeyc, sz);
            g.chcolor();
            FastText.aprint(g, sz, 1, 1, key.bind.get().replace("NumPad-", "N"));
        }

        private void reset() {
            data.remove(slot);
            bslot = null;
            pag = null;
            tt = null;
        }

        private void setSlot(final int slot) {
            this.slot = slot;
            tt = null;
            bslot = null;
            pag = null;
            if (ui.gui.belt[slot] != null) {
                bslot = ui.gui.belt[slot];
                data.remove(slot);
            } else {
                //Check for any pagina int his slot from db
                data.get(slot).ifPresent(key -> pag = ui.gui.menu.custompag.get(key));
            }
        }

        private void setPag(final MenuGrid.CustomPagina pag) {
            data.add(slot, pag.key);
            this.pag = pag;
            bslot = null;
            tt = null;
        }

        @Override
        public Object tooltip(Coord c, Widget prev) {
            if (tt != null) {
                //cached tt
                return tt;
            } else if (pag != null && pag.act() != null) {
                tt = new TexI(pag.button().rendertt(true));
                return tt;
            }
            return null; //no tt
        }

        public void use() {
            if (bslot != null) {
                ui.gui.wdgmsg("belt", slot, 1, ui.modflags());
            } else if (pag != null) {
                pag.use();
            }
        }

        @Override
        public boolean globtype(char key, KeyEvent ev) {
            final String bind = KeyBind.generateSequence(ev, ui);
            if(!bind.equals("")) {
                System.out.println(bind);
            }
            if(this.key.match(bind)) {
                use();
                return true;
            } else if(this.key_ctrl.match(bind)) {
                //key_ctrl is only for loftar stuff right now
                if(bslot != null) {
                    ui.gui.wdgmsg("belt", slot, 1, 2);
                }
                return true;
            } else {
                return super.globtype(key, ev);
            }
        }

        @Override
        public boolean mousedown(Coord c, int button) {
            if (((bslot != null && bslot.res.get() != null) || pag != null) && !(button == 3 && ui.modflags() == 0)) {
                dm = ui.grabmouse(this);
                return true;
            }
            return false;
        }

        @Override
        public boolean mouseup(Coord c, int button) {
            if (dm != null) {
                dm.remove();
                dm = null;
                if (dragging) {
                    if (bslot != null) {
                        if (ui.dropthing(ui.root, ui.mc, bslot.res.get())) {
                            reset();
                            //delete anything that might already belong to this slot
                            ui.gui.wdgmsg("setbelt", slot, 1);
                        }
                    } else {
                        if (ui.dropthing(ui.root, ui.mc, pag)) {
                            reset();
                        }
                    }
                    dragging = false;
                } else if (button == 1 && c.isect(Coord.z, sz)) {
                    use();
                } else if (button == 3 && ui.modshift && c.isect(Coord.z, sz) && !BeltWnd.this.locked) {
                    ui.gui.wdgmsg("setbelt", slot, 1);
                    reset();
                }
                return true;
            }
            return false;
        }

        @Override
        public void mousemove(Coord c) {
            super.mousemove(c);
            if (dm != null && !BeltWnd.this.locked) {
                dragging = true;
            }
        }

        @Override
        public boolean drop(Coord cc, Coord ul) {
            //This is for dropping inventory items on our mouse to a hotkey
            ui.gui.wdgmsg("setbelt", slot, 0);
            //reset for now and wait for server to send us uimsg if this was valid drop
            reset();
            return true;
        }

        @Override
        public boolean iteminteract(Coord cc, Coord ul) {
            return false;
        }

        @Override
        public boolean dropthing(Coord cc, Object thing) {
            //don't drop things on yourself..
            if (!dragging) {
                //Dropping "things" on us, mainly menugrid items
                if (thing instanceof Resource) {
                    //Normal server-side menu items
                    ui.gui.wdgmsg("setbelt", slot, ((Resource) thing).name);
                    //reset for now and wait for server to send us uimsg if this was valid drop
                    reset();
                    return true;
                } else if (thing instanceof MenuGrid.CustomPagina) {
                    //Not normal stuff.
                    setPag((MenuGrid.CustomPagina) thing);
                    //delete anything that might already belong to this slot
                    ui.gui.wdgmsg("setbelt", slot, 1);
                    return true;
                }
            }
            return false;
        }
    }

    public enum Style {
        HORIZONTAL, VERTICAL, GRID
    }

    //The actual belt..
    private Style style;
    private boolean locked;

    private final BeltBtn[] btns;
    private final IButton rotate, up, down, lock;

    //This is all about which slots we "own" and how we split it up between pages
    //each page is 10 slots
    private final int start_slot;
    private final int pagecount;
    //What page we're currently on
    private int page;

    //Belt data
    private final BeltData data;

    //Settings
    final IndirSetting<String> style_s;
    final IndirSetting<Boolean> visible_s;
    final IndirSetting<Integer> page_s;
    final IndirSetting<Boolean> locked_s;

    public BeltWnd(final String name, final BeltData data,
                   final IndirSetting<String> style, final IndirSetting<Boolean> vis,
                   final IndirSetting<Integer> page, final IndirSetting<Boolean> locked,
                   final List<KeyBind> kbs, final List<KeyBind> kbs_ctrl,
                   final int pages, final int start) {
        super(name);
        this.data = data;
        try {
            this.style = Style.valueOf(style.get());
        } catch (Exception e) {
            this.style = Style.HORIZONTAL;
        }
        this.visible = vis.get();
        this.page = page.get();
        this.locked  = locked.get();
        this.style_s = style;
        this.visible_s = vis;
        this.page_s = page;
        this.locked_s = locked;
        this.pagecount = pages;
        this.start_slot = start;
        this.btns = new BeltBtn[kbs.size()];

        rotate = add(new IButton("belt/rotate", "Rotate belt", () -> {
            switch (this.style) {
                case HORIZONTAL -> this.style = Style.VERTICAL;
                case VERTICAL -> this.style = Style.GRID;
                case GRID -> this.style = Style.HORIZONTAL;
            }
            style_s.set(this.style.toString());
            reposition();
        }));
        lock = add(new IButton("belt/lock", "Lock belt", this::toggleLock));
        if(this.locked) {
            lock.swap(IButton.Type.UP, IButton.Type.HOVER);
        }

        up = add(new IButton("belt/up", "Go up a page", () -> {
            this.page++;
            if (this.page >= pagecount)
                this.page = 0;
            upd_page();
        }));
        down = add(new IButton("belt/down", "Go down a page", () -> {
            this.page--;
            if (this.page < 0)
                this.page = pagecount - 1;
            upd_page();
        }));

        for(var i = 0;i < kbs.size(); ++i) {
            btns[i] = add(new BeltBtn(kbs.get(i), kbs_ctrl.get(i)), new Coord(0,  0));
        }
        reposition();
        pack();
    }

    public void toggleLock() {
        this.locked = !this.locked;
        locked_s.set(this.locked);
        lock.swap(IButton.Type.UP, IButton.Type.HOVER);
    }

    @Override
    public void draw(GOut g) {
        super.draw(g);
    }

    public boolean useSlot(final int slot) {
        if(visible) {
            btns[slot].use();
            return true;
        } else {
            return false;
        }
    }

    public boolean switchPage(final int page) {
        if(visible) {
            this.page = page;
            page_s.set(page);
            upd_page();
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected void added() {
        super.added();
        upd_page();
    }

    @Override
    public void dispose() {
        super.dispose();
    }

    @Override
    protected boolean moveHit(Coord c, int btn) {
        if(btn  == 3 && ui.modflags() == 0) {
            for (BeltBtn bbtn : btns) {
                if (c.isect(bbtn.c, bbtn.sz))
                    return true;
            }
        }
        return false;
    }

    private void upd_page() {
        page_s.set(page);
        int i = 0;
        for (BeltBtn btn : btns) {
            btn.setSlot(start_slot + i + (page * 10));
            i++;
        }
    }

    public void update(int slot) {
        int idx = (slot % 10);
        if (btns[idx].slot == slot)
            btns[idx].setSlot(slot);
    }

    private void reposition_grid() {
        final int spacer = UI.scale(2);
        int x = 0, y = 0;
        for (BeltBtn btn : btns) {
            btn.c = new Coord(x * (Inventory.invsq.sz().x + spacer),
                    y * (Inventory.invsq.sz().y + spacer));
            x++;
            if (x >= 3) {
                x = 0;
                y++;
            }
        }
        up.c = new Coord(x * (Inventory.invsq.sz().x + spacer),
                y * (Inventory.invsq.sz().y + spacer));
        down.c = new Coord(x * (Inventory.invsq.sz().x + spacer) + up.sz.x + spacer,
                y * (Inventory.invsq.sz().y + spacer));
        rotate.c = up.c.add(0, up.sz.y + spacer);
        lock.c = rotate.c.add(rotate.sz.x + spacer, 0);
    }

    private void reposition() {
        final int spacer = UI.scale(2);
        if (style == Style.GRID)
            reposition_grid();
        else {
            int n = 0;
            for (BeltBtn btn : btns) {
                switch (style) {
                    case VERTICAL -> {
                        btn.c = new Coord(0, n);
                        n += Inventory.invsq.sz().y + spacer;
                    }
                    case HORIZONTAL -> {
                        btn.c = new Coord(n, 0);
                        n += Inventory.invsq.sz().x + spacer;
                    }
                }
            }
            switch (style) {
                case VERTICAL -> {
                    up.c = new Coord(0, n);
                    down.c = new Coord(up.sz.x + spacer, n);
                    rotate.c = up.c.add(0, up.sz.y + spacer);
                    lock.c = rotate.c.add(rotate.sz.x + spacer, 0);
                }
                case HORIZONTAL -> {
                    up.c = new Coord(n, 0);
                    down.c = new Coord(n, up.sz.y + spacer);
                    rotate.c = up.c.add(up.sz.x + spacer, 0);
                    lock.c = rotate.c.add(0, rotate.sz.y + spacer);
                }
            }
        }
        pack();
    }

    @Override
    public void toggleVisibility() {
        super.toggleVisibility();
        visible_s.set(visible);
    }
}
