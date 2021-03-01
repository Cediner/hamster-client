package hamster.ui.core;

import haven.Coord;
import haven.UI;
import haven.Widget;

/**
 * Based class to handle everything to do with moving widgets around.
 */
public abstract class MovableWidget extends Widget {
    private final WdgLocationHelper loc;

    public MovableWidget(final Coord sz, final String name) {
        super(sz);
        loc = new WdgLocationHelper(name, () -> this, this::moveHit, this::relpos, this::setPosRel);
    }

    public MovableWidget(final String name) {
        super();
        loc = new WdgLocationHelper(name, () -> this, this::moveHit, this::relpos, this::setPosRel);
    }

    public MovableWidget() {
        this(null);
    }

    public MovableWidget(final UI ui, final Coord c, final Coord sz, final String name) {
        super(ui, c, sz);
        loc = new WdgLocationHelper(name, () -> this, this::moveHit, this::relpos, this::setPosRel);
    }

    public void toggleLock() {
        loc.toggleLock();
    }

    public boolean locked() {
        return loc.locked();
    }

    public boolean moving() {
        return loc.moving();
    }

    public void savePosition() {
        loc.savePosition();
    }

    @Override
    protected void added() {
        loc.added();
        super.added();
    }

    protected abstract boolean moveHit(final Coord c, final int btn);

    @Override
    public boolean mousedown(final Coord mc, final int button) {
        if (super.mousedown(mc, button)) {
            //Give preference to the Widget using this
            return true;
        } else  {
            return loc.mousedown(mc, button);
        }
    }

    @Override
    public boolean mouseup(final Coord mc, final int button) {
        if (!loc.mouseup(mc, button)) {
            return super.mouseup(mc, button);
        } else {
            return true;
        }
    }

    @Override
    public void mousemove(final Coord mc) {
        if (!loc.mousemove(mc)) {
            super.mousemove(mc);
        }
    }
}
