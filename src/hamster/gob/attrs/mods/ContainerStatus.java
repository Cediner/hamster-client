package hamster.gob.attrs.mods;

import hamster.GlobalSettings;
import haven.GAttrib;
import haven.Gob;
import haven.UI;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;
import java.util.Set;

/**
 * Color mod for containers to tell if they are empty, full, or inbetween
 */
public class ContainerStatus extends GAttrib implements Gob.SetupMod {
    enum State {
	EMPTY(new Color(0, 255, 0, 255)),
	FULL(new Color(255, 0, 0, 255)),
	INBETWEEN();

	public final BaseColor mod;
	State(final Color mod) {
	    this.mod = new BaseColor(mod);
	}

	State() {
	    this.mod = null;
	}
    }


    private State state;
    private final Set<Integer> empty;
    private final Set<Integer> full;

    public ContainerStatus(final Gob g, final Set<Integer> empty, final Set<Integer> full) {
	super(g);
	this.state = State.INBETWEEN;
	this.empty = empty;
	this.full = full;
    }

    @Override
    public Pipe.Op gobstate() {
	if (GlobalSettings.COLORFULCONTAINERS.get()) {
	    return state.mod;
	} else {
	    return null;
	}
    }

    @Override
    public void ctick(double dt) {
	final int sdt = gob.sdt();
	if(empty.contains(sdt))
	    state = State.EMPTY;
	else if(full.contains(sdt))
	    state = State.FULL;
	else
	    state = State.INBETWEEN;
    }

    @Override
    public String toString() {
	return "ContainerStatus(" +
		"state=" + state +
		')';
    }
}
