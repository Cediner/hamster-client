package hamster.gob.attrs.mods;

import hamster.GlobalSettings;
import haven.GAttrib;
import haven.Gob;
import haven.UI;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;

public class PotStatus extends GAttrib implements Gob.SetupMod {
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

    private ContainerStatus.State state;

    public PotStatus(final Gob g) {
	super(g);
	this.state = ContainerStatus.State.INBETWEEN;
    }

    @Override
    public Pipe.Op gobstate() {
	if (GlobalSettings.COLORFULPOTS.get()) {
	    return state.mod;
	} else {
	    return null;
	}
    }

    @Override
    public void ctick(double dt) {
        final var ols = gob.ols.size();
        state = switch (ols) {
	    case 0 -> ContainerStatus.State.EMPTY;
	    case 1 -> ContainerStatus.State.INBETWEEN;
	    default -> ContainerStatus.State.FULL;
	};
    }

    @Override
    public String toString() {
	return "PotStatus(" +
		"state=" + state +
		')';
    }
}
