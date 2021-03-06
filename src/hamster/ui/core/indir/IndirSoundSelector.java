package hamster.ui.core.indir;

import hamster.IndirSetting;
import hamster.gob.Alerted;
import haven.*;

public class IndirSoundSelector extends Window {
    private final Listbox<Resource.Named> sounds;
    private final IndirSetting<String> res;

    public IndirSoundSelector(final String purpose, final IndirSetting<String> res) {
	super(Coord.z, "Sound Selector", "Sound Selector");
	this.res = res;

	Coord c = new Coord(0, 0);
	c.y += add(new Label("Select sound for " + purpose)).sz.y;
	final Button select = new Button(50, "Select", this::select);
	final Button preview = new Button(50, "Preview", this::preview);
	sounds = add(new Listbox<Resource.Named>(200, 20, 20) {
	    @Override
	    protected Resource.Named listitem(int i) {
		return Alerted.sounds.get(i);
	    }

	    @Override
	    protected int listitems() {
		return Alerted.sounds.size();
	    }

	    @Override
	    protected void drawitem(GOut g, Resource.Named item, int i) {
		g.text(item.name, new Coord(5, 1));
	    }
	}, c.copy());
	for(final var sound : Alerted.sounds) {
	    if(sound.name.equals(res.get())) {
	        sounds.sel = sound;
	        sounds.showsel();
	        break;
	    }
	}
	add(select, c.add(sounds.sz.x + 5, sounds.sz.y / 2 - select.sz.y));
	add(preview, c.add(sounds.sz.x + 5, sounds.sz.y / 2 + select.sz.y));
	pack();
    }

    @Override
    public void close() {
	ui.destroy(this);
    }

    private void select() {
	if (sounds.sel != null) {
	    res.set(sounds.sel.name);
	    ui.destroy(this);
	}
    }

    private void preview() {
	if (sounds.sel != null) {
	    ui.sfx(Resource.remote().load(sounds.sel.name));
	}
    }
}
