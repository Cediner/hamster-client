package hamster.ui;

import hamster.data.ShortenData;
import hamster.ui.core.layout.LinearGrouping;
import hamster.util.ObservableMapListener;
import haven.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ShortenWnd extends Window implements ObservableMapListener<String, Float> {
    private final List<String> shortenlst = new ArrayList<>();
    private final Map<String, Float> scalers = new HashMap<>();

    private final HSlider slider;

    public ShortenWnd() {
        super(Coord.z, "Shorten Gobs", "Shorten Gobs");
    	final var cont = add(new LinearGrouping(UI.scale(3), false));
    	final var lst = cont.add(new Listbox<String>(UI.scale(200), 15, UI.scale(20)) {
	    @Override
	    protected String listitem(int i) {
		return shortenlst.get(i);
	    }

	    @Override
	    protected int listitems() {
		return shortenlst.size();
	    }

	    @Override
	    protected void drawitem(GOut g, String item, int i) {
	        FastText.aprintf(g, g.sz().div(2), 0.5, 0.5, "%s - %d%%", item, (int)(scalers.get(item) * 100));
	    }

	    @Override
	    protected void itemclick(String item, int button) {
		super.itemclick(item, button);
		slider.val = (int)(scalers.get(item) * 100);
	    }
	});
    	cont.add(new Button("Remove Selected", () -> {
    	    if(lst.sel != null) {
		ShortenData.rem(lst.sel);
	    }
	}));
    	slider = cont.add(new HSlider(UI.scale(200), 1, 100, 1) {
	    @Override
	    public void changed() {
		super.changed();
		if(lst.sel != null) {
		    ShortenData.add(lst.sel, val / 100f);
		}
	    }
	});
    	cont.pack();
    	pack();
	hide();
	ShortenData.listen(this);
    }

    @Override
    public void close() {
	hide();
    }

    @Override
    protected void removed() {
	super.removed();
	ShortenData.unlisten(this);
    }

    @Override
    public void init(Map<String, Float> base) {
        for(final var key : base.keySet()) {
            scalers.put(key, base.get(key));
            shortenlst.add(key);
	}
    }

    @Override
    public void put(String key, Float val) {
	scalers.put(key, val);
	if(!shortenlst.contains(key))
	    shortenlst.add(key);
    }

    @Override
    public void remove(String key) {
	scalers.remove(key);
	shortenlst.remove(key);
    }
}
