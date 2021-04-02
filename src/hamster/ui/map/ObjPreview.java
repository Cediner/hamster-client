package hamster.ui.map;

import haven.*;
import haven.Window;


/**
 * Self contain Obj previewer. Very similar to MapView, but a lot cut out.
 */
public class ObjPreview extends Window {
    private final Preview view;
    public ObjPreview(final Gob g) {
	super(Coord.z, "ObjPreview", "ObjPreview");
	view = add(new Preview(new PreviewGob(g)));
	pack();
    }

    @Override
    public void close() {
	ui.destroy(this);
	view.dispose();
    }
}
