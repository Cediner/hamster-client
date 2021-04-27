package hamster.gfx;

import haven.*;
import haven.render.Pipe;
import haven.render.RenderTree;
import haven.render.Rendered;
import haven.render.States;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.HashMap;
import java.util.Map;

public class SimpleTextMesh extends FastMesh {
    private static final Map<String, SimpleTextMesh> meshes = new HashMap<>();
    private static final Font font = new Font("Sans", Font.BOLD, 64);
    private static final Text.Foundry fd = new Text.Foundry(font, 64);
    public static class TexIL extends TexL {
        final String name;
        public TexIL(final String name) {
            super(new Coord(64, 64));
            this.name = name;
	}

	@Override
	public BufferedImage fill() {
            final var img = TexI.mkbuf(this.sz());
            final var gfx = img.createGraphics();
            final var txt = fd.renderstroked(name, Color.WHITE, Color.BLACK);
            final var txtsz = Utils.imgsz(txt.img);
            final var start = this.sz().div(2).sub(txtsz.div(2));
            gfx.drawImage(txt.img, start.x, start.y, null);
            gfx.dispose();
            Debug.dumpimage(img, "test.png");
            return img;
	}
    }

    public final TexIL texture;

    public SimpleTextMesh(final VertexBuf buf, final ShortBuffer sa, final TexIL texture) {
	super(buf, sa);
	this.texture = texture;
    }

    public static SimpleTextMesh get(final String name) {
        if(meshes.containsKey(name))
            return meshes.get(name);
	final FloatBuffer pa = Utils.mkfbuf(12), tx = Utils.mkfbuf(8);
	final ShortBuffer sa = Utils.mksbuf(6);

	final Coord offset = MCache.tilesz2.div(2).mul(-1);
	final Coord sz = MCache.tilesz2.div(2);

	pa.put(offset.x).put(offset.y).put(3f); tx.put(0.0f).put(0.0f);
	pa.put(sz.x).put(offset.y).put(3f);     tx.put(0.0f).put(1.0f);
	pa.put(sz.x).put(sz.y).put(3f);         tx.put(1.0f).put(1.0f);
	pa.put(offset.x).put(sz.y).put(3f);     tx.put(1.0f).put(0.0f);

	sa.put((short)0).put((short)1).put((short)2);
	sa.put((short)2).put((short)3).put((short)0);

	meshes.put(name, new SimpleTextMesh(new VertexBuf(new VertexBuf.VertexData(pa),
		new VertexBuf.TexelData(tx)),
		sa,
		new TexIL(name)));

	return meshes.get(name);
    }

    @Override
    public void added(RenderTree.Slot slot) {
	super.added(slot);
	slot.ostate(Pipe.Op.compose(Rendered.last,
		new States.Facecull(States.Facecull.Mode.NONE)));
    }
}
