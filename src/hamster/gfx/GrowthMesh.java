package hamster.gfx;

import haven.FastMesh;
import haven.MapMesh;
import haven.Utils;
import haven.VertexBuf;
import haven.render.Pipe;
import haven.render.RenderTree;
import haven.render.States;
import haven.render.VertexColor;

import java.awt.*;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.HashMap;
import java.util.Map;

public class GrowthMesh extends FastMesh {
    private static final float[] outercol = Utils.c2fa(Color.RED);
    private static final float[] deadcol = Utils.c2fa(Color.GRAY);
    private static final float[] notready = Utils.c2fa(Color.PINK);
    private static final float[] minready = Utils.c2fa(Color.YELLOW);
    private static final float[] maxready = Utils.c2fa(Color.GREEN);
    private static final Map<String, GrowthMesh> insts = new HashMap<>();

    public GrowthMesh(VertexBuf buf, ShortBuffer sa) {
        super(buf, sa);
    }

    public void added(RenderTree.Slot slot) {
        slot.ostate(Pipe.Op.compose(MapMesh.postmap,
                new VertexColor(),
                new States.Facecull(States.Facecull.Mode.NONE)));
    }

    public static GrowthMesh mesh(final int stage, final int min, final int max) {
        final String key = String.format("{%s,%s,%s}", stage, min, max);
        synchronized (insts) {
            if(insts.containsKey(key))
                return insts.get(key);

            final float[] ocol = stage != -1 ? outercol : deadcol;
            final float[] col = stage >= max ? maxready : stage >= min ? minready : notready;
            final float outerrad = 3f; //Outer distance of the circle
            final float innerrad = 2f;  //Inner distance of the circle
            final double step = Math.PI / 64;
            final int verts = 4 * 64;
            final float h = 15f;


            //Position buffer, outer - inner - outer - ...
            //ends with an inner, start with an outer
            final FloatBuffer pa = Utils.mkfbuf(verts * 3 * 2);
            final FloatBuffer na = Utils.mkfbuf(verts * 3 * 2);    //normal, don't care about
            final FloatBuffer cl = Utils.mkfbuf(verts * 4 * 2);    //color buffer
            //How to draw it. each tri will be 3 points. 2 from the outer, 1 form the inner.
            final ShortBuffer sa = Utils.mksbuf(verts * 3 * 2);


            double rad = 0;
            for (int start = 0; start < verts; ++start) {
                final float angx = (float) (Math.cos(rad)), angy = (float) (Math.sin(rad));
                final float ox = angx * outerrad, oy = angy * outerrad;
                final float ix = angx * innerrad, iy = angy * innerrad;
                pa.put(ox).put(oy).put(h);
                na.put(ox).put(oy).put(h);
                cl.put(ocol[0]).put(ocol[1]).put(ocol[2]).put(ocol[3]);
                pa.put(ix).put(iy).put(h);
                na.put(ix).put(iy).put(h);
                cl.put(ocol[0]).put(ocol[1]).put(ocol[2]).put(ocol[3]);
                rad += step;
            }

            if(stage >= 0) {
                final int limit = Math.min((verts * stage / max), verts);
                for (int start = 0; start < limit; ++start) {
                    cl.put(start * 4, col[0]).put(start * 4 + 1, col[1]).put(start * 4 + 2, col[2]).put(start * 4 + 3, col[3]);
                }
            }

            for (int start = 0; start < verts; start += 2) {
                sa.put((short) start).put((short) (start + 1)).put((short) ((start + 2) % verts));
                sa.put((short) (start + 1)).put((short) ((start + 2) % verts)).put((short) ((start + 3) % verts));
            }

            final GrowthMesh mesh = new GrowthMesh(new VertexBuf(new VertexBuf.VertexData(pa),
                    new VertexBuf.NormalData(na),
                    new VertexBuf.ColorData(cl)),
                    sa);
            insts.put(key, mesh);
            return mesh;
        }
    }
}
