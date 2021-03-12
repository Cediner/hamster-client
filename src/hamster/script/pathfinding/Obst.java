package hamster.script.pathfinding;

import com.google.common.flogger.FluentLogger;
import hamster.gfx.ObstMesh;
import haven.*;

import java.awt.*;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.List;

/**
 * Loftar's `obst` layer.
 * <p>
 * There is one `obst` layer per bounding box that the object has.
 * <p>
 * Village Idol for instance has two `obst`s as it has two separate rectangular bounding boxes on each side of it.
 * <p>
 * Not everything has an `obst`
 * `obst` always start with:
 * <p>
 * 01
 * OR
 * 02 [STRING-OP]
 * <p>
 * Examples:
 * <p>
 * 02 | 00             | (no Op)
 * 02 | 62 75 6C 64 00 | (BUILD op)
 * <p>
 * PaliCP Example:
 * 01 01 04 FC B7 FC B7 FC 37 FC B7 FC 37 FC 37 FC B7 FC 37
 * 01 - Version
 * 01 - Type? 01 = polygon?
 * 04 - # of points
 * Data points: (float16, float16) offset from center of object multiplied by tilesz
 * (FC B7, FC B7)
 * (FC 37, FC B7)
 * (FC 37, FC 37)
 * (FC B7, FC 37)
 * <p>
 * <p>
 * Village Idol examples:
 * <p>
 * Obst 1 (Defines the two different boxes that you can't walk into
 * 02 00 02 03 03 67 B3 0A 3A BB 39 0A 3A 67 B3 03 3E 67 B3 03 BE BB 39 0A BA 67 B3 0A BA
 * 02 - Version
 * 00 - No Op
 * 02 - Type?
 * 03 - subtype? Triangle?
 * 03 - 3 coords per triangle?
 * (67 B3, 0A 3A) (BB 39, 0A 3A)
 * (67 B3, 03 3E) (67 B3, 03 BE)
 * (BB 39, 0A BA) (67 B3, 0A BA)
 * <p>
 * (67 B3) (OA 3A) (BB 39) (O3 BE) (03 3E) (OA BA)
 * <p>
 * Obst 2 (Defines the entire village idol box, like it used to be before muti-bounding boxes)
 * 02 62 75 69 6C 64 00 01 04 91 B9 53 BD 18 3C 53 BD 18 3C 53 3D 91 B9 53 3D
 * 02 - Version
 * 62 75 69 6C 64 00 - BUILD Op
 * 01 - Type? (Simple?)
 * 04 - # Data points?
 * (91 B9, 53 BD) (18 3C, 53 BD) (18 3C, 53 3D) (91 B9, 53 3D) - Data points gob.rc.add(float16, float16).mul(tilesz)?
 * <p>
 * Geyser:
 * 02 00 02 04 06 7B BE 0D BC 00 00 95 BF 03 00 95 3F 7B BE 0D 3C 7B BE 0D BC 00
 * 00 95 BF 04 3F 0D BC 04 3F 0D 3C 03 00 95 3F 7B BE 0D 3C
 * 02 - Version
 * 00 - No Op
 * 02 - Type??
 * 04 - subtype? ???
 * 06 - ???
 * (7B BE, OD BC) (00 00, 95 BF)
 * (03 00, 95 3F) (7B BE, OD 3C)
 * (7B BE, OD BC) (00 00, 95 BF)
 * (04 3F, OD BC) (04 3F, 0D 3C)
 * (03 00, 95 3F) (7B BE, OD 3C)
 * <p>
 * Headwater
 * 02 00 02 05 04 77 C3 4F C0 75 43 4F C0 75 43 4F 40 00 00 4F 44 D3 C2 0F 40 77 C3 4F C0 06 80 6E C3 75 43 4F C0 00 00 4F 44
 * 02 - Version
 * 00 - No Op
 * 02 - Type
 * 05 - Subtype
 * 04 - ???
 * (77 C3, 4F C0)
 * (75 43, 4F C0)
 * (75 43, 4F 40)
 * (00 00, 4F 44)
 * (D3 C2, 0F 40)
 * (77 C3, 4F C0)
 * (06 80, 6E C3)
 * (75 43, 4F C0)
 * (00 00, 4F 44)
 * <p>
 * <p>
 * Rock Crystal:
 * 02 00 01 06 5F BA 5B B7 00 00 42 BA 5F 3A 5B B7 41 39 1E 36 02 00 5B 3B DD B9 0F 36
 * 02 - Version
 * 00 - No Op
 * 01 - Type? (Simple?)
 * 06 - # of Data points
 * (5F BA, 5B B7)
 * (00 00, 42 BA)
 * (5F 3A, 5B B7)
 * (41 39, 1E 36)
 * (02 00, 5B 3B)
 * (DD B9, 0F 36)
 */
@Resource.LayerName("obst")
public class Obst extends Resource.Layer {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final Coord2d tilesz = new Coord2d(11, 11);
    private final Coord2d[][] shapes;

    /**
     * It is basically a list of 2D polygons.
     *  * The first byte is a uint8 version number, which can currently be 1 or 2.
     *  * If version >= 2, it is followed by a string ID. If version < 2, the ID is assumed to be the empty string.
     *  * Next is a uint8 indicating the number of polygons to follow.
     *  * For each polygon, there is then a uint8 indicating the number of vertices in that polygon.
     *  * Following the complete list of polygon sizes, is the complete list of vertices, such that the vertices of one
     *    polygon is directly followed by the vertices of the next. Each vertex is an (x, y) pair of half-floats.
     */
    public Obst(Resource res, final Message buf) {
        res.super();
        logger.atFine().log("Res: " + res.name);
        //The first byte is a uint8 version number, which can currently be 1 or 2.
        final var ver = buf.uint8();
        //If version >= 2, it is followed by a string ID. If version < 2, the ID is assumed to be the empty string.
        final var id = ver == 1 ? "" : buf.string();
        //Next is a uint8 indicating the number of polygons to follow.
        final var polygons = buf.uint8();
        final var verts_cnts = new int[polygons];
        for(var poly = 0; poly < polygons; ++poly) {
            //For each polygon, there is then a uint8 indicating the number of vertices in that polygon.
            verts_cnts[poly] = buf.uint8();
        }
        //Following the complete list of polygon sizes, is the complete list of vertices, such that the vertices of one
        // polygon is directly followed by the vertices of the next. Each vertex is an (x, y) pair of half-floats.
        shapes = new Coord2d[polygons][];
        for(var poly = 0; poly < polygons; ++poly) {
            shapes[poly] = new Coord2d[verts_cnts[poly]];
            for(var vert = 0; vert < verts_cnts[poly]; ++vert) {
                shapes[poly][vert] = buf.coordf16().mul(tilesz);
            }
        }
    }

    public int polygons() {
        return shapes.length;
    }

    public List<Coord2d> verts(final int polygon) {
        return List.of(shapes[polygon]);
    }

    public static ObstMesh makeMesh(final Coord2d[][] shapes, final Color col, final float h) {
        final int polygons = shapes.length;
        final float[] hiddencolor = Utils.c2fa(col);
        final FloatBuffer pa, na, cl;
        final ShortBuffer sa;

        {
            int verts = 0, inds = 0;
            for (Coord2d[] shape : shapes) {
                verts += shape.length;
                inds += (int) (Math.ceil(shape.length / 3.0));
            }
            pa = Utils.mkfbuf(verts * 3);
            na = Utils.mkfbuf(verts * 3);
            cl = Utils.mkfbuf(verts * 4);
            sa = Utils.mksbuf(inds * 3);
        }

        for (Coord2d[] shape : shapes) {
            for (final Coord2d off : shape) {
                pa.put((float) off.x).put((float) off.y).put(h);
                na.put((float) off.x).put((float) off.y).put(0f);
                cl.put(hiddencolor[0]).put(hiddencolor[1]).put(hiddencolor[2]).put(hiddencolor[3]);
            }
        }

        short voff = 0;
        for (int poly = 0; poly < polygons; ++poly) {
            final int vertsper = shapes[poly].length;
            for (int j = 0; j < (int) Math.ceil(vertsper / 3.0); ++j) {
                short s1 = (short) ((voff * j % vertsper) + (poly * vertsper));
                short s2 = (short) (((voff * j + 1) % vertsper) + (poly * vertsper));
                short s3 = (short) (((voff * j + 2) % vertsper) + (poly * vertsper));
                sa.put(s1).put(s2).put(s3);
                voff += 2;
            }
            voff = 0;
        }

        return new ObstMesh(new VertexBuf(new VertexBuf.VertexData(pa),
                new VertexBuf.NormalData(na),
                new VertexBuf.ColorData(cl)),
                sa);
    }

    public ObstMesh makeMesh(final Color col, final float h) {
        return makeMesh(shapes, col, h);
    }

    @Override
    public void init() {

    }
}
