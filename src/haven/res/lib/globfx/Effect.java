/* Preprocessed source code */
package haven.res.lib.globfx;

import haven.render.*;

public interface Effect extends RenderTree.Node {
    boolean tick(float dt);
    void gtick(Render out);
}

