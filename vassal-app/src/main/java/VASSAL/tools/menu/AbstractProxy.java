
package VASSAL.tools.menu;

import java.awt.Container;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class AbstractProxy<T extends JComponent>
                implements ChildProxy<T> {
  protected final List<WeakReference<T>> peers =
    new ArrayList<>();

  protected final ReferenceQueue<T> queue = new ReferenceQueue<>();

  protected void processQueue() {
    Reference<? extends T> ref;
    while ((ref = queue.poll()) != null) {
      peers.remove(ref);
    }
  }

  protected void forEachPeer(Functor<T> functor) {
    processQueue();

    for (WeakReference<T> ref : peers) {
      final T peer = ref.get();
      if (peer != null) {
        functor.apply(peer);
      }
    }
  }

  protected ParentProxy parent;

  @Override
  public ParentProxy getParent() {
    return parent;
  }

  @Override
  public void setParent(ParentProxy parent) {
    this.parent = parent;

    if (parent == null) {
      forEachPeer(new Functor<>() {
        @Override
        public void apply(T peer) {
          final Container par = peer.getParent();
          if (par != null) {
            par.remove(peer);
          }
        }
      });
    }
  }

  @Override
  public abstract T createPeer();
}
